(module spiffy-request-vars
  (request-vars with-request-vars with-request-vars*

   ;; Parameters
   true-boolean-values

   ;; Converters
   as-string as-symbol as-boolean as-list as-number as-alist as-vector as-hash-table nonempty
   )

(import chicken scheme extras ports files data-structures srfi-69)
(use srfi-1 srfi-13 intarweb uri-common spiffy)

(define true-boolean-values
  ;; A list of strings to be considered `#t' for request variables
  ;; when `as-boolean' is used as converter.
  (make-parameter
   '("y" "yes" "1" "on" "true")))

(define (req-vars/vals var vars/vals)
  (if vars/vals
      (let loop ((vars/vals vars/vals))
        (if (null? vars/vals)
            '()
            (let ((var/val (car vars/vals)))
              (if (eq? var (car var/val))
                  (cons (cdr var/val) (loop (cdr vars/vals)))
                  (loop (cdr vars/vals))))))
      '()))

(define (as-string var vars/vals)
  (alist-ref var vars/vals))

(define (as-symbol var vals)
  (and-let* ((val (alist-ref var vals)))
    (string->symbol val)))

(define (as-boolean var vals)
  (and-let* ((val (alist-ref var vals)))
    (or (eqv? #t val)
	(not (not (member val (true-boolean-values) string-ci=?))))))

(define (as-list var vals)
  (let ((vals (req-vars/vals var vals)))
    (if (null? vals)
        #f
        vals)))

(define (as-number var vals)
  (and-let* ((val (alist-ref var vals)))
    (string->number val)))

(define (build-alist var vars/vals converter)
  (let* ((var (->string var))
         (alist
          (let loop ((vars/vals vars/vals))
            (if (null? vars/vals)
                '()
                (let* ((var/val (car vars/vals))
                       (current-var (symbol->string (car var/val)))
                       (tokens (string-split current-var ".")))
                  (if (and (not (null? (cdr tokens)))
                           (equal? var (car tokens)))
                      (let ((idx (converter (cadr tokens))))
                        (cons (cons idx
                                    (cdr var/val))
                              (loop (cdr vars/vals))))
                      (loop (cdr vars/vals))))))))
    (if (null? alist)
        #f
        alist)))

(define (as-alist var vars/vals)
  (build-alist var vars/vals string->symbol))

(define (as-vector var vars/vals)
  (and-let* ((alist (build-alist var vars/vals string->number)))
    (let* ((max-idx (car (sort (delete #f (map car alist)) >)))
           (vec (make-vector (add1 max-idx))))
      (for-each (lambda (idx/val)
                  (vector-set! vec (car idx/val) (cdr idx/val)))
                alist)
      vec)))

(define (as-hash-table var vars/vals)
  (and-let* ((alist (build-alist var vars/vals string->symbol)))
    (alist->hash-table alist)))

(define (nonempty converter)
  (lambda (var vars/vals)
    (and-let* ((val (alist-ref var vars/vals)))
      (and (not (equal? val "")) (converter var vars/vals)))))

(define (request-vars #!key (source 'both) max-content-length)

  (let* ((content-matters? (not (memq (request-method (current-request)) '(GET HEAD))))
         (query-string-vars
          (and (memq source '(both query-string))
               (uri-query (request-uri (current-request)))))
         (request-body
          (and (memq source '(both request-body))
               content-matters? ;; don't bother reading the contents when method is either GET or HEAD
               (let* ((headers (request-headers (current-request)))
                      (content-length (header-value 'content-length headers)))
                 (when (and max-content-length
                            content-length ;; sometimes this header does not exist
                            (> content-length max-content-length))
                   (error 'request-vars "content-length exceeds the provided max-content-length."))
                 (if (and content-length (zero? content-length))
                     #f
                     (let ((body (read-string (or max-content-length content-length)
                                              (request-port (current-request)))))
                       (case (header-value 'content-type headers)
                         ((application/x-www-form-urlencoded) (form-urldecode body))
                         (else #f) ;; not supported
                         )))))))

    (lambda (var #!optional default/converter)
      (let* ((var (if (string? var)
                      (string->symbol var)
                      var))
             (vals (case source
                     ((both) (append (or request-body '())
                                     (or query-string-vars '())))
                     ((request-body) request-body)
                     ((query-string) query-string-vars)
                     (else (error 'request-vars (conc "Unkown source: " source))))))
        (if (procedure? default/converter)
            (default/converter var vals)
            (let ((vals (req-vars/vals var vals)))
              (if (null? vals)
                  default/converter
                  (car vals))))))))

(define-syntax with-request-vars*
  (syntax-rules ()
    ((_ $ () form . forms)
     (begin form . forms))

    ((_ $ ((var converter) . more-bindings) forms ...)
     (let* (($$ $)
            (var ($$ (quote var) converter)))
       (with-request-vars* $$ more-bindings forms ...)))

    ((_ $ (var . more-bindings) forms ...)
      (let* (($$ $)
             (var ($$ (quote var))))
        (with-request-vars* $$ more-bindings forms ...)))))

(define-syntax with-request-vars
   (syntax-rules ()
     ((_ bindings forms ...)
      (with-request-vars* (request-vars) bindings forms ...))
     ((_ $ bindings forms ...)
      (with-request-vars* $ bindings forms ...))))

) ; end module
