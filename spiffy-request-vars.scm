(module spiffy-request-vars
  (request-vars with-request-vars with-request-vars*

   ;; Parameters
   true-boolean-values

   ;; Converters
   as-boolean as-list as-number
   )

(import chicken scheme extras ports files data-structures)
(use srfi-1 srfi-13 intarweb uri-common spiffy)

(define true-boolean-values
  ;; A list of strings to be considered `#t' for request variables
  ;; when `as-boolean' is used as converter.
  (make-parameter
   '("y" "yes" "1" "on")))

(define (req-vars/vals var vars/vals)
  (and vars/vals
       (let loop ((vars/vals vars/vals))
         (if (null? vars/vals)
             '()
             (let ((var/val (car vars/vals)))
               (if (eq? var (car var/val))
                   (cons (cdr var/val) (loop (cdr vars/vals)))
                   (loop (cdr vars/vals))))))))

(define (as-boolean var vals)
  (and-let* ((val (alist-ref var vals)))
    (not (not (member val (true-boolean-values) string-ci=?)))))

(define (as-list var vals)
  (let ((vals (req-vars/vals var vals)))
    (if (null? vals)
        #f
        vals)))

(define (as-number var vals)
  (and-let* ((val (alist-ref var vals)))
    (string->number val)))

(define (request-vars #!key (source 'both) max-content-length)

  (let* ((content-matters? (not (memq (request-method (current-request)) '(GET HEAD))))
         (get-vars (and (memq source '(both query-string))
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
                         (else body))))))))

    (lambda (var #!optional default/converter)
      (let* ((var (if (string? var)
                      (string->symbol var)
                      var))
             (vals (or request-body get-vars)))

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
     (let* ((var ($ (quote var) converter)))
       (with-request-vars* $ more-bindings forms ...)))

    ((_ $ (var . more-bindings) forms ...)
      (let* ((var ($ (quote var))))
        (with-request-vars* $ more-bindings forms ...)))))

(define-syntax with-request-vars
   (syntax-rules ()
     ((_ bindings forms ...)
      (with-request-vars* (request-vars) bindings forms ...))
     ((_ $ bindings forms ...)
      (with-request-vars* $ bindings forms ...))))

) ; end module
