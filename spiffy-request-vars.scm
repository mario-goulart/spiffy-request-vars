(module spiffy-request-vars (request-vars with-request-vars with-request-vars*)

(import chicken scheme extras ports files data-structures)
(use intarweb uri-common spiffy)

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
                 (let ((body (read-string (or max-content-length content-length)
                                          (request-port (current-request)))))
                   (case (header-value 'content-type headers)
                     ((application/x-www-form-urlencoded) (form-urldecode body))
                     (else body)))))))

    (lambda (var #!optional default (converter identity))
      (let* ((var (if (string? var)
                      (string->symbol var)
                      var))
             (val (or (and get-vars (alist-ref var get-vars))
                      (and request-body (alist-ref var request-body)))))
        (if val
            (converter val)
            default)))))

(define-syntax with-request-vars
  (syntax-rules ()
    ((_ (var1 ...) e1 ...)
     (let* ((% (request-vars))
            (var1 (% (quote var1)))
            ...)
       e1 ...))
    ((_ % (var1 ...) e1 ...)
     (let* (($ %)
            (var1 ($ (quote var1)))
            ...)
       e1 ...))))

(define-syntax with-request-vars*
  (syntax-rules ()
    ((_ % (var1 ...) e1 ...)
     (let* (($ %)
            (var1 ($ (quote var1)))
            ...)
       e1 ...))))

) ; end module
