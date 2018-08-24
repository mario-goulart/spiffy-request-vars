(define *resources* '())

(define http-request-variables (make-parameter #f))

(define (run-resource proc path)
  (http-request-variables #f)
  (let ((out (->string (proc))))
    (with-headers
     `((content-type text/html)
       (content-length ,(string-length out)))
     (lambda ()
       (write-logged-response)
       (display out (response-port (current-response)))))))

(handle-directory
 (let ((old-handler (handle-directory)))
   (lambda (path)
     (let ((method (request-method (current-request))))
       (cond ((alist-ref (cons path method) *requests* equal?)
              => (lambda (proc)
                   (run-resource proc path)))
             (else (old-handler path)))))))

(handle-not-found
 (let ((old-handler (handle-not-found)))
   (lambda (_)
     (let* ((path-list (uri-path (request-uri (current-request))))
            (method (request-method (current-request)))
            (path (string-intersperse (cdr path-list) "/"))
            (proc (alist-ref (cons path method) *resources* equal?)))
       (if proc
           (run-resource proc path)
           (old-handler path))))))

(define ($ var #!optional default/converter)
  (unless (http-request-variables)
    (http-request-variables (request-vars)))
  ((http-request-variables) var default/converter))

(define (define-page path proc #!key (method 'GET))
  (set! *resources*
    (append (list
             (cons (cons path method) proc)
             (cons (cons (string-append "compound/" path) method)
                   (lambda ()
                     (parameterize ((compound-variable-separator "_"))
                       (proc)))))
            *resources*)))
