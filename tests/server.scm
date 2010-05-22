#!/usr/bin/awful

(use srfi-69 awful spiffy-request-vars spiffy intarweb uri-common html-tags)

(page-template (lambda (contents . more) contents))

(debug-log (current-error-port))

(page-exception-message
 (lambda (exn)
   (<pre> convert-to-entities?: #t
          (with-output-to-string
            (lambda ()
              (print-call-chain)
              (print-error-message exn))))))

(define (show var)
  (with-output-to-string (cut write var)))

(define-page "as-string" (lambda () (show ($ 'var))))

(define-page "as-number" (lambda () (show ($ 'var as-number))))

(define-page "as-boolean" (lambda () (show ($ 'var as-boolean))))

(define-page "as-list" (lambda () (show ($ 'var as-list))))

(define-page "as-vector" (lambda () (show ($ 'var as-vector))))

(define-page "as-alist" (lambda () (show ($ 'var as-alist))))

(define-page "as-hash-table"
  (lambda ()
    (and-let* ((var ($ 'var as-hash-table)))
      (show (hash-table->alist var)))))
