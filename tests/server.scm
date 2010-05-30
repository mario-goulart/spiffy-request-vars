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


;;; with-request-vars
(define-page "wrv-as-string"
  (lambda ()
    (with-request-vars* $ (var)
        (show var))))

(define-page "wrv-as-number"
  (lambda ()
    (with-request-vars ((var as-number))
        (show var))))

(define-page "wrv-as-boolean"
  (lambda ()
    (with-request-vars ((var as-boolean))
        (show var))))

(define-page "wrv-as-list"
  (lambda ()
    (with-request-vars ((var as-list))
        (show var))))

(define-page "wrv-as-vector"
  (lambda ()
    (with-request-vars ((var as-vector))
        (show var))))

(define-page "wrv-as-alist"
  (lambda ()
    (with-request-vars ((var as-alist))
        (show var))))

(define-page "wrv-as-hash-table"
  (lambda ()
    (with-request-vars ((var as-hash-table))
        (and var (show (hash-table->alist var))))))


;;; test1
(define-page "test1"
  (lambda ()
    (with-request-vars (foo (bar as-list) (baz 5))
        (show (list foo bar baz)))))


;;; test2
(define-page "test2"
  (lambda ()
    (with-request-vars* $ (foo (bar as-list) (baz 5))
      (show (list foo bar baz)))))


;;; test3
(define-page "test3"
  (lambda ()
    (with-request-vars ((foo as-alist) (bar as-number) (baz as-vector) (bool as-boolean))
      (show (list foo bar baz bool)))))


;;; test4
(define-page "test4"
  (lambda ()
    (with-request-vars* $ ((foo as-alist) (bar as-number) (baz as-vector) (bool as-boolean))
      (show (list foo bar baz bool)))))
