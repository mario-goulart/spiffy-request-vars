#!/usr/bin/csi -script

(use test http-client)

(define (get path/vars)
  (with-input-from-request
   (string-append "http://localhost:8080" path/vars)
   #f read))

;;; Strings
(test #f (get "/as-string"))

(test "a" (get "/as-string?var=a"))

(test "a" (get "/as-string?var=a;var=b"))


;;; Numbers
(test 3 (get "/as-number?var=3"))

(test #f (get "/as-number?foo=3"))


;;; Booleans
(test #f (get "/as-boolean"))

(test #t (get "/as-boolean?var=yes"))


;;; List
(test '("a" "b")
      (get "/as-list?var=a;var=b"))

(test '("c")
      (get "/as-list?var=c"))

(test #f
      (get "/as-list"))

;;; Vectors
(test '#("a" "b")
      (get "/as-vector?var.0=a;var.1=b"))

(test #f
      (get "/as-vector?var=c"))

(test '#("a" "b")
      (get "/as-vector?var.0=a;var.1=b"))

;;; Alists
(test '((A . "a") (B . "b"))
      (get "/as-alist?var.A=a;var.B=b"))

(test #f
      (get "/as-alist?var=c"))


;;; Hash-tables
(test '((B . "b") (A . "a"))
      (get "/as-hash-table?var.A=a;var.B=b"))

(test #f
      (get "/as-hash-table?var=c"))
