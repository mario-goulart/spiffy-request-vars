#!/usr/bin/csi -script

(use test http-client spiffy-request-vars)

(define (get path/vars)
  (let ((val (with-input-from-request
              (string-append "http://localhost:8080" path/vars)
              #f read)))
    (close-all-connections!)
    val))

(test-begin "spiffy-request-vars")

;;; Strings
(test #f (get "/as-string"))
(test #f (get "/wrv-as-string"))

(test "a" (get "/as-string?var=a"))
(test "a" (get "/wrv-as-string?var=a"))

(test "a" (get "/as-string?var=a;var=b"))
(test "a" (get "/wrv-as-string?var=a;var=b"))

(test "1" (get "/as-string?var=1"))
(test "1" (get "/wrv-as-string?var=1"))


;;; Numbers
(test #f (get "/as-number"))
(test #f (get "/wrv-as-number"))

(test 3 (get "/as-number?var=3"))
(test 3 (get "/wrv-as-number?var=3"))

(test #f (get "/as-number?foo=3"))
(test #f (get "/wrv-as-number?foo=3"))

(test #f (get "/as-number?var=a"))
(test #f (get "/wrv-as-number?var=a"))


;;; Booleans
(test #f (get "/as-boolean"))
(test #f (get "/wrv-as-boolean"))

(test #t (get "/as-boolean?var=yes"))
(test #t (get "/wrv-as-boolean?var=yes"))

(test #t (get "/as-boolean?var=Yes"))
(test #t (get "/wrv-as-boolean?var=Yes"))

(test #t (get "/as-boolean?var=1"))
(test #t (get "/wrv-as-boolean?var=1"))

(test #f (get "/as-boolean?var=0"))
(test #f (get "/wrv-as-boolean?var=0"))


;;; List
(test #f (get "/as-list"))
(test #f (get "/wrv-as-list"))

(test '("a" "b") (get "/as-list?var=a;var=b"))
(test '("a" "b") (get "/wrv-as-list?var=a;var=b"))

(test '("a" "b") (get "/as-list?var=a;var=b;varb=4"))
(test '("a" "b") (get "/wrv-as-list?var=a;var=b;varb=4"))

(test '("c") (get "/as-list?var=c"))
(test '("c") (get "/wrv-as-list?var=c"))


;;; Vectors
(test #f (get "/as-vector"))
(test #f (get "/wrv-as-vector"))

(test '#("a" "b") (get "/as-vector?var.0=a;var.1=b"))
(test '#("a" "b") (get "/wrv-as-vector?var.0=a;var.1=b"))

(test 3 (vector-length (get "/as-vector?var.0=a;var.1=b;var.2=4;varc=4")))
(test 3 (vector-length (get "/wrv-as-vector?var.0=a;var.1=b;var.2=4;varc=4")))

(test #f (get "/as-vector?var=c"))
(test #f (get "/wrv-as-vector?var=c"))

(test '#("a" "b") (get "/as-vector?var.0=a;var.1=b;var3=c"))
(test '#("a" "b") (get "/wrv-as-vector?var.0=a;var.1=b;var3=c"))


;;; Alists
(test #f (get "/as-list"))
(test #f (get "/wrv-as-alist"))

(test '((A . "a") (B . "b")) (get "/as-alist?var.A=a;var.B=b"))
(test '((A . "a") (B . "b")) (get "/wrv-as-alist?var.A=a;var.B=b"))

(test #f (get "/as-alist?var=c"))
(test #f (get "/wrv-as-alist?var=c"))


;;; Hash-tables
(test #f (get "/as-hash-table"))
(test #f (get "/wrv-as-hash-table"))

(test '((B . "b") (A . "a")) (get "/as-hash-table?var.A=a;var.B=b"))
(test '((B . "b") (A . "a")) (get "/wrv-as-hash-table?var.A=a;var.B=b"))

(test #f (get "/as-hash-table?var=c"))
(test #f (get "/wrv-as-hash-table?var=c"))


(test-end "spiffy-request-vars")
