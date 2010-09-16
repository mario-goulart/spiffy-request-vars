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
(test "" (get "/wrv-as-string?var="))

(test "a" (get "/as-string?var=a"))
(test "a" (get "/wrv-as-string?var=a"))

(test "a" (get "/as-string?var=a;var=b"))
(test "a" (get "/wrv-as-string?var=a;var=b"))

(test "1" (get "/as-string?var=1"))
(test "1" (get "/wrv-as-string?var=1"))


;;; Numbers
(test #f (get "/as-number"))
(test #f (get "/wrv-as-number"))
(test #f (get "/wrv-as-number?var="))

(test 3 (get "/as-number?var=3"))
(test 3 (get "/wrv-as-number?var=3"))

(test #f (get "/as-number?foo=3"))
(test #f (get "/wrv-as-number?foo=3"))

(test #f (get "/as-number?var=a"))
(test #f (get "/wrv-as-number?var=a"))


;;; Booleans
(test #f (get "/as-boolean"))
(test #f (get "/wrv-as-boolean"))
(test #f (get "/wrv-as-boolean?var="))

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
(test '("") (get "/wrv-as-list?var="))

(test '("a" "b") (get "/as-list?var=a;var=b"))
(test '("a" "b") (get "/wrv-as-list?var=a;var=b"))

(test '("a" "b") (get "/as-list?var=a;var=b;varb=4"))
(test '("a" "b") (get "/wrv-as-list?var=a;var=b;varb=4"))

(test '("c") (get "/as-list?var=c"))
(test '("c") (get "/wrv-as-list?var=c"))


;;; Vectors
(test #f (get "/as-vector"))
(test #f (get "/wrv-as-vector"))
(test '#("") (get "/wrv-as-vector?var.0="))

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
(test '((A . "")) (get "/wrv-as-alist?var.A="))

(test '((A . "a") (B . "b")) (get "/as-alist?var.A=a;var.B=b"))
(test '((A . "a") (B . "b")) (get "/wrv-as-alist?var.A=a;var.B=b"))

(test #f (get "/as-alist?var=c"))
(test #f (get "/wrv-as-alist?var=c"))


;;; Hash-tables
(test #f (get "/as-hash-table"))
(test #f (get "/wrv-as-hash-table"))

(test '((B . "b") (A . "a")) (get "/as-hash-table?var.A=a;var.B=b"))
(test '((B . "b") (A . "a")) (get "/wrv-as-hash-table?var.A=a;var.B=b"))
(test '((A . "")) (get "/wrv-as-hash-table?var.A="))

(test #f (get "/as-hash-table?var=c"))
(test #f (get "/wrv-as-hash-table?var=c"))


;;; test1
(test '(#f #f 5)             (get "/test1"))
(test '("10" #f 5)           (get "/test1?foo=10"))
(test '("10" ("1") 5)        (get "/test1?foo=10&bar=1"))
(test '("10" ("1" "2") 5)    (get "/test1?foo=10&bar=1&bar=2"))
(test '("10" ("1" "2") "-8") (get "/test1?foo=10&bar=1&bar=2&baz=-8"))

;;; test2
(test '(#f #f 5)             (get "/test2"))
(test '("10" #f 5)           (get "/test2?foo=10"))
(test '("10" ("1") 5)        (get "/test2?foo=10&bar=1"))
(test '("10" ("1" "2") 5)    (get "/test2?foo=10&bar=1&bar=2"))
(test '("10" ("1" "2") "-8") (get "/test2?foo=10&bar=1&bar=2&baz=-8"))


;;; test3
(test '(#f #f #f #f) (get "/test3"))

(test '(((A . "0") (B . "1")) 0 #("a" "b") #t)
      (get "/test3?foo.A=0&foo.B=1&bar=0&baz.0=a&baz.1=b&bool=yes"))

(test '(#f #f #f #f)
      (get "/test3?foo=0&bar=a&baz=0&bool=3"))


;;; test4
(test '(#f #f #f #f) (get "/test4"))

(test '(((A . "0") (B . "1")) 0 #("a" "b") #t)
      (get "/test4?foo.A=0&foo.B=1&bar=0&baz.0=a&baz.1=b&bool=yes"))

(test '(#f #f #f #f)
      (get "/test4?foo=0&bar=a&baz=0&bool=3"))


;;; test5
(test '("" "" "")
      (get "/test5?foo=&bar=&baz="))

(test '("" "2" "")
      (get "/test5?foo=&bar=2&baz="))

(test '("1" "2" "3")
      (get "/test5?foo=1&bar=2&baz=3"))


;;; test6
(test '("" "" "")
      (get "/test6?foo=&bar=&baz="))

(test '("" "2" "")
      (get "/test6?foo=&bar=2&baz="))

(test '("1" "2" "3")
      (get "/test6?foo=1&bar=2&baz=3"))


(test-end "spiffy-request-vars")
