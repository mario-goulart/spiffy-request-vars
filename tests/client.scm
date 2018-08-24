(import scheme)
(cond-expand
  (chicken-4
   (use test http-client spiffy-request-vars intarweb uri-common srfi-1))
  (chicken-5
   (import (chicken format)
           (chicken pathname))
   (import http-client intarweb spiffy-request-vars srfi-1 test uri-common)
   (define close-all-connections! close-idle-connections!))
  (else
   (error "Unsupported CHICKEN version.")))

(define server-uri (sprintf "http://localhost:~a" (server-port)))

(define (get path/vars)
  (let ((val (with-input-from-request
              (string-append server-uri path/vars)
              #f read)))
    (close-all-connections!)
    val))

(define (post path vars/vals #!optional (query-string-vars/vals '()))
  (let* ((vars/vals (and (not (null? vars/vals)) (form-urlencode vars/vals)))
         (resp (with-input-from-request
                (make-request
                 uri: (uri-reference
                       (make-pathname server-uri
                                      (string-append
                                       path
                                       (if (null? query-string-vars/vals)
                                           ""
                                           (string-append
                                            "?"
                                            (form-urlencode query-string-vars/vals))))))
                 headers: (headers `((content-length ,(string-length vars/vals))
                                     (content-type application/x-www-form-urlencoded)))
                 method: 'POST)
                (lambda ()
                  (print (or vars/vals "")))
                read)))
    (close-all-connections!)
    resp))

(test-begin "spiffy-request-vars")

;;; Strings
(test #f (get "/as-string"))
(test #f (get "/wrv-as-string"))
(test "" (get "/wrv-as-string?var="))
(test #f (get "/as-nonempty-string?var="))

(test "a" (get "/as-string?var=a"))
(test "a" (get "/wrv-as-string?var=a"))

(test "a" (get "/as-string?var=a;var=b"))
(test "a" (get "/wrv-as-string?var=a;var=b"))

(test "1" (get "/as-string?var=1"))
(test "1" (get "/wrv-as-string?var=1"))


;;; Symbols
(test #f (get "/as-symbol"))
(test #f (get "/wrv-as-symbol"))
(test '|| (get "/wrv-as-symbol?var="))
(test #f (get "/as-nonempty-symbol?var="))

(test 'a (get "/as-symbol?var=a"))
(test 'a (get "/wrv-as-symbol?var=a"))

(test 'a (get "/as-symbol?var=a;var=b"))
(test 'a (get "/wrv-as-symbol?var=a;var=b"))


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
(test #f (get "/as-nonempty-number?var="))
(test 3 (get "/as-nonempty-number?var=3"))


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

(test #t (let ((res (get "/as-hash-table?var.A=a;var.B=b")))
           (lset= equal? res '((A . "a") (B . "b")))))

(test #t (let ((res (get "/wrv-as-hash-table?var.A=a;var.B=b")))
           (lset= equal? res '((A . "a") (B . "b")))))

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


;;; test7
(test '("1" "2" "3" #f) (post "/test7" '((foo . 1) (bar . 2) (baz . 3)) '((blah . 4))))


;;; test8
(test '(#f #f #f "4") (post "/test8" '((foo . 1) (bar . 2) (baz . 3)) '((blah . 4))))


;;; test9
(test '("1" "2" "3" #f) (post "/test9" '((foo . 1) (bar . 2) (baz . 3)) '((blah . 4))))


;;; test10
(test '("1" "2" "3" #f) (post "/test10" '((foo . 1) (bar . 2) (baz . 3)) '((blah . 4))))


;;; test11
(test '("1" "2" "3" "4") (post "/test11" '((foo . 1) (bar . 2) (baz . 3)) '((blah . 4))))


;;;
;;; Compound variable names
;;;

;; Vectors
(test #f (get "/compound/as-vector"))
(test #f (get "/compound/wrv-as-vector"))
(test '#("") (get "/compound/wrv-as-vector?var_0="))

(test '#("a" "b") (get "/compound/as-vector?var_0=a;var_1=b"))
(test '#("a" "b") (get "/compound/wrv-as-vector?var_0=a;var_1=b"))

(test 3 (vector-length (get "/compound/as-vector?var_0=a;var_1=b;var_2=4;varc=4")))
(test 3 (vector-length (get "/compound/wrv-as-vector?var_0=a;var_1=b;var_2=4;varc=4")))

(test #f (get "/compound/as-vector?var=c"))
(test #f (get "/compound/wrv-as-vector?var=c"))

(test '#("a" "b") (get "/compound/as-vector?var_0=a;var_1=b;var3=c"))
(test '#("a" "b") (get "/compound/wrv-as-vector?var_0=a;var_1=b;var3=c"))


;; Alists
(test #f (get "/compound/as-list"))
(test #f (get "/compound/wrv-as-alist"))
(test '((A . "")) (get "/compound/wrv-as-alist?var_A="))

(test '((A . "a") (B . "b")) (get "/compound/as-alist?var_A=a;var_B=b"))
(test '((A . "a") (B . "b")) (get "/compound/wrv-as-alist?var_A=a;var_B=b"))

(test #f (get "/compound/as-alist?var=c"))
(test #f (get "/compound/wrv-as-alist?var=c"))


;; Hash-tables
(test #f (get "/compound/as-hash-table"))
(test #f (get "/compound/wrv-as-hash-table"))

(test #t (let ((res (get "/compound/as-hash-table?var_A=a;var_B=b")))
           (lset= equal? res '((A . "a") (B . "b")))))

(test #t (let ((res (get "/compound/wrv-as-hash-table?var_A=a;var_B=b")))
           (lset= equal? res '((A . "a") (B . "b")))))

(test '((A . "")) (get "/compound/wrv-as-hash-table?var_A="))

(test #f (get "/compound/as-hash-table?var=c"))
(test #f (get "/compound/wrv-as-hash-table?var=c"))


;; test3
(test '(#f #f #f #f) (get "/compound/test3"))

(test '(((A . "0") (B . "1")) 0 #("a" "b") #t)
      (get "/compound/test3?foo_A=0&foo_B=1&bar=0&baz_0=a&baz_1=b&bool=yes"))

(test '(#f #f #f #f)
      (get "/compound/test3?foo=0&bar=a&baz=0&bool=3"))


;; test4
(test '(#f #f #f #f) (get "/compound/test4"))

(test '(((A . "0") (B . "1")) 0 #("a" "b") #t)
      (get "/compound/test4?foo_A=0&foo_B=1&bar=0&baz_0=a&baz_1=b&bool=yes"))

(test '(#f #f #f #f)
      (get "/compound/test4?foo=0&bar=a&baz=0&bool=3"))

(test-end "spiffy-request-vars")
