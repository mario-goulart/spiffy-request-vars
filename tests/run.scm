(use server-test test spiffy posix)

(test-server-port
 (cond ((get-environment-variable "SPIFFY_TEST_PORT")
        => (lambda (port)
             (string->number port)))
       (else (server-port))))

(server-port (test-server-port))

(with-test-server
 (lambda ()
   (load "server.scm"))
 (lambda ()
   (load "client.scm")))

(test-exit)
