(import scheme)
(cond-expand
  (chicken-4
   (use posix server-test spiffy test))
  ((or chicken-5 chicken-6)
   (import (chicken process-context))
   (import server-test spiffy test))
  (else
   (error "Unsupported CHICKEN version.")))

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
