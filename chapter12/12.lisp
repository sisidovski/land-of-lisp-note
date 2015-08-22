(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))

(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))

(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :output 
                                      :if-exists :supersede)
  (print "my data" my-stream))

; scoket stream
(defparameter my-socket (socket-server 4321)) ; on the server
(defparameter my-stream (socket-accept my-socket)) ; on the server

(defparameter my-stream (socket-connect 4321 "127.0.0.1")) ; on the client
(print "Yo Server!" my-stream)

(read my-stream)
(print "What up, Client!" my-stream)

(read my-stream)
(close my-stream)

(close my-stream)
(socket-server-close my-socket)

; string stream
(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will also go into foo." foo)
(get-output-stream-string foo)

(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
