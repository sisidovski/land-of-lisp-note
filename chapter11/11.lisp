(format t "Add onion rings for only ~$ dollars more!" 1.5)

(princ (reverse 
         (format nil "Add onion rings for only ~$ dollars more!" 1.5)))

(prin1 "foo")
(princ "foo")

(format t "I am printing ~s in the middle of this sentence." "foo")
(format t "I am printing ~a in the middle of this sentence." "foo")

(format t "I am printing ~10a within ten spaces of room." "foo")
(format t "I am printing ~10@a within ten spaces of room." "foo")
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
(format t "I am printing ~,,4a in the middle of this sentence." "foo")
(format t "The word ~,,4,'!a feels very important." "foo")
(format t "The word ~,,4,'!@a feels very important." "foo")

(format t "The number 1000 in hexadecimal is ~x" 1000)
(format t "The number 1000 in binary is ~b" 1000)
(format t "The number 1000 in decimal is ~d" 1000)

(format t "Numbers with conmas in them are ~:d times better." 1000000)
(format t "I am printing ~10d within ten spaces of room." 1000000)
(format t "I am printing ~10,'xd within ten spaces of room." 1000000)

(format t "PI can be estimated as ~4f" 3.141593)
(format t "PI can be estimated as ~,4f" 3.141593)
(format t "PI can be estimated as ~,4f" pi)

(format t "Percentages are ~,,2f percent better than fractions" 0.77)

(format t "I wish I had ~$ dollars in my bank account." 1000000.2)

(progn (princ 22)
       (terpri)
       (princ 33))

(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))

(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))
(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

(format t "this will pring ~5%on two lines spread far apart")

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(random-animal)

(loop repeat 10
      do (format t "~5t~a ~15t~a ~25t~a~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
(loop repeat 10
      do (format t "~30<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))
(loop repeat 10
      do (format t "~30:@<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
(loop repeat 10
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))
*animals*

(format t "~{I see a ~a! ~}" *animals*)
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
