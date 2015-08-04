(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))

(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character"))

(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

'(+ 1 2)
(+ 1 2)

(defparameter *foo* '(+ 1 2))
(eval *foo*)

(defun game-repl()
  (loop (print (eval (read)))))

(defun game-repl()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

(game-print '(THIS IS A SENTENCE. WHAT ABOUT THIS? PROBABLY.))

(defun twek-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (twek-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (twek-text rest t lit)))
            ((eql item #\") (twek-text rest caps (not lit)))
            (lit (cons item (twek-text rest nil lit)))
            (caps (cons (char-upcase item) (twek-text rest nil lit)))
            (t (cons (char-downcase item) (twek-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (twek-text (coerce (string-trim "()"
                                                 (prin1-to-string lst))
                                    'list)
                            t
                            nil)
                 'string))
  (fresh-line))

; 6.5 lambda
(defun half (n)
  (/ n 2))

(lambda (n) (/ n 2))

(mapcar (lambda (n) (/ n 2)) '(2 4 6))
