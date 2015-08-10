(make-array 3)
(defparameter x (make-array 3))
(aref x 1)
(defparameter x (make-array 3))
(setf (aref x 1) 'foo)
(aref x 1)

(setf foo (list 'a 'b 'c))
(second foo)
(setf (second foo) 'z)
(setf foo (make-array 4))
(setf (aref foo 2) (list 'x 'y 'z))
foo
(setf (car (aref foo 2)) (make-hash-table))
(setf (gethash 'zoink (car (aref foo 2))) 5)

(make-hash-table)

(defparameter x (make-hash-table))
(gethash 'yup x)

(defparameter x (make-hash-table))
(setf (gethash 'yup x) '25)
(gethash 'yup x)


(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-latte)
(gethash 'lisa *drink-order*)

(round 2.4)

(defun foo ()
	(values 3 7))
(foo)

(+ (foo) 5)

(multiple-value-bind (a b) (foo)
	(* a b)
	)

; grund sept wamps
(setf *edge-num* 1000)
(setf *node-num* 1000)
(time (dotimes (i 100) (get-connected 1 (make-edge-list))))

(defun hash-edges (edge-list)
	(let ((tab (make-hash-table)))
		(mapc (lambda (x)
						(let ((node (car x)))
							(push (cdr x) (gethash node tab))))
					edge-list)
		tab))

(defun get-connected-hash (node edge-tab)
	(let ((visited (make-hash-table)))
		(labels ((traverse (node)
						 (unless (gethash node visited)
							 (setf (gethash node visited) t)
							 (mapc (lambda (edge)
											 (traverse edge))
										 (gethash node edge-tab)))))
		(traverse node))
		visited))

(time (dotimes (i 100)
				(get-connected-hash 1 (hash-edges (make-edge-list)))))

(defstruct person
	name 
	age 
	waist-size
	favorite-color)
(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))
*bob*
(person-age *bob*)
(setf (person-age *bob*) 36)
(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
(person-age *that-guy*)

(defun make-person (name age waist-size favorite-color)
  (list name age waist-size favorite-color))
(defun person-age (person) (cadr person))
(defparameter *bob* (make-person "bob" 35 32 "blue"))
*bob*
(person-age *bob*)

(length '(a b c))
(length "blub")
(length (make-array 5))

(find-if #'numberp '(a b 5 d))
(count #\s "misissippi")
(position #\4 "2kewl4skewl")
(some #'numberp '(a b 5 d))
(every #'numberp '(a b 5 d))

(reduce #'+ '(3 4 6 5 2))
; good
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
            item
            best))
        '(7 4 6 5 2)
        :initial-value 0)
; bad
(reduce (lambda (best item)
         (if (and (evenp item) (> item best))
           item
           best))
        '(7 4 6 5 2))

(defun sum (lst)
  (reduce #'+ lst))
(sum '(1 2 3))
(sum (make-array 5 :initial-contents '(1 2 3 4 5)))
(sum "blablabla")

(map 'list 
     (lambda (x)
       (if (eq x #\s)
         #\S
         x))
     "this is a string")

(subseq "america" 2 6)
(sort '(5 8 2 4 9 3 6) #' <)

(numberp 6)
(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b) (append a b)))))
