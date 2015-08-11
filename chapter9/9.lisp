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
        ((and (listp a)) (listp b) (append a b))))

(add 3 5)
(add '(a b) '(c d))

(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))
(add 3 4)
(add '(a b) '(c d))

; Ork Battle
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have benn killed. Game Over."))
  (when (monsters-dead)
    (princ "Congulatulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list 
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble [r]oundhouse ")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
      (random-monster)
      m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
      (progn (princ "That is not a valid monster number.")
             (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
          (progn (princ "That monster is already dead.")
                 (pick-monster))
          m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list 
         (lambda (m)
           (fresh-line)
           (princ "  ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
             (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
    *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (progn (princ "You killed the ")
           (princ (type-of m))
           (princ "! "))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A mailcious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (princ "The corpse of the fully decapitated and decapaticated hydra falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirs in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))



; other
(dotimes (i 3)
  (fresh-line)
  (princ i)
  (princ ". Hatchoo!"))
(princ "Hoge")

(dotimes (i 10)
  (princ (random 5))
  (princ " "))

(make-monster)

(type-of 'foo)
(type-of 5)
(type-of "foo")
(type-of (make-monster))
