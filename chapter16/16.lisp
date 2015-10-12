(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(add 2 3)

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let ((foo (+ 2 3)))
  (* foo foo))

(let1 foo (+ 2 3)
      (* foo foo))

(defun add (a b)
  (let1 x (+ a b)
        (format t "The sum is ~a" x)
        x))

(macroexpand '(let1 foo (+ 2 3)
                    (* foo foo)))

(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst 
               (f (cdr lst) (1+ acc))
               acc)))
    (f lst 0)))

(split '(2 3)
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))

(split '()
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))

; include some bugs
(defmacro split (val yes no)
  `(if ,val
     (let ((head (car ,val))
           (tail (cdr ,val)))
       ,yes)
     ,no))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst 
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

; better
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                 (let ((head (car ,g))
                       (tail (cdr ,g)))
                   ,yes)
                 ,no))))

(macroexpand '(split '(2 3)
                     (+ x head)
                     nil))

(macroexpand '(split '(2 3)
                     (+ x head)
                     nil))


(recurse (n 9)
         (fresh-line)
         (if (zerop n)
           (princ "lift-off!")
           (progn (princ n)
                  (self (1- n)))))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst 
                    (if tail
                      (f (cdr tail) (cons (cons head (car tail)) acc))
                      (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(pairs '(a b c d e f))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p)
                    ,@body))
           (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst 
                acc 0)
           (split lst 
                  (self tail (1+ acc))
                  acc)))

; use reduce
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst 
          :initial-value 0))
