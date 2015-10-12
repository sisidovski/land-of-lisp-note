(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(add 2 3)

(defmacro let1 (var val &body body)
  '(let ((,var ,val))
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
