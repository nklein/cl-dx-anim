
(in-package :cl-dx-anim)

(defun name-to-reader (name)
  (intern (concatenate 'string "READ-" (symbol-name name))))

(defun expand-var (var)
  (etypecase var
   (list
    (case (first var)
      ((optional zero-or-more one-or-more seq)
       `(,*ignored-value* (,(first var) (,@(rest var)) ,(second var))))
      (t
       `(,(first var) ,(second (expand-var (second var)))))))
   (symbol
    (let ((reader (name-to-reader var)))
      `(,var (,reader))))
   (string
    `(,*ignored-value* (read-string ,var)))))

(defun var-to-let (var-or-var-reader)
  (destructuring-bind (var reader) (expand-var var-or-var-reader)
    `(,var ,reader)))

(defun var-to-ignorable (var-or-var-reader)
  (destructuring-bind (var reader) (expand-var var-or-var-reader)
    (declare (ignore reader))
    var))
