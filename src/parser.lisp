
(in-package :cl-dx-anim)

(defvar *ignored-value* (gensym "IGNORED-VALUE-"))

(defmacro optional ((type &optional default) &body body)
  (let ((var (expand-var type))
        (pos (gensym "POS-")))
    `(let ((,(first var)
            (let ((,pos (file-position *standard-input*)))
              (handler-case ,(second var)
               (error ()
                 (file-position *standard-input* ,pos)
                 ,default)))))
       (declare (ignorable ,(first var)))
       ,@body)))

(defmacro zero-or-more ((type) &body body)
  (let ((var (expand-var type))
        (vals (gensym "VALS-"))
        (pos (gensym "POS-"))
        (one  (gensym "ONE-")))
    `(let ((,(first var)
            (loop :with ,vals = nil
                  :do (let ((,pos (file-position *standard-input*)))
                        (handler-case (let ((,one ,(second var)))
                                        (push ,one ,vals))
                          (error ()
                            (file-position *standard-input* ,pos)
                            (return (nreverse ,vals))))))))
       (declare (ignorable ,(first var)))
       ,@body)))

(defmacro one-or-more ((type) &body body)
  (let ((var (expand-var type)))
    `(let ((,(first var) (list* ,(second var)
                                (zero-or-more (,type) ,(first var)))))
       (declare (ignorable ,(first var)))
       ,@body)))

(defmacro seq ((&rest var-readers) &body body)
  `(let ,(mapcar #'var-to-let var-readers)
     (declare (ignorable ,@(mapcar #'var-to-ignorable var-readers)))
     ,@body))

(defmacro defparser (name (op &rest vars-or-var-readers) &body body)
  `(defun ,(name-to-reader name) (&optional (*standard-input*
                                             *standard-input*))
     (,op ,vars-or-var-readers
         ,@body)))

(defun read-string (string &optional (*standard-input* *standard-input*))
  (loop :for tgt :across string
        :for ch = (read-char)
        :do (assert (char= ch tgt)))
  string)

(defparser digit (seq char)
  (assert (digit-char-p char))
  (digit-char-p char))

(defparser number (one-or-more digit)
  (flet ((accumulate (acc d) (+ (* acc 10) d)))
    (reduce #'accumulate digit :initial-value 0)))
