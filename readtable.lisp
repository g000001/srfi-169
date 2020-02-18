(cl:in-package "https://github.com/g000001/srfi-169#internals")
(in-readtable :rnrs)


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defvar *srfi-169-syntax* (cl:copy-readtable () ))

  (let ((cl:*readtable* *srfi-169-syntax*))
    (cl:flet ((setmc (c)
                (cl:set-macro-character
                 c
                 (lambda (stm chr)
                   (cl:unread-char chr stm)
                   (let ((cl:*standard-input* stm))
                     (read-number)))
                 #t))
              (setdmc (c)
                (cl:set-dispatch-macro-character
                 #\# c
                 (lambda (stm chr arg)
                   (cl:declare (cl:ignore arg))
                   (cl:unread-char chr stm)
                   (cl:unread-char #\# stm)
                   (let ((cl:*standard-input* stm))
                     (read-number))))))
      (cl:map #f #'setmc "+-0123456789")
      (cl:map #f #'setdmc "bodx")))
  (cl:defconstant srfi-169-syntax *srfi-169-syntax*))


;;; *EOF*
