(cl:in-package "https://github.com/g000001/srfi-169#internals")
(in-readtable :rnrs)


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defvar *srfi-169-syntax* (cl:copy-readtable #f))

  (let ((cl:*readtable* *srfi-169-syntax*)
        (stdrt (cl:copy-readtable #f)))
    (cl:flet ((setmc (c)
                (cl:set-macro-character
                 c
                 (lambda (stm chr)
                   (cl:unread-char chr stm)
                   (let ((cl:*standard-input* stm))
                     (read-number)))
                 #t))
              (set+- (c)
                (cl:set-macro-character
                 c
                 (lambda (stm chr)
                   (let ((cl:*standard-input* stm))
                     (cond ((cl:find (peek-char) "0123456789")
                            (if (cl:eql #\- chr)
                                (cl:- (read-number))
                                (read-number)))
                           (else
                             (cl:unread-char chr stm)
                             (let ((cl:*readtable* stdrt))
                               (read))))))
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
      (cl:map #f #'setmc "0123456789")
      (cl:map #f #'set+- "+-")
      (cl:map #f #'setdmc "bodx")))
  (cl:defconstant srfi-169-syntax *srfi-169-syntax*))


;;; *EOF*
