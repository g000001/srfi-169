;;;; srfi-169.asd

(cl:in-package :asdf)


(defsystem :srfi-169
  :version "20200219"
  :description "SRFI 169 for CL: Underscores in numbers"
  :long-description "SRFI 169 for CL: Underscores in numbers
https://srfi.schemers.org/srfi-169"
  :license "MIT"
  :author "Lassi Kortela"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:named-readtables
               :rnrs-compat
               :srfi-11
               :srfi-23)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-169")
               (:file "readtable")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-169))))
  (let ((name "https://github.com/g000001/srfi-169")
        (nickname :srfi-169))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-169))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-169#internals")))
    (eval (read-from-string "(run!)"))))


;;; *EOF*
