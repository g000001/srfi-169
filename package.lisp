;;;; package.lisp

(cl:in-package :cl-user)

(defpackage "https://github.com/g000001/srfi-169"
  (:use)
  (:export srfi-169-syntax))

(defpackage "https://github.com/g000001/srfi-169#internals"
  (:use "https://github.com/g000001/srfi-169"
        rnrs
        srfi-11
        srfi-23
        named-readtables)
  (:shadowing-import-from cl when unless))
