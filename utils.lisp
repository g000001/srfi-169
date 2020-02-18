(cl:in-package "https://github.com/g000001/srfi-169#internals")


(define (inexact n)
  (cl:float n 0d0))


(define (exact n)
  (cl:rational n))


;;; *EOF*
