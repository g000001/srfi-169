(cl:in-package "https://github.com/g000001/srfi-169#internals")
(in-readtable :rnrs)

;;;; This is the reference test suite for the reference implementation
;;;; of SRFI 169: Underscores in Numbers.

;; You can use any procedure that reads from current-input-port.
(define read-procedure #'read-number)

(cl:defparameter all-conforming '())
(cl:defparameter all-non-conforming '())

(define (conforming string-repr right-answer)
  (set! all-conforming
        (append all-conforming (list (cons string-repr right-answer)))))

(define (non-conforming string-repr)
  (set! all-non-conforming
        (append all-non-conforming (list string-repr))))

;;; Integers

(conforming "0123" 123)
(conforming "0_1_2_3" 123)
(conforming "0_123" 123)
(conforming "01_23" 123)
(conforming "012_3" 123)
(conforming "+0123" 123)
(conforming "+0_123" 123)
(conforming "-0123" -123)
(conforming "-0_123" -123)

(non-conforming "_0123")
(non-conforming "0123_")
(non-conforming "0123__")
(non-conforming "01__23")
(non-conforming "0_1__2___3")
(non-conforming "+_0123")
(non-conforming "+0123_")
(non-conforming "-_0123")
(non-conforming "-0123_")

;;; Rational numbers

(conforming "1_2_3/4_5_6_7" 123/4567)
(conforming "12_34/5_678" 1234/5678)

(non-conforming "1_2_3/_4_5_6_7")
(non-conforming "_12_34/5_678")

;;; Real numbers
(conforming "0_1_23.4_5_6" 123.456D0)
(conforming "1_2_3.5e6" 123.5e6)
#+TBD (conforming "1_2e1_2" 12e12)
(conforming "1_2e1_2" 12000000000000)
(non-conforming "_0123.456")
(non-conforming "0123_.456")
(non-conforming "0123._456")
(non-conforming "0123.456_")
(non-conforming "123_.5e6")
(non-conforming "123._5e6")
(non-conforming "123.5_e6")
(non-conforming "123.5e_6")
(non-conforming "123.5e6_")
(non-conforming "12_e12")
(non-conforming "12e_12")
(non-conforming "12e12_")

;;; Complex numbers

;; TODO: (conforming "-12_3.0_00_00-12_34.56_78i" -123.00000-1234.5678i)
;; TODO: (conforming "-12_3.0_00_00@-12_34.56_78" -123.00000@-1234.5678)

#+TBD (non-conforming "-12_3.0_00_00-12_34.56_78_i")
#+TBD (non-conforming "-12_3.0_00_00-12_34.56_78i_")
(non-conforming "-12_3.0_00_00_@-12_34.56_78")
#+TBD (non-conforming "-12_3.0_00_00@_-12_34.56_78")

;;; Hypercomplex numbers

;; Kawa supports quaternions using the following syntax:

;; "1+2i-3j+4k"

;; By applying the rule a syntax like that can be extended as follows:

;; (conforming "1_0+2_0i-3_0j+4_0k")

(non-conforming "1_0_+2_0i-3_0j+4_0k")
#+TBD (non-conforming "1_0+2_0_i-3_0j+4_0k")
#+TBD (non-conforming "1_0+2_0i-3_0j_+4_0k")
#+TBD (non-conforming "1_0+2_0i-3_0j+4_0k_")

;;; Units of measure

;; By applying the rule a syntax like that can be extended as follows:

;; (conforming "123_456cm^2")

(non-conforming "123_456_cm^2")
(non-conforming "123_456.78_cm^2")

;;; Numbers with radix or exactness prefixes

(conforming "#b10_10_10" #b101010)
(conforming "#o23_45_67" #o234567)
(conforming "#d45_67_89" 456789)
(conforming "#xAB_CD_EF" #xABCDEF)
(conforming "#x789_9B_C9_EF" #x7899BC9EF)
(conforming "#x-2_0" #x-20)
(conforming "#o+2_345_6" #o23456)

(non-conforming "#x-_2")
(non-conforming "_#x-_2")
(non-conforming "#d_45_67_89")

(non-conforming "#e_45/67_89")
(non-conforming "#i#o_1234")
(non-conforming "#i_#o_1234")
(non-conforming "#e#x1234_")

;;; Run tests

(define (displayln x)
  (display x)
  (newline))

#++
(define (safe-read string-repr)
  (guard (err (else 'error))
    (parameterize ((current-input-port (srfi-6:open-input-string string-repr)))
      (read-procedure))))

(define (safe-read string-repr)
  (cl:with-input-from-string (cl:*standard-input* string-repr)
    (or (cl:ignore-errors (read-procedure))
        'error)))

(define (test-conforming pair)
  (let* ((string-repr (car pair))
         (right-answer (cdr pair))
         (val (safe-read string-repr)))
    (newline)
    (displayln string-repr)
    (cond ((and (number? val) (= right-answer val))
           (displayln "Conforming number correct!"))
          ((eqv? 'error val)
           (displayln "Conforming number caused read error"))
          ((number? val)
           (displayln "Conforming number read as a different number:")
           (displayln val))
          ((symbol? val)
           (displayln "Conforming number read as a symbol"))
          (else
           (displayln "Conforming number read as WTF")))))

(define (test-non-conforming string-repr)
  (let ((val (safe-read string-repr)))
    (newline)
    (displayln string-repr)
    (cond ((eqv? 'error val)
           (displayln "Non-conforming number caused read error"))
          ((number? val)
           (displayln "Non-conforming number read as a number:")
           (displayln val))
          ((symbol? val)
           (displayln "Non-conforming number read as a symbol"))
          (else
           (displayln "Non-conforming number read as WTF")))))

(define (run!)
  (displayln "Testing SRFI 169")
  (for-each #'test-conforming all-conforming)
  (newline)
  (displayln (make-string 70 #\=))
  (for-each #'test-non-conforming all-non-conforming)
  (let ((cl:*readtable* srfi-169-syntax))
    (cl:dolist (x all-conforming)
      (cl:print
       (cl:ignore-errors
         (cl:read-from-string (car x))))))
  #t)
