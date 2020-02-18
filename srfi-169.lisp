(cl:in-package "https://github.com/g000001/srfi-169#internals")
(in-readtable :rnrs)

;;; Conditional reading utility

(define (read-char-limit? chars limit)
  (let ((ch (peek-char)))
    (let loop ((i 0))
      (cond ((>= i limit) #f)
            ((eqv? ch (string-ref chars i)) (read-char) i)
            (else (loop (+ i 1)))))))

(define (read-char? chars)
  (read-char-limit? chars (string-length chars)))

;;; Digit span reader

;; All the underscore considerations are in the following procedure,
;; which reads a span of one or more digits. It returns two values:
;; the exact nonnegative integer value represented by the digits, and
;; the count of digits. If there are no digits, those are #f and 0.

(define (read-and-count-digits? radix)
  (let ((radix (or radix 10))
        (lodigits "0123456789abcdef")
        (updigits "0123456789ABCDEF"))
    (let loop ((was-digit? #f) (count 0) (value #f))
      (if (read-char? "_")
          (cond (was-digit? (loop #f count value))
                (value (error "More than one consecutive underscore"))
                (else (error "Underscore before digits")))
          (let ((digit (or (read-char-limit? lodigits radix)
                           (read-char-limit? updigits radix))))
            (cond (digit
                   (loop #t (+ count 1) (+ digit (* radix (or value 0)))))
                  ((and value (not was-digit?))
                   (error "Underscore after digits"))
                  (else (values value count))))))))

(define (read-digits? radix)
  (let-values (((value _) (read-and-count-digits? radix)))
    _              
    value))

(define (read-fractional-part)
  (let-values (((value count) (read-and-count-digits? 10)))
    (inexact (/ (or value 0) (expt 10 count)))))

;;; Scheme number reader

;; The rest of the code covers a large subset of R7RS number syntax.
;; Complex numbers are regrettably not included. I am not a math
;; wizard and did not work on any RnRS report so do not trust this
;; code as an authority on how to do numbers right.

(define (read-prefix? old chars values error-msg)
  (let ((new (read-char? chars)))
    (cond ((and old new) (error error-msg))
          (new (vector-ref values new))
          (else #f))))

(define (read-radix-prefix? old)
  (read-prefix? old "bodx" #(2 8 10 16) "More than one radix prefix"))

(define (read-exactness-prefix? old)
  (read-prefix? old "ei" #(e i) "More than one exactness prefix"))

(define (read-number-prefixes)
  (let loop ((radix #f) (exactness #f))
    (if (not (read-char? "#"))
        (values radix exactness)
        (let ((new-radix (read-radix-prefix? radix)))
          (if new-radix
              (loop new-radix exactness)
              (let ((new-exactness (read-exactness-prefix? exactness)))
                (if new-exactness
                    (loop radix new-exactness)
                    (error "Unknown # prefix"))))))))

(define (make-decimal integer-part fractional-part exponent-part)
  (* (+ integer-part fractional-part) (expt 10 (or exponent-part 0))))

(define (read-exponent? radix)
  (cond ((not (read-char? "e")) #f)
        (radix (error "Exponent not allowed with radix prefix"))
        (else (or (read-digits? 10) (error "No exponent")))))

(define (read-decimal-part radix integer-part)
  (when radix (error "Decimal point not allowed with radix prefix"))
  (let ((fractional-part (read-fractional-part)))
    (make-decimal integer-part fractional-part (read-exponent? radix))))

(define (read-ureal? radix)
  (if (read-char? ".")
      (read-decimal-part radix 0)
      (let ((first-digits (read-digits? radix)))
        (cond ((not first-digits) #f)
              ((read-char? ".") (read-decimal-part radix first-digits))
              ((read-char? "/")
               (let ((other-digits (read-digits? radix)))
                 (if other-digits
                     (/ first-digits other-digits)
                     (error "Missing denominator in ratio"))))
              (else
               (make-decimal first-digits 0 (read-exponent? radix)))))))

(define (read-number)
  (let-values (((radix exactness) (read-number-prefixes)))
    (let* ((sign (if (read-char? "-") -1 (begin (read-char? "+") 1)))
           (magnitude (read-ureal? radix)))
      #++(unless (eof-object? (peek-char)) (error "Junk after number"))
      (cond ((and (not magnitude) (or radix exactness))
             (error "Missing numeric value after # prefix"))
            ((not magnitude)
             (error "Missing numeric value"))
            (else (let ((value (* sign magnitude)))
                    (cl:case exactness
                      ((e) (exact value))
                      ((i) (inexact value))
                      (cl:otherwise value))))))))

