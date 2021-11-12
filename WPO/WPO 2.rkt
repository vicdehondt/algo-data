#lang r7rs

; Oefening 3
; Maak een library met een ADT die een breuk maakt en 2 breuken kan + - * /
(define-library (fraction)
  (export new numer denom fraction? + - / *)
  (import (scheme base)
          (rename (scheme base) (+ base:+) (- base:-) (/ base:/) (* base:*))
          (scheme write))
  (begin
    (define-record-type fraction
      (new n d)
      fraction?
      (n numer)
      (d denom))

    (define (+ x y)
      (if (and (fraction? x) (fraction? y))
          (new (base:+ (base:* (numer x) (denom y)) (base:* (numer y) (denom x)))
               (base:* (denom x) (denom y)))
          (display "Not same type")))

    (define (- x y)
      (if (and (fraction? x) (fraction? y))
          (new (base:- (base:* (numer x) (denom y)) (base:* (numer y) (denom x)))
               (base:* (denom x) (denom y)))
          (display "Not same type")))

    (define (* x y)
      (if (and (fraction? x) (fraction? y))
          (new (base:* (numer x) (numer y))
               (base:* (denom x) (denom y)))
          (display "Not same type")))

    (define (/ x y)
      (if (and (fraction? x) (fraction? y))
          (new (base:* (numer x) (denom y))
               (base:* (denom x) (numer y)))
          (display "Not same type")))))