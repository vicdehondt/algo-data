#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Exception Handling Implementation Tests              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (scheme base)
        (scheme write)
        (scheme inexact)
        (a-d examples try-catch))

(define (divide x y)
  (if (= y 0)
      (throw 'division-by-zero)
      (/ x y)))

(define (logarithm x y)
  (if (or (<= x 0)
          (<= y 0))
      (throw 'negative-log)
      (divide (log x) (log y))))

(define (do-it x y)
  (try-catch
   (lambda ()
     (try-catch
      (lambda ()
        (logarithm x y))
      (lambda (exception)
        (eq? exception 'division-by-zero))
      (lambda (exception)
        (display "x/0"))))
   (lambda (exception)
     (eq? exception 'negative-log))
   (lambda (exception)
     (display "log(-x)"))))