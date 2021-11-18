#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (scheme char))

; oef 2
(define (power x y)
  (if (= y 0)
      1
      (* x (power x (- y 1)))))

(define (my-string->number string)
  (define (iter to-the count res)
    (if (= count (string-length string))
        res
        (iter (- to-the 1) (+ count 1) (+ res (* (- (char->integer (string-ref string count)) 48) (power 10 to-the))))))
  (iter (- (string-length string) 2) 1 (* (- (char->integer (string-ref string 0)) 48) (power 10 (- (string-length string) 1)))))

; of
(define (my-second-string->number string)
  (define (iter count res)
    (if (= count (string-length string))
        (/ res 10)
        (iter (+ count 1) (* (+ res (- (char->integer (string-ref string count)) 48)) 10))))
  (iter 1 (* (- (char->integer (string-ref string 0)) 48) 10)))

