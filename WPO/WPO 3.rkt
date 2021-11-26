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

; Oef 3
; - prefix
; - suffix
; - 3
; - |w| = 3
; - Yes

; Oef 4
; prefix: "" "h" "he" "hel" "hell" "hello"
; proper: "h" "he" "hel" "hell"
; suffix: "" "o" "lo" "llo" "ello" "hello"
; proper:  "o" "lo" "llo" "ello"

; Oef 6
(define (match-1 t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)
     (i-p 0)
     (res '()))
    (cond
      ((> i-p (- n-p 1))
       (loop (+ i-t 1) 0 (cons i-t res)))
      ((> i-t (- n-t n-p))
       (reverse res))
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1) res))
      (else
       (loop (+ i-t 1) 0 res)))))

; Oef 7
(define (match-2 t p w) ; vb (match "hallo hello" "h.llo" #\.)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)
     (i-p 0)
     (res '()))
    (cond
      ((> i-p (- n-p 1))
       (loop (+ i-t 1) 0 (cons i-t res)))
      ((> i-t (- n-t n-p))
       (reverse res))
      ((or (eq? (string-ref t (+ i-t i-p)) (string-ref p i-p)) (eq? (string-ref p i-p) w))
       (loop i-t (+ i-p 1) res))
      (else
       (loop (+ i-t 1) 0 res)))))

; Oef 11
(define (compute-failure-function p)
      (define n-p (string-length p))
      (define sigma-table (make-vector n-p 0))
      (let loop
        ((i-p 2)
         (k 0))
        (when (< i-p n-p)
          (cond
            ((eq? (string-ref p k) 
                  (string-ref p (- i-p 1)))
             (vector-set! sigma-table i-p (+ k 1))
             (loop (+ i-p 1) (+ k 1)))
            ((> k 0)
             (loop i-p (vector-ref sigma-table k)))
            (else ; k=0
             (vector-set! sigma-table i-p 0)
             (loop (+ i-p 1) k))))
        (vector-set! sigma-table 0 -1)
        (lambda (q)
          (vector-ref sigma-table q))))
 
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (define sigma (compute-failure-function p))
      (display t)
      (newline)
      (let loop
        ((i-t 0)
         (i-p 0))
        (cond 
          ((> i-p (- n-p 1))
           (newline)
           (display "Gedaan!"))
          ((> i-t (- n-t n-p))
           #f)
          ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (loop i-t (+ i-p 1)))
          (else
           (loop (+ i-t (- i-p (sigma i-p))) (if (> i-p 0)
                                                 (sigma i-p)
                                                 0))
           (display #\space)))))

; Oef 12
