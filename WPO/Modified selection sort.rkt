#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr))

; Oef 4
(define (make-index-vector n)
  (define index-vector (make-vector n 0))
  (define (iter i)
    (if (< i n)
        (begin
          (vector-set! index-vector i i)
          (iter (+ i 1)))
        index-vector))
  (iter 0))

(define (selection-sort input-vector <<?)
  (define vector (make-index-vector (vector-length input-vector)))
  (define (swap vector i j)
    (let ((keep (vector-ref vector i)))
      (vector-set! vector i (vector-ref vector j))
      (vector-set! vector j keep)))
  (let outer-loop
    ((outer-idx 0))
    (swap vector
          outer-idx 
          (let inner-loop
            ((inner-idx (+ outer-idx 1))
             (smallest-idx outer-idx))
            (cond 
              ((>= inner-idx (vector-length input-vector))
               smallest-idx)
              ((<<? (vector-ref input-vector (vector-ref vector inner-idx))
                    (vector-ref input-vector (vector-ref vector smallest-idx)))
               (inner-loop (+ inner-idx 1) inner-idx))
              (else
               (inner-loop (+ inner-idx 1) smallest-idx)))))
    (if (< outer-idx (- (vector-length input-vector) 1))
        (outer-loop (+ outer-idx 1))))
  vector)

; Oef 9
(define (merge-sort lst <<?)
  (define (split lst)
    (define (loop current result)
      (if (or (= (length current) (/ (length lst) 2)) (= (length current) (/ (+ (length lst) 1 ) 2)))
          (cons result current)
          (loop (cdr current) (cons (car current) result))))
    (loop lst '()))
  
  (define (merge lst1 lst2 <<?)
    (cond
      ((null? lst1) lst2)
      ((null? lst2) lst1)
      ((<<? (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2 <<?)))
      (else (cons (car lst2) (merge lst1 (cdr lst2) <<?)))))
  
  (if (> (length lst) 1)
      (let* ((helften (split lst))
             (linkerhelft (car helften))
             (rechterhelft (cdr helften)))
        
        (merge (merge-sort linkerhelft <<?)
               (merge-sort rechterhelft <<?)
               <<?))
      lst))

; Oef 7
(define ())




