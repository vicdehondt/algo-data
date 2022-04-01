#lang r7rs
(import (scheme base)
        (scheme cxr))

(define (bubble-swap cons-cell)
  (let ((keep (car cons-cell)))
    (set-car! cons-cell (cadr cons-cell))
    (set-car! (cdr cons-cell) keep))
  #t)




    
(define (bubble-sort pair <<?)
  
  (let outer-loop
    ((unsorted-idx (- (length pair) 2))
     (current-pair (reverse pair)))
    (if (>= unsorted-idx 0)
        (if (let inner-loop
              ((inner-idx 0)
               (has-changed? #f)
               (current-pair-piece current-pair))
              (if (> inner-idx unsorted-idx)
                  has-changed?
                  (inner-loop (+ inner-idx 1)
                              (if (<<? (cadr current-pair-piece)
                                       (car current-pair-piece))
                                  (bubble-swap current-pair-piece)
                                  has-changed?)
                              (cdr current-pair-piece))))
            (outer-loop (- unsorted-idx 1) (cdr (reverse current-pair)))))
    (reverse current-pair)))
 
(define sort bubble-sort)