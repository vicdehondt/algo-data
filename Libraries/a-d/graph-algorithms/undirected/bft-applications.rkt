#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Undirected BFS Applications                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (basic algorithms)
  (export shortest-path distance)
  (import (scheme base)
          (a-d graph unweighted config)
          (a-d graph-traversing bft))
  (begin
   
    (define (shortest-path g from to)
      (define paths (make-vector (order g) '()))
      (vector-set! paths from (list from))
      (bft g 
           root-nop
           (lambda (node) 
             (not (eq? node to)))
           (lambda (from to)
             (vector-set! paths to (cons to (vector-ref paths from))))
           edge-nop
           (list from))
      (vector-ref paths to))
 
    (define (distance g from to)
      (define distances (make-vector (order g) +inf.0))
      (vector-set! distances from 0)
      (bft g 
           root-nop
           (lambda (node) 
             (not (eq? node to)))
           (lambda (from to)
             (vector-set! distances to (+ (vector-ref distances from) 1)))
           edge-nop
           (list from))
      (vector-ref distances to))))