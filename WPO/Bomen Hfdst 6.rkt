#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d tree binary-tree) tree:))

; Oef 1
(define (leaf? tree)
  (and (tree:null-tree? (tree:left tree))
       (tree:null-tree? (tree:right tree))))

(define (count-leaves tree)
  (cond
    ((tree:null-tree? tree) 0)
    ((leaf? tree) 1)
    (else (+ (count-leaves (tree:left tree))
             (count-leaves (tree:right tree))))))

(define the-tree (tree:new 1
                           (tree:new 2
                                     tree:null-tree
                                     tree:null-tree)
                           (tree:new 5
                                     (tree:new 7
                                               tree:null-tree
                                               tree:null-tree)
                                     tree:null-tree)))

; Oef 2
(define (depth tree)
  (if (tree:null-tree? tree)
      0
      (+ 1 (max (depth (tree:left tree))
                (depth (tree:right tree))))))
(define (height-tree tree)
  (- (depth tree) 1))

; Oef 3

(define (nodes tree)
  (if (tree:null-tree? tree)
      0
      (+ 1
         (nodes (tree:left tree))
         (nodes (tree:right tree)))))

(define (count-children tree)
  (- (nodes tree) 1))

; Oef 6
;Zie blad