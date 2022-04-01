#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d tree binary-tree) tree:))

(define (checkavltree)
  (and (check-values avltree)
       (check-heights avltree)))

(define (check-heights bst)
  (define (left-count bst)
    (cond
      ((tree:null-tree? bst) 0)
      ((and (tree:null-tree? (tree:left bst)) (tree:null-tree? (tree:right bst))))))
  (or (= (modulo (left-count (tree:left bst)) (right-count (tree:right bst))) 1)
      (= (modulo (left-count (tree:left bst)) (right-count (tree:right bst))) 0)))