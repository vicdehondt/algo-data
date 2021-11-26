#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         BST Trees Test                          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2018 Software Languages Lab                    *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (scheme base)
 (a-d scheme-tools)
 (scheme write)
 (except (a-d tree binary-tree) new)
 (a-d tree binary-search-tree)
 (a-d tree binary-tree-algorithms))

(define (check-bst bst)
  (define (inner node)
    (if (null-tree? node)
      #t
      (let* ((lft (inner (left node)))
             (rgh (inner (right node)))
             (Lv (if (not (null-tree? (left node)))
                   (value (left node))
                   -inf.0))
             (Rv (if (not (null-tree? (right node)))
                   (value (right node))
                   +inf.0)))
        (and (< Lv (value node))
             (< (value node) Rv)
             lft
             rgh))))
  (inner (root bst)))

(define (random-inbetween l r)
  (+ l (random-integer (+ (- r l) 1))))

(define a (new = <))

(define (iter b n)
  (if (= n 0)
    (display "done")
    (let ((v (random-inbetween 10 10000)))
      (insert! b v)
      (display v)
      (iter b (- n 1)))))

(define (reti b n)
  (define count 0)
  (define (rreti n)
    (if (= n 0)
      (display "done")
      (let ((val (random-inbetween 10 10000)))
        (if (find b val)
          (begin (set! count (+ 1 count))
                 (delete! b val)))
        (rreti (- n 1)))))
  (rreti n)
  count)
(iter a 200)
(check-bst a)
;(reti a 200)
(check-bst a)