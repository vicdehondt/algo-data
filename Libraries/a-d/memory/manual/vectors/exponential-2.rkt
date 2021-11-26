#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Exponential Buddy System Memory Manager            *-*-
;-*-*                  (optimised freelist management)                *-*-
;-*-*                 Theo D'Hondt - Wolfgang De Meuter               *-*-
;-*-*               1993-2009 Programming Technology Lab              *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (exponential)
 (export initialize size-free overhead null locate-free pop-free push-free peek poke!)
 (import (rnrs base)
         (rnrs control))
 
 (define overhead 1)
 (define null '())
 
 (define memory-size (expt 2 5))
 (define memory  (make-vector memory-size null))
 
 (define (peek addr)
   (vector-ref memory addr))
 
 (define (poke! addr value)
   (vector-set! memory addr value))
 
 (define lower-size 16)
 
 (define max-index (let loop
                         ((size memory-size)
                          (index 0))
                         (if (< size lower-size)
                           index
                           (loop (div size 2) (+ index 1)))))
 
 (define free-tab (make-vector max-index))
 (define size-tab (make-vector max-index))
 
 (define (size index)
   (vector-ref size-tab (- index 1)))
 
 (define (size! index size)
   (vector-set! size-tab (- index 1) size))
 
 (define (free index)
   (vector-ref free-tab (- index 1)))
 
 (define (free! index free)
   (vector-set! free-tab (- index 1) free))
 
 (define (index addr)
   (peek (+ addr 0)))
 
 (define (index! addr index)
   (poke! (+ addr 0) index))
 
 (define (next addr)
   (peek (+ addr 1)))
 
 (define (next! addr next)
   (poke! (+ addr 1) next))
 
 (define (previous addr)
   (peek (+ addr 2)))
 
 (define (previous! addr next)
   (poke! (+ addr 2) next))
 
 (define (locate-free req-size)
   (define (locate first last)
     (if (<= first last)
       (let 
           ((here (div (+ first last) 2)))
         (if (> req-size (size here))
           (locate (+ here 1) last)
           (locate first (- here 1))))
       first))
   (define index (locate 1 max-index))
   (if (> index max-index)
     null
     index))
 
 (define (size-free addr)
   (size (index addr)))
 
 (define (pop-free index) 
   (define (split index)
     (if (= index max-index)
         null
         (let
             ((addr (pop-free (+ index 1))))
           (if (null? addr)
               null
               (let*
                   ((size (size index))
                    (buddy-addr (+ addr size)))
                 (next!     buddy-addr null)
                 (previous! buddy-addr null)
                 (free!  index buddy-addr)
                 (index! buddy-addr (- index))
                 (index! addr index)
                 addr)))))
   (define addr (free index))
   (if (null? addr)
     (split index)
     (let 
         ((next (next addr)))
       (free! index next)
       (if (not (null? next))
         (previous! next null))
       (index! addr index)
       addr)))
 
 (define (push-free addr)
   (define (merge addr addr-index)
     (if (< addr-index max-index)
       (let*
           ((block-size (size addr-index))
            (left-buddy (even? (div addr block-size)))
            (buddy-addr ((if left-buddy + -) addr block-size))
            (buddy-index (index buddy-addr)))
         (if (= addr-index (- buddy-index))
           (let 
               ((next (next buddy-addr))
                (previous (previous buddy-addr)))
             (if (null? previous)
               (free! addr-index next)
               (next! previous   next))
             (if (not (null? next))
               (previous! next previous))
             (push (if left-buddy addr buddy-addr) (+ addr-index 1))
             #t)
           #f))
       #f))
   (define (push addr index)
     (if (not (merge addr index))
       (let 
           ((next (free index)))
         (next!     addr next)
         (previous! addr null)
         (if (not (null? next))
           (previous! next addr))
         (index! addr (- index))
         (free!  index addr))))
   (push addr (index addr)))
 
 (define (initialize)
   (let loop 
     ((index 1)
      (size lower-size))
     (when (<= index max-index)
       (size! index size)
       (free! index null)
       (loop (+ index 1) (* size 2))))
   (poke! 0 (locate-free memory-size))
   (push-free 0)))