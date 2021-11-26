#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d positional-list adt) kak:)
        (a-d pattern-matching kmp))

; Oef 1

; a)
(define exercise-a-lst (list 'exercise-a
                             3
                             eq?
                             (vector 5 6 3 0 0 0 0 0 0 0 0 0)))

(define-record-type exercise-a
  (make-a n p v)
  header-a?
  (n number-a number-a!)
  (p proc-a proc-a!)
  (v vec-a))

(define exercise-a-record (make-a 3
                                  eq?
                                  (vector 5 6 3 0 0 0 0 0 0 0 0 0)))

; b)
(define-record-type exercise-b
  (make-b n v)
  header-b?
  (n number-b number-b!)
  (v vec-b))

(define exercise-b-record (make-b 4
                                (vector 6 -2 9 -1 -7 0 10 -5 1)))

; c)
(define-record-type exercise-c
  (make-c end neg l)
  header-c?
  (end end-c end-c!)
  (neg number-c number-c!)
  (l lst-c))

(define last (cons 7 '()))
(define list-c (list -5 -8 -1 6 2 0 last))

(define exercise-c-list (make-c last
                              3
                              list-c))

; Oef 2

; a)
;“and”, “me”, “to”, “goodday”, “hello”, “world”
;“hello” “world” “and” “goodday” “to” “me”
(define exercise-2 (kak:new string=?))
(kak:add-after! exercise-2 "and")
(kak:add-after! exercise-2 "me")
(kak:add-after! exercise-2 "to" (kak:first exercise-2))
(kak:add-after! exercise-2 "goodday" (kak:first exercise-2))
(kak:add-before! exercise-2 "hello")
(kak:add-after! exercise-2 "world" (kak:first exercise-2))

; b)
(define (count-words-containing-e l)
  (define result 0)
  (kak:for-each l (lambda (element)
                    (if (match element "e")
                        (set! result (+ result 1)))))
  result)
