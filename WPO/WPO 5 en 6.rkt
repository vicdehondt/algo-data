#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d positional-list adt) poslst:)
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
(define exercise-2 (poslst:new string=?))
(poslst:add-after! exercise-2 "and")
(poslst:add-after! exercise-2 "me")
(poslst:add-after! exercise-2 "to" (poslst:first exercise-2))
(poslst:add-after! exercise-2 "goodday" (poslst:first exercise-2))
(poslst:add-before! exercise-2 "hello")
(poslst:add-after! exercise-2 "world" (poslst:first exercise-2))

; b)
(define (count-words-containing-e l)
  (define result 0)
  (poslst:for-each l (lambda (element)
                       (if (match element "e")
                           (set! result (+ result 1)))))
  result)

; Oef 3
(define (pair-eq? p1 p2)
  (eq? (cdr p1) (cdr p2)))

;(define m (poslst:map l
;                      (lambda (element)
;                        (cons element (string-length element)))
;                      pair-eq?))

; Oef 4
; zie single-linked.rkt

; Oef 5

;(define (accumulate plst combiner null)
;      (define (iter pos result)
;        (if (has-next? plst pos)
;            (iter (next plst pos) (combiner (peek plst pos) result))
;            (combiner (peek plst pos) result)))
;      (iter (first plst) null))

(define testlist (poslst:from-scheme-list (list 1 2 3 4) =))

(define (test lst)
  (poslst:accumulate lst + 0))

; Oef 6

(define (intersection p1 p2)
  (define (iter pos result)
    (if (poslst:has-next? p1 pos)
        (if (poslst:find p2 (poslst:peek p1 pos))
            (iter (poslst:next p1 pos) (append (list (poslst:peek p2 (poslst:find p2 (poslst:peek p1 pos)))) result))
            (iter (poslst:next p1 pos) result))
        (if (poslst:find p2 (poslst:peek p1 pos))
            (begin
              (append (list (poslst:peek p2 (poslst:find p2 (poslst:peek p1 pos)))) result)
              (poslst:from-scheme-list result))
            (poslst:from-scheme-list result eq?))))
  (iter (poslst:first p1) '()))

; Oef 8
