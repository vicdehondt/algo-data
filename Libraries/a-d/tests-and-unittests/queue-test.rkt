#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                             Queues                              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (except (scheme base) length)
        (a-d queue linked)
        ;(a-d queue circular-vector)
        ;(a-d queue list)
        (only (racket base) require planet))

(require (planet schematics/schemeunit:3/test))

(define q (new))

(check-equal? (empty? q) #t)

(enqueue! q 10)

(check-equal? (empty? q) #f)

(enqueue! q 20)
(enqueue! q 30)
(define first (peek q))

(check-equal? first 10)
(check-equal? (serve! q) 10)
(check-equal? (serve! q) 20)
(check-equal? (serve! q) 30)

(check-equal? (empty? q) #t)

