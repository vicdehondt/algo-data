#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Priority Queues Tests                       *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import ;(a-d priority-queue heap)
        ;(a-d priority-queue sorted-list)
        (a-d priority-queue positional-list)
        (scheme base)
        (only (racket base) require planet))

(require (planet schematics/schemeunit:3/test))

(define p (new >))

(check-equal? (empty? p) #t)

(enqueue! p "bloop" 5)
(enqueue! p "floop" 1)
(enqueue! p "gloop" 6)

(check-equal? (peek p) "gloop")
(check-equal? (serve! p) "gloop")

(check-equal? (serve! p) "bloop")
(check-equal? (serve! p) "floop")

(check-equal? (empty? p) #t)