#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Coroutine 2 Implementation Tests               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (scheme base)
        (scheme write)
        (a-d examples coroutines2))

(define p1
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 1 ")
       (display input)
       (newline)
       (set! input (suspend (+ input 1)))))))

(define p2
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 2 ")
       (display input)
       (newline)
       (set! input (suspend (+ input 1)))))))

(define p3
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 3 ")
       (display input)
       (newline)
       (set! input (suspend (+ input 1)))))))
