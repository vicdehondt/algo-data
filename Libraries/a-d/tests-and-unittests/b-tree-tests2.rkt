#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Tests for B-Trees                       *-*-
;-*-*                                                                 *-*-
;-*-*                           Theo D'Hondt                          *-*-
;-*-*                 1993 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d file constants)
        (prefix (a-d db rcid) rcid:)
        (prefix (a-d db index b-tree b-tree) b-tree:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d disk disk) disk:))

(define dsk (disk:new "treedisk"))
(fs:format! dsk)

(define d (b-tree:new dsk "Nummers" decimal-tag 8))

(b-tree:insert! d 3.0   (rcid:new 0 3476))
(b-tree:insert! d 1.2   (rcid:new 0   22))
(b-tree:insert! d 14.3  (rcid:new 0    8))
(b-tree:insert! d 54.2  (rcid:new 0 3550))
(b-tree:insert! d 9.0   (rcid:new 0 3100))
(b-tree:insert! d 90.4  (rcid:new 0 5600))
(b-tree:insert! d 43.3  (rcid:new 0 5050))
(b-tree:insert! d 33.5  (rcid:new 0  520))
(b-tree:insert! d 67.8  (rcid:new 0  600))
(b-tree:insert! d 123.2 (rcid:new 0 1200))
(b-tree:insert! d 98.3  (rcid:new 0 1300))
(b-tree:insert! d 94.2  (rcid:new 0 1300))
(b-tree:insert! d 85.0  (rcid:new 0 4950))
(b-tree:insert! d 22.1  (rcid:new 0  400))
(b-tree:insert! d 11.2  (rcid:new 0 1200))
(b-tree:insert! d 94.2  (rcid:new 0  300))
(b-tree:insert! d 87.3  (rcid:new 0  350))
(b-tree:insert! d 00.4  (rcid:new 0  600))
(b-tree:insert! d 432.2 (rcid:new 0  400))
(b-tree:insert! d 431.2 (rcid:new 0 1000))
(b-tree:insert! d 954.3 (rcid:new 20  800))
(b-tree:insert! d 749.2 (rcid:new 0  100))
(b-tree:insert! d 111.4 (rcid:new 0 4000))
(b-tree:insert! d 948   (rcid:new 0  300))


(newline)
(display "LINKS NAAR RECHTS")
(newline)
(b-tree:set-current-to-first! d)
(let loop
  ((p (b-tree:peek d)))
  (display p)(display " | ")
  (when (not (eq? (b-tree:set-current-to-next! d) 'no-current))
    (loop (b-tree:peek d))))


(b-tree:flush! d)