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
        (rnrs mutable-pairs)
        (a-d file constants)
        (a-d scheme-tools)
        (prefix (a-d db rcid) rcid:)
        (prefix (a-d db index b-tree b-tree) b-tree:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d disk disk) disk:))

(define dsk (disk:new "treedisk"))
(fs:format! dsk)
(define disk-size (fs:df dsk))
(define d (b-tree:new dsk "Manen" string-tag 10))

(b-tree:insert! d "Maan"      rcid:null)
(b-tree:insert! d "Phobos"    rcid:null)
(b-tree:insert! d "Deimos"    rcid:null)
(b-tree:insert! d "Io"        rcid:null)
(b-tree:insert! d "Europa"    rcid:null)
(b-tree:insert! d "Ganymedes" rcid:null)
(b-tree:insert! d "Callisto"  rcid:null)
(b-tree:insert! d "Mimas"     rcid:null)
(b-tree:insert! d "Enceladus" rcid:null)
(b-tree:insert! d "Tethys"    rcid:null)
(b-tree:insert! d "Dione"     rcid:null)
(b-tree:insert! d "Rhea"      rcid:null)
(b-tree:insert! d "Titan"     rcid:null)
(b-tree:insert! d "Hyperion"  rcid:null)
(b-tree:insert! d "Japetus"   rcid:null)
(b-tree:insert! d "Phoebe"    rcid:null)
(b-tree:insert! d "Janus"     rcid:null)
(b-tree:insert! d "Ariel"     rcid:null)
(b-tree:insert! d "Umbriel"   rcid:null)
(b-tree:insert! d "Titania"   rcid:null)
(b-tree:insert! d "Oberon"    rcid:null)
(b-tree:insert! d "Miranda"   rcid:null)
(b-tree:insert! d "Triton"    rcid:null)
(b-tree:insert! d "Nereide"   rcid:null)
(b-tree:print d)
(newline)
(display "DONE")
(newline)

(b-tree:find! d "Triton")
(newline)
(display (list "peek at Triton" (b-tree:peek d)))(newline)
(b-tree:set-current-to-next! d)
(display (list "peek at next" (b-tree:peek d)))(newline)

(b-tree:delete! d)

(b-tree:find! d "Titan")
(b-tree:peek d)
(b-tree:update! d (rcid:new 0 9999))
(display (list "updates Titan = " (b-tree:peek d)))(newline)
  
(newline)(newline)
(display "VAN LINKS NAAR RECHTS")(newline)

(b-tree:find! d "Ariel")
(let loop
  ((p (b-tree:peek d)))
  (display p)(display " | ")
  (when (not (eq? (b-tree:set-current-to-next! d) 'no-current))
    (loop (b-tree:peek d))))

(newline)
(display "NOG EENS met set-current-to-first")
(newline)
(b-tree:set-current-to-first! d)
(let loop
  ((p (b-tree:peek d)))
  (display p)(display " | ")
  (when (not (eq? (b-tree:set-current-to-next! d) 'no-current))
    (loop (b-tree:peek d))))
(newline)

(define manen (vector "Maan" "Phobos" "Deimos" "Io" "Europa"
                      "Ganymedes" "Callisto" "Mimas" "Enceladus"
                      "Tethys" "Dione" "Rhea" "Titan" "Hyperion"
                      "Japetus" "Phoebe" "Janus" "Ariel" "Umbriel"
                      "Titania" "Oberon" "Miranda" "Triton" "Nereide"))
(define del-vector (cons (vector-length manen) manen))

(define (del-random?)
  (if (= (car del-vector) 0)
      #t
      (let ((rnd (random-inbetween 0 (- (car del-vector) 1))))
        (display (list "deleting" (vector-ref (cdr del-vector) rnd)))
        (b-tree:find! d (vector-ref (cdr del-vector) rnd))
        (b-tree:delete! d)
        (vector-set! (cdr del-vector) rnd (vector-ref (cdr del-vector) (- (car del-vector) 1)))
        (set-car! del-vector (- (car del-vector) 1))
        #f)))

(define left-to-right #f)
(define right-to-left #t)

(cond (left-to-right
       (newline)
       (b-tree:find! d "Ariel")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Callisto")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Deimos")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Dione")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Enceladus")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Europa")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Ganymedes")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Hyperion")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:find! d "Io")
       (b-tree:delete! d)
       (b-tree:print d)
       (b-tree:set-current-to-first! d)
       (let loop
         ((p (b-tree:peek d)))
         (display "delete:")(display p)(newline)
         (b-tree:delete! d)
         (when (not (eq? (b-tree:set-current-to-first! d) no-current))
           (loop (b-tree:peek d))))
       (newline))
      (right-to-left
         (newline)
       (b-tree:find! d "Oberon")(display (list "delete Oberon"))
       (b-tree:delete! d)(b-tree:print d)
       (b-tree:find! d "Callisto")(display (list "delete Callisto"))
       (b-tree:delete! d)
       (b-tree:find! d "Janus")(display (list "delete Janus"))
       (b-tree:delete! d)
       (b-tree:find! d "Ariel")(display (list "delete Ariel"))
       (b-tree:delete! d)(b-tree:print d)
       (b-tree:find! d "Triton")(display (list "delete Triton"))
       (b-tree:delete! d)
       (b-tree:find! d "Maan")(display (list "delete Maan"))
       (b-tree:delete! d)(b-tree:print d)
       (b-tree:find! d "Japetus")(display (list "delete Japetus"))
       (b-tree:delete! d)
       (b-tree:find! d "Phobos")(display (list "delete Phobos"))
       (b-tree:delete! d)
       (b-tree:find! d "Io")(display (list "delete Io"))
       (b-tree:delete! d)
       (b-tree:find! d "Rhea")(display (list "delete Io"))
       (b-tree:delete! d)(b-tree:print d)
     )
      (else 
       (do ()
         ((del-random?))))) 

(b-tree:flush! d)

(b-tree:print d)

(b-tree:drop! d)

;(print-file d)
(display "DONE")
(if (not (= disk-size (fs:df dsk)))
    (error "Memory leackage!" disk-size (fs:df dsk)))

