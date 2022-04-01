#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Queues (Slow Positional List Implementation)          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2018  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (queue)
  (export new queue? enqueue! enqueue-at-head! serve! serve-at-rear! peek peek-at-rear full? empty?)
  (import (prefix (a-d positional-list adt) plist:)
          (except (scheme base) length map for-each ))
  (begin
    
    (define-record-type queue
      (make p)
      queue?
      (p plist))
 
    (define (new)
      (make (plist:new eq?)))
 
    (define (enqueue! q val)
      (define plst (plist q))
      (if (full? q)
          (error "full queue (enqueue!)" q)
          (plist:add-before! plst val)))

    (define (enqueue-at-head! q val)
      (define plst (plist q))
      (if (full? q)
          (error "full queue (enqueue!)" q)
          (plist:add-after! plst val)))
 
    (define (peek q)
      (define plst (plist q))
      (if (= (plist:length plst) 0)
          (error "empty queue (peek)" q))
      (plist:peek plst (plist:last plst)))

    (define (peek-at-rear q)
      (define plst (plist q))
      (if (= (plist:length plst) 0)
          (error "empty queue (peek)" q))
      (plist:peek plst (plist:first plst)))
 
    (define (serve! q)
      (define plst (plist q))
      (define last-position (plist:last plst))
      (if (plist:empty? plst)
          (error "queue empty (pop)" q))
      (let ((val (plist:peek plst last-position)))
        (plist:delete! plst last-position)
        val))

    (define (serve-at-rear! q)
      (define plst (plist q))
      (define first-position (plist:first plst))
      (if (plist:empty? plst)
          (error "queue empty (pop)" q))
      (let ((val (plist:peek plst first-position)))
        (plist:delete! plst first-position)
        val))
 
    (define (empty? q)
      (plist:empty? (plist q)))
 
    (define (full? q)
      (plist:full? (plist q)))))