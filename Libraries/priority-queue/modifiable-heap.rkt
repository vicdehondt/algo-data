#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*         Priority Queues (Modifiable Heap Implementation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (priority-queue)
  (export new priority-queue? enqueue! reschedule! peek serve! full? empty? priority-of)
  (import (scheme base)
          (prefix (a-d heap modifiable) heap:))
  (begin
    (define pq-item-make cons)
    (define pq-item-val car)
    (define pq-item-priority cdr)
    (define pq-item-priority! set-cdr!)
 
    (define (lift func)
      (lambda (item1 item2)
        (func (pq-item-priority item1)
              (pq-item-priority item2))))
 
    (define-record-type priority-queue
      (make h)
      priority-queue?
      (h heap))
 
    (define (new capacity >>?)
      (make (heap:new capacity (lift >>?))))
 
    (define (empty? pq)
      (heap:empty? (heap pq)))
 
    (define (full? pq)
      (heap:full? (heap pq)))
 
    (define (serve! pq notify)
      (if (empty? pq)
          (error "empty priority Queue (serve!)" pq))
      (heap:delete! (heap pq) 
                    (lambda (idx item)
                      (notify 
                       idx
                       (pq-item-val item)
                       (pq-item-priority item)))))
 
    (define (peek pq)
      (if (empty? pq)
          (error "empty priority queue (peek)" pq)
          (heap:peek (heap pq))))
 
    (define (enqueue! pq value priority notify)
      (heap:insert! (heap pq)
                    (pq-item-make value priority) 
                    (lambda (idx item)
                      (notify
                       idx
                       (pq-item-val item)
                       (pq-item-priority item))))
      pq)
 
    (define (reschedule! pq index new-priority notify)
      (define h (heap pq))
      (define pq-item (heap:peek-at h index))
      (pq-item-priority! pq-item new-priority)
      (heap:touch-at! h
                      index (lambda (idx item)
                              (notify 
                               idx
                               (pq-item-val item)
                               (pq-item-priority item)))))
 
    (define (priority-of pq index)
      (define h (heap pq))
      (pq-item-priority (heap:peek-at h index)))))