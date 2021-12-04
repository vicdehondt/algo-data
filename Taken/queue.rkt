#lang r7rs
(#%require racket/trace)
(define-library (queue)
  (export new queue? enqueue! serve! peek full? empty?)
  (import (scheme base)
          (scheme write))
  (begin
   
    (define default-size 5)
    (define-record-type queue
      (make s h r)
      queue?
      (s storage storage!)
      (h head head!)
      (r rear rear!))
 
    (define (new)
      (make (make-vector default-size) 0 0))
 
    (define (empty? q)
      (= (head q)
         (rear q)))
 
    (define (full? q)
      (= (remainder (+ (rear q) 1) (vector-length (storage q)))
         (head q)))
 
    (define (enqueue! q val)
      (if (full? q)
          (if (< (rear q) (head q))
              (begin
                (storage! q (vector-append (vector-copy (storage q) (head q) (vector-length (storage q)))
                                           (vector-copy (storage q) 0 (+ (rear q) 1))
                                           (make-vector default-size)))
                (rear! q (+ (rear q) (- (vector-length (storage q)) (head q))))
                (head! q 0))
              (begin
                (storage! q (vector-append (vector-copy (storage q) (head q) (rear q))
                                           (vector-copy (storage q) (rear q) (vector-length (storage q)))
                                           (vector-copy (storage q) 0 (head q))
                                           (make-vector default-size)))
                (rear! q (- (rear q) (head q)))
                (head! q 0))))
      
      (let ((new-rear (remainder (+ (rear q) 1) (vector-length (storage q)))))
        (vector-set! (storage q) (rear q) val)
        (rear! q new-rear))
      q)
 
    (define (peek q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (vector-ref (storage q) (head q)))
 
    (define (serve! q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (let ((result (vector-ref (storage q) (head q))))
        (head! q (remainder (+ (head q) 1) (vector-length (storage q))))
        result))))

(define test (new))