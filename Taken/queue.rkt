#lang r7rs
(define-library (queue)
  (export new queue? enqueue! serve! peek full? empty?)
  (import (scheme base)
          (scheme write))
  (begin
   
    (define default-size 5)
    (define-record-type queue
      (make s h r)
      queue?
      (s storage storage!) ;; Er wordt nu ook een storage! gebruikt om de grote van de vector van de queue aan te passen.
      (h head head!)
      (r rear rear!))
 
    (define (new)
      (make (make-vector default-size) 0 0))
 
    (define (empty? q)
      (= (head q)
         (rear q)))
 
    (define (full? q)
      (= (remainder (+ (rear q) 1) (vector-length (storage q))) ;; Deel door de lengte van de vector ipv de default-size.
         (head q)))
 
    (define (enqueue! q val)
      (if (full? q)
          (if (< (rear q) (head q)) ;; Wanneer de vector vol is en bijgevuld is langs voor.
              (begin
                (storage! q (vector-append (vector-copy (storage q) (head q) (vector-length (storage q))) ;; Append de vector van [de head tot het einde van de vector[
                                           (vector-copy (storage q) 0 (rear q))                           ;; met de vector van [plaats 0 tot de rear[
                                           (vector val)                                                   ;; en de nieuwe value die met enqueue! was meegegeven
                                           (make-vector default-size)))                                   ;; en laat de vector groeien met default-size.
                (rear! q (- (vector-length (storage q)) (+ (head q) (rear q))))                           ;; Pas daarna de rear aan naar de nieuwe plaats.
                (head! q 0))                                                                              ;; Omdat de vector gerangschikt is wordt de head terug 0.
              (begin ;; Wanneer de vector vol is en de head nog voor de rear staat.
                (storage! q (vector-append (vector-copy (storage q) (head q) (rear q))                          ;; Append de vector van [de head tot de rear[
                                           (vector val)                                                         ;; met de nieuwe value die met enqueue! was meegegeven
                                           (vector-copy (storage q) (+ (rear q) 1) (vector-length (storage q))) ;; en de vector van [de rear + 1 tot het einde van de vector[
                                           (vector-copy (storage q) 0 (head q))                                 ;; en de vector van [plaats 0 tot de head[
                                           (make-vector default-size)))                                         ;; en laat de vector groeien met default-size.
                (rear! q (- (+ (rear q) 1) (head q)))                                                           ;; Pas daarna de rear aan naar de rear - de head, maar toch 1 groter.
                (head! q 0)))                                                                                   ;; Omdat de vector gerangschikt is wordt de head terug 0.
      
          (let ((new-rear (remainder (+ (rear q) 1) (vector-length (storage q))))) ;; Deel door de lengte van de vector ipv de default-size.
            (vector-set! (storage q) (rear q) val)
            (rear! q new-rear)))
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