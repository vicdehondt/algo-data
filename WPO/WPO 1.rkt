; Oefening 1
; (cons: any any -> pair)
; (car: pair -> any)
; (cdr: pair -> any)
; (vector-ref: vector number -> any)
; (vector-set!: vector number any -> 0)
; (member: any pair -> pair U {#f})

; (map: (any -> any) pair -> pair)
; (sum: (number -> number) (number -> number) number number -> number)
; (compose: (any -> any) (any -> any) -> (any -> any))


; Oefening 8
(define (all-but-first-n l n)
  (define (iterate current counter)
    (if (or (= counter 0) (null? current))
        current
        (iterate (cdr current) (- counter 1))))
  (iterate l n))

; Oefening 6
(define (last-of-list l)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst) (car lst))))
  (iter l '()))

(define (last-of-vector v)
  (let ((l (- (vector-length v) 1)))
    (if (zero? l)
        '()
        (vector-ref v l))))