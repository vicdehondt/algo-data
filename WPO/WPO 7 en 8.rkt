#lang r7rs
(#%require racket/trace)
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (scheme inexact)
        (prefix (a-d stack linked) stack:)
        (prefix (a-d queue list) queue:)
        (prefix (a-d heap standard) heap:))

; Oef 1
(define (postfix-eval expression)
  
  (define s (stack:new))

  (define (make-stack stack expr)
    (if (null? expr)
        stack
        (make-stack (stack:push! stack (car expr)) (cdr expr))))
  
  (let loop ((stack (make-stack s expression))
             (current-op 'nil)
             (result 'nil))
    (let ((element (stack:pop! stack)))
      (cond
        ((stack:empty? stack) (current-op result element))
        ((and (eq? current-op 'nil) (not (number? element))) (loop stack element result))
        ((and (not (eq? current-op element)) (not (number? element))) (loop stack element result))
        ((and (number? element) (eq? result 'nil)) (loop stack current-op element))
        ((number? element) (loop stack current-op (current-op result element)))))))

(postfix-eval (list 5 6 +))

(postfix-eval (list 5 6 + 7 -))


; Oef 2
(define (valid? lst)
  (define (opening-parenthesis? symb)
    (let ((str (symbol->string symb)))
      (and (eq? (string-ref str 0) #\<)
           (eq? (string-ref str (- (string-length str) 1)) #\>)
           (not (eq? (string-ref str 1) #\/)))))
  
  (define (closing-parenthesis? symb)
    (let ((str (symbol->string symb)))
      (and (eq? (string-ref str 0) #\<)
           (eq? (string-ref str (- (string-length str) 1)) #\>)
           (eq? (string-ref str 1) #\/))))

  (define (matches? symbol-a symbol-b)
    (define string-a (symbol->string symbol-a))
    (define string-b (symbol->string symbol-b))
    (and (opening-parenthesis? symbol-a)
         (closing-parenthesis? symbol-b)
         (string=? (substring string-a 2 (- (string-length string-a) 2))
                   (substring string-b 2 (- (string-length string-b) 2)))))

  (let loop ((lst lst)
             (stack (stack:new)))
    (let ((current (car lst))
          (next (cdr lst)))
    (cond
      ((and (null? lst) (stack:empty? stack)) #t)
      ((opening-parenthesis? current) (loop next (stack:push! stack current)))
      ((and (closing-parenthesis? current) (matches? current (stack:pop! stack))) (loop next stack))
      (else #f)))))

; Oef 3
(define (josephus lst m)
  (define q (queue:new))

  (define (make-from-list current-list result)
    (if (null? current-list)
        q
        (make-from-list (cdr current-list) (queue:enqueue! q (car current-list)))))
  
  (let decide-winner ((count 1)
                      (queue (make-from-list lst q)))
    (let ((current (queue:serve! q)))
      (cond
        ((queue:empty? q) current)
        ((eq? m count) (decide-winner 1 q))
        (else (decide-winner (+ count 1) (queue:enqueue! q current)))))))

; Oef 10
(define h (heap:new 6 <))
(heap:insert! h 5)

; 5

(heap:insert! h 2)

;   2
; 5

(heap:insert! h 3)

;   2
; 3   5

(heap:insert! h 1)

;     1
;   2   3
; 5

(heap:insert! h 2)

;       1
;   2       3
; 5   2



