#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d stack linked) stack:))

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






