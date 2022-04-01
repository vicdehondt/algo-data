#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Binary Search Trees                        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (binary-search-tree)
  (export new bst? find insert! delete-symmetrical! empty? full? root)
  (import (prefix (a-d tree binary-tree) tree:)
          (scheme base)
          (scheme write))
  (begin 
    (define-record-type bst
      (make r e l c)
      bst?
      (r root root!)
      (e equality)
      (l lesser)
      (c current current!))

    (define (new ==? <<?)
      (make tree:null-tree ==? <<? '()))
 
    (define (find bst key)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (let find-key 
        ((node (root bst)))
        (if (tree:null-tree? node)
            #f
            (let 
                ((node-value (tree:value node)))
              (cond 
                ((==? node-value key)
                 node-value)
                ((<<? node-value key)
                 (find-key (tree:right node)))
                ((<<? key node-value)
                 (find-key (tree:left node))))))))
 
    (define (insert! bst val)
      (define <<? (lesser bst))
      (let insert-iter 
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           (child! parent 
                   (tree:new val 
                             tree:null-tree 
                             tree:null-tree)))
          ((<<? (tree:value child) val)
           (insert-iter child tree:right! 
                        (tree:right child)))
          ((<<? val (tree:value child))
           (insert-iter child tree:left!
                        (tree:left child)))
          (else 
           (tree:value! child val)))))
 
    (define (delete-symmetrical! bst val)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (define (find-rightmost deleted parent child! child)
        (if (tree:null-tree? (tree:right child))
            (begin 
              (tree:value! deleted (tree:value child))
              (child! parent (tree:left child)))
            (find-rightmost deleted child 
                           tree:right!
                           (tree:right child))))
      (define (delete-node parent child! child)
        (cond 
          ((tree:null-tree? (tree:right child))
           (child! parent (tree:left child)))
          ((tree:null-tree? (tree:left child))
           (child! parent (tree:right child)))
          (else
           (find-rightmost child
                          child 
                          tree:left! 
                          (tree:left child)))))
      (let find-node
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           #f)
          ((==? (tree:value child) val)
           (delete-node parent child! child)
           (tree:value child))
          ((<<? (tree:value child) val)
           (find-node child tree:right! (tree:right child)))
          ((<<? val (tree:value child))
           (find-node child tree:left! (tree:left child))))))
 
    (define (empty? bst)
      (tree:null-tree? (root bst)))
 
    (define (full? bst)
      #f)

    (define (get-current bst)
      (if (has-current? bst)
          (tree:value (car (current bst)))
          (error "Jaja tis een error")))

    (define (has-current? bst)
      (not (null? (current bst))))

    (define (set-current-to-leftmost! bst start-node)
      (let loop ((node start-node))
        (if (not (tree:null-tree? node))
            (begin (current! bst (cons node (current bst)))
                   (loop (tree:left node))))))

    (define (set-current-to-first! bst)
      (current! bst '())
      (set-current-to-leftmost! bst (root bst)))

    (define (set-current-to-next! bst)
      (let loop (())
        (cond
          ((and (tree:null-tree? (tree:right (get-current bst))) (< (tree:value (cadr current)) (tree:value (get-current bst))))
           (current! bst (cddr current)))
          ((tree:null-tree? (tree:right (get-current bst))) (current! bst (cdr current)))
          ((not (tree:null-tree? (tree:right (get-current bst)))) (current! bst (cons (tree:right (get-current bst)) current))
                                                                  (set-current-to-leftmost! bst (get-current bst)))
          (else ()))))))

(define test (make '() = <))
(insert! test 50)
(insert! test 60)
(insert! test 40)
(insert! test 45)
(insert! test 30)
(insert! test 70)
(insert! test 55)
(insert! test 42)
(insert! test 57)