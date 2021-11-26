#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Shortest Path Algorithms Tests                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (scheme base)
 (scheme write)
 (a-d graph-algorithms directed single-source-shortest-path)
 (a-d graph-algorithms directed traclo-weighted)
 (a-d graph examples directed-weighted))

(display "Cormen589")(newline)
(display (list "Bellman-Ford" (bellman-ford cormen589 0)))(newline)
(display (list "Dijkstra    " (dijkstra cormen589 0)))(newline)
(display "Cormen")(newline)
(display (list "Dijkstra    " (dijkstra cormen 0))) (newline)
(display (list "Bellman-Ford" (bellman-ford cormen 0)))(newline)

(display "weighted-dag")(newline)

(display (list "Lawler      " (lawler weighted-dag 1)))(newline)

(display "cormen")(newline)

(display (list "pre-Floyd-Warshall          " (traclo-weighted-exp cormen)))(newline)

(display (list "Floyd-Warshall          " (floyd-warshall cormen)))(newline)
