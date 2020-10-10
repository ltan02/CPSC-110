;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname contains-key-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p12)
(@cwl ??? ???)

(@problem 1)
;; Starting with the following data definition for a binary tree
;; (not a binary search tree) design a tail-recursive function called contains?
;; that consumes a key and a binary tree and produces true if the tree contains
;; the key.


(@htdd BT)
(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))

(@htdf contains?)
(@signature Integer BT -> Boolean)
;; true if the tree contains the key
(check-expect (contains? 1 BT1) false)
(check-expect (contains? 1 BT2) true)
(check-expect (contains? 4 BT2) true)
(check-expect (contains? 7 BT2) true)
(check-expect (contains? 10 BT2) false)

;(define (contains? k bt) false) ;stub

(define (contains? k bt0)
  ;; todo is (listof BT) ; worklist accumulator
  (local [(define (solve--bt bt todo)
            (cond [(false? bt) (solve--lobt todo)]
                  [(= (node-k bt) k) true]
                  [else
                   (solve--lobt (append (list (node-l bt))
                                        (list (node-r bt))
                                        todo))]))

          (define (solve--lobt todo)
            (cond [(empty? todo) false]
                  [else
                   (solve--bt (first todo)
                              (rest todo))]))]
    (solve--bt bt0 empty)))
