;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lec11-bst-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

;; bst-starter.rkt

(define TEXT-SIZE 60)
(define TEXT-COLOR "black")

;; Data definitions:

(@htdd BST)
(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

;; An example tree is at
;; https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lecture/lec11-bst.png

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))


(@problem 1)
#|
PROBLEM: Complete the data definition.  Include example constants for nodes 27,
42, and 10. Also write a template. As of module 6A you are no longer required
to have a @dd-template-rules tag.  But you must still follow the rules!
|#


;; Functions:

(@problem 2)
#|
PROBLEM: Design a function that consumes a bst and produces a SIMPLE
rendering of that bst. Emphasis on SIMPLE. You might want to skip the lines 
for example.
|#


(@problem 3)
#|
PROBLEM: Design a function that consumes a bst and a key and produces the 
val associated with that key or false if the key does not exist.
|#


(@problem 4)
#|
PROBLEM: Design a function that consumes a bst and counts the number of 
nodes in the tree.
|#


(@problem 5)
#|
PROBLEM: Design a function that consumes a bst and counts the number of
nodes in the tree that have an odd key.(Hint - odd? predicate exists in BSL)
|#