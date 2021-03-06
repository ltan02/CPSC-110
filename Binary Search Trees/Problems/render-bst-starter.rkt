;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render-bst-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment bsts-l4)
(@cwl ??? ???)

;; Consider the following data definition for a binary search tree: 


;; =================
;; Data definitions:

(@htdd BST)
(define-struct node (key val l r))
;; BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))



;; =================
;; Functions:

(define FONT-SIZE 12)
(define COLOR "black")

(@problem 1)
;; Design a function that consumes a bst and produces a SIMPLE 
;; rendering of that bst. Emphasis on SIMPLE. You might want to 
;; skip the lines for example.
;;
;; Here is an image of BST10 for reference. Use the link to view it:
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/bst10.png

(@htdf render)
(@signature BST -> Image)
;; produces a simple rendering of the BST
(check-expect (render BST0) empty-image)
(check-expect (render BST1) (above (text "1:abc" FONT-SIZE COLOR)
                                   (beside empty-image empty-image)))
(check-expect (render BST3)
              (above (text "3:ilk" FONT-SIZE COLOR)
                     (beside (text "1:abc" FONT-SIZE COLOR)
                             (text "4:dcj" FONT-SIZE COLOR))
                     (beside empty-image
                             empty-image
                             empty-image
                             (text "7:ruf" FONT-SIZE COLOR))))

(define (render t) empty-image)



