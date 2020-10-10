;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lecture 9 - Notes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; List Abbreviations
(cons "a" (cons "b" (cons "c" empty))) ;what we've been using

(list "a" "b" "c") ;new way of making lists
(list (+ 1 2) (+ 3 4) (+ 5 6))

(define L1 (list "b" "c"))
(define L2 (list "d" "e" "f"))

(cons "a" L1) ;produce new list by adding "a" to front of L1
(list "a" L1) ;produce new list with "a" as first element and L1 as second element

(list L1 L2)

(append L1 L2) ;consumes two list an dcombines its arguements together into a single list

;; List of Account: lookup-in-list-starter.rkt

;; Binary Search Trees
; invariant - true throughout the entire tree

;; Data Definition for Binary Search Trees: bst-dd-starter.rkt
;; Lookup in BSTs: lookup-in-bst-starter.rkt
;; Rendering BST: render-bst-starter.rkt