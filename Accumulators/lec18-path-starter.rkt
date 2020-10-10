;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec18-path-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd Tree)
(define-struct node (name subs))
;; Tree is (make-node String (listof Tree))
;; interp. a bare bones arbitrary arity tree, each node has a name and subs
#;
(define (fn-for-tree t)  
  (local [(define (fn-for-t t)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              
              (... name (fn-for-lot subs))))
          
          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-t (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-t t)))

(define L1 (make-node "L1" empty))
(define L2 (make-node "L2" empty))
(define L3 (make-node "L3" empty))

(define M1 (make-node "M1" (list L1)))
(define M2 (make-node "M2" (list L2 L3)))

(define TOP (make-node "TOP" (list M1 M2)))

;;
;; PROBLEM
;;
;; Complete the design of the all-paths function below.
;;
;; Your function definition should use a context preserving accumulator to
;; represent the path to the current tree.
;;

(@htdf all-paths)

(@signature Tree -> (listof (listof String)))
;; produce all the paths of names in the tree
(check-expect (all-paths L1) (list (list "L1")))
(check-expect (all-paths L2) (list (list "L2")))
(check-expect (all-paths M1) (list (list "M1" "L1")))
(check-expect (all-paths M2) (list (list "M2" "L2") (list "M2" "L3")))
(check-expect (all-paths TOP) (list (list "TOP" "M1" "L1")
                                    (list "TOP" "M2" "L2")
                                    (list "TOP" "M2" "L3")))


(define (all-paths t)
  ;; path is (listof String); the path that is currently taken
  ;; (all-paths TOP)
  ;;
  ;; (fn-for-t TOP empty)
  ;; (fn-for-t M1 (list "TOP"))
  ;; (fn-for-t L1 (list "TOP" "M1"))
  ;; (fn-for-t M2 (list "TOP"))
  ;; (fn-for-t L2 (list "TOP" "M2")
  ;; (fn-for-t L3 (list "TOP" "M2"))
  (local [(define (fn-for-t t path)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              (if (empty? subs)
                  (list (append path (list name)))
                  (fn-for-lot subs (append path (list name))))))
          
          (define (fn-for-lot lot path)
            (cond [(empty? lot) empty]
                  [else
                   (append (fn-for-t (first lot) path)
                           (fn-for-lot (rest lot) path))]))]
    
    (fn-for-t t empty)))


