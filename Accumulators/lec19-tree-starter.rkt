;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec19-tree-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; Design a function that given a tree and a name tries to find a path to a
;; node with that name.
;;

(@htdf find-path)

(@signature Tree String -> (listof String) or false)
;; produce path to given node name if found
(check-expect (find-path L1 "L1") (list "L1"))
(check-expect (find-path L1 "foo") false)
(check-expect (find-path TOP "L3") (list "TOP" "M2" "L3"))

;(define (find-path t nm) false)

(@template Tree backtracking accumulator encapsulated)
#;
(define (find-path t0 nm)
  ;; path: (listof String) ; the current path that is taken
  ;; (find-path TOP "L3")
  ;;
  ;; (fn-for-t TOP empty)
  ;; (fn-for-t M1 (list "TOP"))
  ;; (fn-for-t L1 (list "TOP" "M1"))
  ;; (fn-for-t M2 (list "TOP"))
  ;; (fn-for-t L2 (list "TOP" "M2"))
  ;; (fn-for-t L3 (list "TOP" "M2"))
  
  (local [(define (fn-for-t t path)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              (if (string=? nm (node-name t))
                  (append path (list name))
                  (fn-for-lot subs (append path (list name))))))
          
          (define (fn-for-lot lot path)
            (cond [(empty? lot) false]
                  [else
                   (local [(define try (fn-for-t (first lot) path))]
                     (if (not (false? try))
                         try
                         (fn-for-lot (rest lot) path)))]))]
    
    (fn-for-t t0 empty)))


;;
;; PROBLEM
;;
;; Define a new version of the function which is tail recursive.  You only
;; need a new function definition. Proceed by
;;   - copying your solution to the problem above
;;   - and renaming the path accumulator to visited
;;   - then convert the code to be tail recursive
;;

(define (find-path t0 nm)
  ;; visited: (listof String) ; name of trees to here
  ;; todo: (listof WLE); worklist
  ;; path: (listof String); naem of grandparents, parents, ... in the tree
  
  (local [(define-struct wle (t path))
          ;; WLE is (make-wle Tree (listof String))
          ;; interp. an entry in the worklist, saying that fn-for-t
          ;; should call t with path path
          
          (define (fn-for-t t todo visited path)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              (if (string=? nm name)
                  (append path (list name))
                  (fn-for-lot (append (map (λ (s)
                                             (make-wle s
                                                       (append path
                                                               (list name))))
                                           subs)
                                      todo)
                              (append visited (list name))))))
          
          (define (fn-for-lot lot visited)
            (cond [(empty? lot) false]
                  [else
                   (fn-for-t (wle-t (first lot))
                             (rest lot)
                             visited
                             (wle-path (first lot)))]))]
    
    (fn-for-t t0 empty empty empty)))

