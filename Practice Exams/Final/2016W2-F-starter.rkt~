;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2016W2-F-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)

(@problem 1)
;; Given the following type comments, provide the templates.
;; It is not necessary to provide template rules. Your template
;; must follow the order of the subclasses in the types comment.


(@htdd Bar)
(define-struct bar (s n))
;; Bar is (make-bar String Natural)


(@htdd Foo)
;; Foo is one of:
;; - false
;; - "summer"
;; - "winter"
;; - Bar
































(@problem 2)
;; Given the following information for a zoo, we need to design data
;; to represent it.
;;
;; Provide a full HtDD except for the templates. 
;;
;; A zoo is located in a city and covers a certain square footage.
;; The zoo contains an arbitrary number of animals where an animal is
;; either a mammal, bird, fish, reptile, amphibian, or an invertebrate.





































(@problem 3)
;; The following signature, purpose, stub, and check-expects for the function
;; count-odd are provided for you to use with Problems 3A, 3B, 3C, and 3D.
;;
;; Read Problems 3A, 3B, 3C and 3D before you start.
;;
;; Note: There is a built-in function (odd? x) with signature Natural -> Boolean
;; which produces true if x is an odd number, and false otherwise.


(@htdf count-odd)
(@signature (listof Natural) -> Natural)
;; produce a count of the number of odd values in lon
(check-expect (count-odd empty) 0)
(check-expect (count-odd (list 3)) 1)
(check-expect (count-odd (list 4 6 10)) 0)
(check-expect (count-odd (list 1 2 3 4 5)) 3)
(check-expect (count-odd (list 2 8 9 10 13 5 3)) 4)

(define (count-odd lon) 0) ;stub



;; Problem 3A:
;;
;; Write a non-tail recursive function definition for count-odd using the    
;; (listof X) template. Include a template tag.
































;; Problem 3B:
;;
;; Write a tail recursive function definition for count-odd using the        
;; (listof X) template. Include a template tag.



































;; Problem 3C:
;;
;; Write a function definition for count-odd using built-in abstract list    
;; functions. Include a template tag.
;;
;; Do NOT use the built-in length function.






















;; Problem 3D:
;;
;; Write a function definition for count-odd using for-each. Include a template
;; tag.








































(@problem 4)
;; Design a function that consumes a natural number n and a binary tree bt and
;; produces a list of all the values stored in the nodes at depth n in the tree.
;; Note that the root is at depth 0 in the tree, its immediate child nodes are
;; at depth 1, etc.
;;
;; You must follow the full recipe for functions that consume two one-of types.


;; =================
;; Data Definitions:

(@htdd Natural)
;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. a natural number

(define N0 0)
(define N1 (add1 N0))
(define N2 (add1 N1))

#;
(define (fn-for-nat n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-nat (sub1 n)))]))


(@htdd BTree)
(define-struct node (val l r))
;; BTree is one of:
;; - false
;; - (make-node String BTree BTree)
;; interp.  a binary tree with strings as node values

(define BT0 false)
(define BT1 (make-node ""
                       (make-node "a" false false)
                       (make-node "b" false false)))
(define BT2 (make-node ""
                     (make-node "a" 
                              (make-node "aa" false false)
                              (make-node "ab" false false))
                     (make-node "b"
                              (make-node "ba" false false)
                              (make-node "bb" false false))))
(define BT3 (make-node ""
                     (make-node "a" 
                              (make-node "aa" false false)
                              (make-node "ab" false false))
                     (make-node "b"
                              false
                              (make-node "bb" false false))))


(define (fn-for-bt bt)
  (cond [(false? bt) (...)]
        [else
         (... (node-val bt)
              (fn-for-bt (node-l bt))
              (fn-for-bt (node-r bt)))]))



;; =================
;; Functions:













































































(@problem 5)
;; Pix is a social network for photographers that allows them to
;; share photos, like photos taken by other photographers and
;; follow other photographers.  Each node in the network is a
;; photographer with screen name, number of likes, number of photos, 
;; and a list of other photographers they are following.
;;
;; Design a function most-active that consumes a photographer p
;; and a number of likes n.  The function must produce the name
;; of the photographer in p's network, with at least n likes,
;; that follows the largest number of other photographers.  Note
;; that by a "photographer in p's network" we mean that there is
;; a path from p through the graph to that photographer.
;;
;; You may use the built-in length function that consumes a list
;; and produces the number of items in that list.  If there is
;; more than one photographer in the network that satisfies the
;; given criteria, you can produce the name of any one of those
;; photographers.
;;
;; Your function must be tail-recursive.  Your check-expects must
;; use photographers in the example network provided below and no
;; others.  A picture of the network PN defined in the examples
;; below follows:
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/pn.PNG


(@htdd Photographer)
(define-struct phtgr (name n-photos n-likes following))
;; Photographer is (make-photo String Natural Natural (listof Photographer))
;; interp. a photographer having a name, number of photos, number
;; of likes and list of other photographers that they are following.

(define PN
  (shared ((-A- (make-phtgr "Snappy" 10 10 (list -B- -E-)))
           (-B- (make-phtgr "Pixie" 10 15 (list -A- -C- -D-)))
           (-C- (make-phtgr "Pano" 4 20 (list -B- -D-)))
           (-D- (make-phtgr "Selfie" 12 20 (list -C-)))
           (-E- (make-phtgr "DarkSky" 5 50 (list ))))
    (list -A- -B- -C- -D- -E-)))

(define SNAPPY (first PN))
(define PIXIE (first (rest PN)))
(define PANO (first (rest (rest PN))))
(define SELFIE (first (rest (rest (rest PN)))))
(define DARKSKY (first (rest (rest (rest (rest PN))))))

(@template Photographer (listof Photographer)) ;no path, visited or worklist yet

(define (fn-for-phtgr p0)
  (local [(define (fn-for-phtgr p)
            (... (phtgr-name p)
                 (phtgr-n-photos p)
                 (phtgr-n-likes p)
                 (fn-for-lop (phtgr-following p))))
          
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-phtgr (first todo))
                        (fn-for-lop (rest todo)))]))]
    
    (fn-for-phtgr p0)))
#;
(define (fn-for-phtgr p0)
  (local [;; todo is (listof Photographer)
          ;; a worklist accumulator
          (define (fn-for-phtgr p todo)
            (... (phtgr-name p)
                 (phtgr-n-photos p)
                 (phtgr-n-likes p)
                 (fn-for-lop (append (phtgr-following p) todo))))
          
          (define (fn-for-lop todo)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-phtgr (first todo) (rest todo))]))]
    (fn-for-phtgr p0 empty)))




















































































