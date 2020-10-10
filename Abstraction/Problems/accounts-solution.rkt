;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname accounts-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment abstraction-p9)
(@cwl ??? ???)


;; =================
;; Data definitions:

(@htdd Accounts)
(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such
;;   that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


(@problem 1)
;; Design an abstract function (including signature, purpose, and tests) 
;; to simplify the remove-debtors and remove-profs functions defined below.
;;
;; Now re-define the original remove-debtors and remove-profs functions 
;; to use your abstract function. Remember, the signature and tests should 
;; not change from the original functions.


(@htdf remove-acts)
(@signature (Accounts -> Boolean) Accounts -> Accounts)
;; remove accounts that satisfy pred predicate
(check-expect (local [(define (yes? act) true)]
                (remove-acts yes? false))
              false)
(check-expect (local [(define (yes? act) true)]
                (remove-acts yes? ACT42))
              false)
(check-expect (local [(define (no? act) false)]
                (remove-acts no? ACT42))
              ACT42)

(check-expect (local [(define (debtor? act)
                        (negative? (node-bal act)))]
                (remove-acts debtor?
                             (make-node 4 "Mrs. Doubtfire" -3
                                        false 
                                        (make-node 7 "Mr. Natural" 13
                                                   false false))))
              (make-node 7 "Mr. Natural" 13 false false))

(@template Accounts)

(define (remove-acts pred act)
  (cond [(false? act) false]
        [else
         (if (pred act) 
             (join (remove-acts pred (node-l act))
                   (remove-acts pred (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-acts pred (node-l act))
                        (remove-acts pred (node-r act))))]))


(@htdf remove-debtors)
(@signature Accounts -> Accounts)
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))

(@template use-abstract-fn)

(define (remove-debtors act)
  (local [(define (debtor? act) (negative? (node-bal act)))]
    (remove-acts debtor? act)))


(@htdf remove-profs)
(@signature Accounts -> Accounts)
;; remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false))
              false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60
                                                  false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))

(@template use-abstract-fn)

(define (remove-profs act)
  (local [(define (prof? act) (has-prefix? "Prof." (node-name act)))]
    (remove-acts prof? act)))


(@htdf has-prefix?)
(@signature String String -> Boolean)
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(@template String)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))


(@htdf join)
(@signature Accounts Accounts -> Accounts)
;; Combine two Accounts's into one
;; ASSUME: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(@template Accounts)

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


(@problem 2)
;; Using your new abstract function, design a function that removes from a given
;; BST any account where the name of the account holder has an odd number of
;; characters.  Call it remove-odd-characters.


(@htdf remove-odd-characters)
(@signature Accounts -> Accounts)
;; Remove accounts where the holder's name has an odd number of characters

(check-expect (remove-odd-characters (make-node 7 "Miss Daisy" 12 false false))
              (make-node 7 "Miss Daisy" 12 false false))
(check-expect (remove-odd-characters
               (make-node 7 "Mrs. Robinson" 12 false false))
              false)

(@template use-abstract-fn)

(define (remove-odd-characters act)
  (local [(define (odd-chars? act) (odd? (string-length (node-name act))))]
    (remove-acts odd-chars? act)))


(@problem 3)
;; Design an abstract fold function for Accounts called fold-act. 
;;
;; Use fold-act to design a function called charge-fee that decrements
;; the balance of every account in a given collection by the monthly fee of 3
;; CAD.


(@htdf fold-act)
(@signature (Natural String Integer Z Z -> Z) Z Accounts -> Z)
;; the fold function for Accounts.
(check-expect (fold-act make-node false ACT10) ACT10)

(@template Accounts)

(define (fold-act c b t)
  (cond [(false? t) b]
        [else
         (c (node-id t)
            (node-name t)
            (node-bal t)
            (fold-act c b (node-l t))
            (fold-act c b (node-r t)))]))


(@htdf charge-fee)
(@signature Accounts -> Accounts)
;; Deduct 3 CAD monthly fee from every account.
(check-expect (charge-fee 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 97 "Prof. X" 4
                         false 
                         (make-node 112 "Ms. Magazine" 464 false false)))

(@template use-abstract-fn)

(define (charge-fee act)
  (local [(define (c id name balance rl rr)
            (make-node id name (- balance 3) rl rr))
          (define b false)]
    (fold-act c b act)))


(@problem 4)
;; Suppose you needed to design a function to look up an account based on its
;; ID. Would it be better to design the function using fold-act, or to design
;; the function using the fn-for-acts template?  Briefly justify your answer.

;; It is better to design the search function using fn-for-acts template, since
;; using fold-act would make it traverse the whole tree every time, even
;; if it finds what it is looking for right away.
