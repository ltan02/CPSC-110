;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lab-11-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Graphs Lab

(@assignment lab-11)
(@problem 1)
(@cwl atan12)  ;; !!! REPLACE ??? with your cwl to hand this file in

(@htdd Venue)
(define-struct vnu (nm los))
;; Venue is (make-vnu String (listof Streetway))
;; interp. a venue name and streetways leading from that venue

(@htdd Streetway)
(define-struct sw (nm dst))
;; Streetway is (make-sw String Venue)
;; interp. (make-sw s v) is a streetway s that leads to the destination venue v.

(define M0
  (shared ([-AHB- (make-vnu "Anti-Hero 13 Boutique" (list))]   
           [-RR- (make-vnu "Rocket Reprographics" 
                         (list (make-sw "Cordova Eastbound" -AHB-)))]
           [-VFS- (make-vnu "Vancouver Film School"
                          (list (make-sw "Hastings Westbound" -WTFD-)
                                (make-sw "Homer Northbound" -RR-)))]
           [-WTFD- (make-vnu "WOW Tasty Food Delivery" 
                           (list (make-sw "Hastings Eastbound" -VFS-)))])
    (list -AHB- -RR- -VFS- -WTFD-)))


;; Online students: complete the following as your lab foundation.
;; On-ground students: complete the following as your pre-lab.

;; The incomplete data definition can represent venues and
;; streetways in Vancouver or anywhere else.  Using the given example as
;; a guide, define an example of your own that includes AT LEAST the venues
;; and streetways in the block that spans from Richards to Hamilton and from
;; Dunsmuir to Hastings (there are 9 venues and 17 streetways in that block).
;; The provided examples are encapsulated, so they should not
;; interfere with your definition.  DO NOT REUSE THE GIVEN EXAMPLE TO 
;; DEFINE YOURS.
;;
;; Then complete the data definition by writing a template for the given data
;; definitions.  DO NOT ADD ACCUMULATORS, simply produce the template
;; that corresponds to the type comments.  You may encapsulate them
;; under the name fn-for-map, which takes a venue as its argument.

(define MAP
  (shared ([-AHB- (make-vnu "Anti-Hero 13 Boutique" (list))]
           [-RR- (make-vnu "Rocket Reprographics"
                           (list (make-sw "Cordova Eastbound" -AHB-)))]
           [-SNC- (make-vnu "Shine Night Club"
                            (list (make-sw "Cordova Eastbound" -RR-)
                                  (make-sw "Richards Southbound" -WTFD-)
                                  (make-sw "Cordova Westbound" -WCE-)))]
           [-WCE- (make-vnu "West Coast Express"
                            (list (make-sw "Cordova Eastbound" -SNC-)))]
           [-VL- (make-vnu "Vancouver Lookout"
                           (list (make-sw "Seymour Northbound" -WCE-)
                                 (make-sw "Hastings Eastbound" -WTFD-)))]
           [-WTFD- (make-vnu "WOW Tasty Food Delivery"
                             (list (make-sw "Hastings Westbound" -VL-)
                                   (make-sw "Hastings Eastbound" -VFS-)
                                   (make-sw "Richards Southbound" -MB-)))]
           [-VFS- (make-vnu "Vancouver Film School"
                            (list (make-sw "Hastings Westbound" -WTFD-)
                                  (make-sw "Homer Northbound" -RR-)
                                  (make-sw "Hastings Eastbound" -BMP-)))]
           [-BMP- (make-vnu "BC Marijuana Party"
                            (list (make-sw "Hastings Westbound" -VFS-)
                                  (make-sw "Hamilton Southbound" -LSHA-)))]
           [-FMCP- (make-vnu "F.M Classic Pizza"
                             (list (make-sw "Seymour Northbound" -VL-)
                                   (make-sw "Pender Eastbound" -MB-)))]
           [-MB- (make-vnu "MacLeod's Books"
                           (list (make-sw "Pender Westbound" -FMCP-)
                                 (make-sw "Pender Eastbound" -CTA-)
                                 (make-sw "Richards Southbound" -DSBT-)))]
           [-CTA- (make-vnu "Capital Tax and Accounting"
                            (list (make-sw "Pender Westbound" -MB-)
                                  (make-sw "Homer Northbound" -VFS-)
                                  (make-sw "Pender Eastbound" -LSHA-)))]
           [-LSHA- (make-vnu "London School of Hairdressing and Aesthetics"
                             (list (make-sw "Pender Westbound" -CTA-)
                                   (make-sw "Hamilton Southbound" -CM-)))]
           [-711- (make-vnu "7-11"
                            (list (make-sw "Seymour Northbound" -FMCP-)))]
           [-DSBT- (make-vnu "D&S Bubble Tea"
                             (list (make-sw "Dunsmuir Westbound" -711-)))]
           [-BH- (make-vnu "BC Hydro"
                           (list (make-sw "Dunsmuir Westbound" -DSBT-)
                                 (make-sw "Homer Northbound" -CTA-)))]
           [-CM- (make-vnu "Candy Meister"
                           (list (make-sw "Dunsmuir Westbound" -BH-)
                                 (make-sw "Hamilton Northbound" -LSHA-)))])
    (list -AHB- -RR- -SNC- -WCE- -VL- -WTFD- -VFS- -BMP- -FMCP- -MB- -CTA-
          -LSHA- -711- -DSBT- -BH- -CM-)))

(define (fn-for-map v)
  (local [(define (fn-for-venue v)
            (... (vnu-nm v)
                 (fn-for-losw (vnu-los v))))

          (define (fn-for-streetway sw)
            (... (sw-nm sw)
                 (fn-for-venue (sw-dst sw))))

          (define (fn-for-losw losw)
            (cond [(empty? losw) (...)]
                  [else
                   (... (fn-for-streetway (first losw))
                        (fn-for-losw (rest losw)))]))]
    (fn-for-venue v)))

;; Problem 1:

;; Design a function that, given a venue and the name of a venue IN THAT ORDER,
;; produces true if the named venue can be reached from the given venue.
;; Call it can-get-to?.

(@htdf can-get-to?)
(@signature Venue String -> Boolean)
;; true if the named venue can be reached from the given name
(check-expect (can-get-to? (first MAP) "Rocket Reprographics") false)
(check-expect (can-get-to? (list-ref MAP 3) "Candy Meister") true)

;(define (can-get-to? v s) false) ;stub

(@template Venue Streetway (listof Streetway) accumulator encapsulated)
(define (can-get-to? v s)
  ;; todo is (listof Streetway) ; worklist accumulator
  ;; visited is (listof Venue) ; venues visited so far
  (local [(define (fn-for-venue v todo visited)
            (cond [(string=? (vnu-nm v) s) true]
                  [(member v visited) (fn-for-losw todo visited)]
                  [else
                   (fn-for-losw (append (vnu-los v) todo)
                                (append visited (list v)))]))

          (define (fn-for-streetway sw todo visited)
            (fn-for-venue (sw-dst sw) todo visited))

          (define (fn-for-losw todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-streetway (first todo) (rest todo) visited)]))]
    
    (fn-for-venue v empty empty)))


;; Problem 2:

;; Design a function, called find-route, that given a venue and the name of
;; some other venue IN THAT ORDER, produces a list of names of streetways that
;; can get you to the named venue (i.e a list of driving directions), or false
;; if the named venue cannot be reached from the given venue.

;; Your find-route design does not need to be tail-recursive. But if you finish
;; early, try refactoring your solution so it is! You may want to keep a copy
;; of your non-tail-recursive solution to hand in.
(@problem 2)
(@htdf find-route)
(@signature Venue String -> (listof String) or false)
;; produces a list of names of streetways that can get you to the named venue
(check-expect (find-route (first MAP) "Rocket Reprographics") false)
(check-expect (find-route (second MAP) "Anti-Hero 13 Boutique")
              (list "Cordova Eastbound"))
(check-expect (find-route (list-ref MAP 3) "Vancouver Film School")
              (list "Cordova Eastbound"
                    "Richards Southbound"
                    "Hastings Eastbound"))

;(define (find-route v s) false) ;stub

(@template Venue Streetway (listof Streetway) encapsulated accumulator)
(define (find-route v s)
  ;; visited is (listof Venue) ; venues visited already
  ;; rsf is (listof String) ; the path taken to get to the named venue
  (local [(define (fn-for-venue v visited rsf)
            (cond [(member v visited) false]
                  [(string=? (vnu-nm v) s) rsf]
                  [else
                   (fn-for-losw (vnu-los v)
                                (cons v visited)
                                rsf)]))

          (define (fn-for-losw losw visited rsf)
            (cond [(empty? losw) false]
                  [else
                   (local [(define try (fn-for-streetway (first losw)
                                                         visited
                                                         rsf))]
                     (if (not (false? try))
                         try
                         (fn-for-losw (rest losw) visited rsf)))]))

          (define (fn-for-streetway sw visited rsf)
            (fn-for-venue (sw-dst sw)
                          visited
                          (append rsf (list (sw-nm sw)))))]
                   
    
    (fn-for-venue v empty empty)))



