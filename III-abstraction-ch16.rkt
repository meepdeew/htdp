;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname III-abstraction-ch16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
;;;; 16 Using Abstractions

(require 2htdp/image)
(require 2htdp/universe)

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-list n f) == (list (f 0) ... (f (- n 1)))
; (define (build-list n f) ...)

; [X] [X -> Boolean] [List-of X] -> [List-of X]
; produces a list from those items on lx for which p holds 
; (define (filter p lx) ...)

; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; produces a version of lx that is sorted according to cmp
; (define (sort lx cmp) ...)

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))
; (define (map f lx) ...)

; [X] [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for every item on lx
; (andmap p (list x-1 ... x-n)) == (and (p x-1) ... (p x-n))
; (define (andmap p lx) ...)

; [X] [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for at least one item on lx
; (ormap p (list x-1 ... x-n)) == (or (p x-1) ... (p x-n))
; (define (ormap p lx) ...)

;;;; 16.1 Existing Abstractions

(check-expect (build-list 3 add1) '(1 2 3))

(check-expect (filter odd? (list 1 2 3 4 5))
              '(1 3 5))

(check-expect (sort (list 3 2 1 4 5) >)
              '(5 4 3 2 1))

(check-expect (map add1 (list 1 2 2 3 3 3))
              '(2 3 3 4 4 4))

(check-expect (andmap odd? (list 1 2 3 4 5))
              #false)

(check-expect (ormap odd? (list 1 2 3 4 5))
              #true)

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))
; (define (foldr f b lx) ...)

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from left to right to each item in lx and b
; (foldl f b (list x-1 ... x-n)) == (f x-n ... (f x-1 b))
; (define (foldl f b lx) ...)

;; Exercise 256

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i,
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
; (define (argmax f lx) ...)

;; argmax goes through each item X in lx, and calls f using that X as an argument.
;; Once it is done, it returns to caller the X that served as input for the greatest
;; output from function f.

(check-expect (argmax add1 '(1 2 3 4)) 4)
(check-expect (argmax sin '(1 2 3 4)) 2)

;; For argmin, everything would be the same but returning the input producing
;; the quantitatively smallest output.

(define-struct address [first-name last-name street])
; An Addr is a structure:
;   (make-address String String String)
; interpretation associates an address with a person's name

; [List-of Addr] -> String
; creates a string from first names,
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing l)
  (foldr string-append-with-space " "
         (sort (map address-first-name l) string<?)))

; String String -> String
; appends two strings, prefixes with " "
(define (string-append-with-space s t)
  (string-append " " s t))

(define ex0
  (list (make-address "Robert"   "Findler" "South")
        (make-address "Matthew"  "Flatt"   "Canyon")
        (make-address "Shriram"  "Krishna" "Yellow")))

(check-expect (listing ex0) " Matthew Robert Shriram ")

;; Exercise 257

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

; [X] N [N -> X] -> [List-of X]
; build-l*st works just like build-list
(check-expect (build-l*st 3 add1)
              (build-list 3 add1))
;; (check-expect (build-l*st ...)
;;               (build-list ...))
;; (define (build-l*st n f)
;;   (cond [(<= n 0) '()]
;;         [else
;;          (add-at-end
;;           (build-l*st (- n 1) f)
;;           (f (- n 1)))]))

(check-expect
 (add-at-end (cons "c" (cons "b" '())) "a")
 (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond [(empty? l) (list s)]
        [else (cons (first l)
                    (add-at-end (rest l) s))]))

;; or could 
(define (build-l*st n f)
  (reverse (build-l*st-helper n f)))

(define (build-l*st-helper n f)
  (cond [(<= n 0) '()]
        [else (cons (f (- n 1))
                    (build-l*st-helper (- n 1) f))]))


;;;; 16.2 Local Definitions

;; (local (def ...)
;;   body-expression)

; [List-of Addr] -> String
; creates a string of first names,
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing.v2 l)
  (local (; 1. extract names
          (define names (map address-first-name l))
          ; 2. sort the names
          (define sorted (sort names string<?))
          ; 3. append them, add spaces
          ; String String -> String
          ; appends two strings, prefix with " "
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

;; ... (local ((define names  ...)
;;             (define sorted ...)
;;             (define concat+spaces
;;               (local (; String String -> String
;;                       (define (helper s t)
;;                         (string-append " " s t)))
;;                 (foldr helper " " sorted))))
;;       concat+spaces) ...


; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
          
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

;; Exercise 258

; Image Polygon -> Image
; adds an image of p to MT
(define (render-polygon img p)
  (local ((define MT (empty-scene 50 50))
          ; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) MT]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))

          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
          ;
          (define connected-dots (connect-dots img p))
          (define rendered-line (render-line connected-dots (first p) (last p))))
    rendered-line))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

;; Exercise 259

;; Doing just arrangements so as not to worry about in-dictionary, etc.
;; (define (alternative-words s)
;;   (in-dictionary
;;     (words->strings (arrangements (string->word s)))))
;; but it would look something like:
;; (define (alternative-words s)
;;   (local ((define as-word (string->word s))
;;           (define all (arrangements as-word))
;;           (define as-strings (word->string all))
;;           (matches (in-dictionary as-strings)))
;;     matches))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (local ((define (insert-at-end item listo)
            (reverse (cons item (reverse listo))))
          ; 1String Word Word ->  [Listof Words]
          (define (insert-in-word letter w-left w-used)
            (cond
              [(empty? w-left) (list (append w-used (list letter)))]
              [else
               (append (list (append w-used (list letter) w-left))
                       (insert-in-word letter (rest w-left)
                                       (insert-at-end (first w-left) w-used)))]))
          ; String Word -> [Listof Words]
          (define (insert-everywhere/in-this-word letter w)
            (insert-in-word letter w (list)))
          ; 1String List-of-words -> [Listof Words]
          (define (insert-everywhere/in-all-words letter w)
            (cond [(empty? w) '()]
                  [else (append (insert-everywhere/in-this-word letter (first w))
                                (insert-everywhere/in-all-words letter (rest w)))])))
    (cond [(empty? w) (list '())]
          [else (insert-everywhere/in-all-words
                 (first w) (arrangements (rest w)))])))


;; Exercise 260

; [Listof Number] -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

(check-expect
 (inf.v2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
               12 11 10 9 8 7 6 5 4 3 2 1))
 1)

(check-expect
 (inf.v2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
               17 18 19 20 21 22 23 24 25))
 1)
; Still quick

;; Exercise 261

(define-struct ir [price name])

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define cached-expr (extract1 (rest an-inv))))
       (cond [(<= (ir-price (first an-inv)) 1.0)
              (cons (first an-inv) cached-expr)]
             [else cached-expr]))]))

;; This does not help increase the speed at which the function computes its result at all.
;; The recursive computation, with and without the local expression, procedes exactly
;; once per function body, independent of execution of path.

;;;; 16.3 Local Definitions Add Expressive Power

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State FSM-State -> Boolean
(define (state=? in1 in2)
  (and (image-color? in1) (image-color? in2)
       (string=? (string-downcase in1)
                 (string-downcase in2))))

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate fsm s0)
  (local (
          (define (find-next-state s key-event)
            (find fsm s)))
    (big-bang s0
              [to-draw state-as-colored-square]
              [on-key find-next-state])))

; FSM-State -> Image
; renders current state as colored square
(define (state-as-colored-square s)
  (square 100 "solid" s))

; FSM FSM-State -> FSM-State
; finds the current state in fsm
(define (find transitions current)
  (cond
    [(empty? transitions) (error "not found")]
    [else
     (local ((define s (first transitions)))
       (if (state=? (transition-current s) current)
           (transition-next s)
           (find (rest transitions) current)))]))

;; (simulate (list (make-transition "red" "green")
;;                 (make-transition "green" "yellow")
;;                 (make-transition "yellow" "red"))
;;           "red")

;; Exercise 262

(check-expect (identityM 1)
              (list (list 1)))

(check-expect (identityM 2)
              (list (list 1 0)
                    (list 0 1)))

(check-expect (identityM 3)
              (list (list 1 0 0)
                    (list 0 1 0)
                    (list 0 0 1)))

; Number -> [Listof [Listof Number]]
(define (identityM n)
  (matrix-helper 1 n))

(check-expect (matrix-helper 1 1)
              (list (list 1)))
(check-expect (matrix-helper 1 2)
              (list (list 1 0)
                    (list 0 1)))
(check-expect (matrix-helper 1 3)
              (list (list 1 0 0)
                    (list 0 1 0)
                    (list 0 0 1)))

; Number Number -> [Listof [Listof Number]]
(define (matrix-helper cur n)
  (cond [(> cur n) '()]
        [else (cons (row-gen cur n)
                    (matrix-helper (add1 cur) n))]))

(check-expect (row-gen 1 1) (list 1))
(check-expect (row-gen 1 2) (list 1 0))
(check-expect (row-gen 2 2) (list 0 1))
(check-expect (row-gen 1 3) (list 1 0 0))
(check-expect (row-gen 2 3) (list 0 1 0))
(check-expect (row-gen 3 3) (list 0 0 1))
(check-expect (row-gen 1 4) (list 1 0 0 0))

(define (zeros-except j)
  (local ((define (current-1? i)
            (if (= (add1 i) j) 1 0)))
    current-1?))

(define (row-gen x y)
  (build-list y (zeros-except x)))

;;;; 16.4 Computing with local

;; Exercise 263: In DrRacket, step through:

(check-expect
 (inf.v2 (list 2 1 3))
 1)

;; Exercise 264: In DrRacket, step through:

; [Listof Number] -> Number
; determines the largest number on l
(define (sup.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define largest-in-rest (sup.v2 (rest l))))
       (if (> (first l) largest-in-rest)
           (first l)
           largest-in-rest))]))

(check-expect
 (sup.v2 (list 2 1 3))
 3)

;; Exercise 265 (Need to move from Intermediate Student --> Intermediate Student with Lambda)

(check-expect
 ((local ((define (f x) (+ (* 4 (sqr x)) 3))) f)
  1)
 7)

;; Exercise 266

(check-expect 
 ((local ((define (f x) (+ x 3))
          (define (g x) (* x 4)))
    (if (odd? (f (g 1)))
        f
        g))
  2)
 5)

;;;; 16.5 Using Abstractions, by Example

;; _Sample Problem_: Design add-3-to-all.
;; The function consumes a list of Posns and adds 3 to the x-coordinates of each.

(check-expect
 (add-3-to-all (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))

;; [Listof Posn] -> [Listof Posn]
;; adds 3 to the x-coordinates of each in a list of Posns
(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; add 3 to the x-coordinate of individual Posn
          (define (add-3-to-1 p)
            (make-posn (+ 3 (posn-x p))
                       (posn-y p))))
    (map add-3-to-1 lop)))

;; _Sample Problem_: Design a function that eliminates all Posns with
;; y-coordinates larger than 100 from some given list.


(check-expect
 (keep-good (list (make-posn 0 110) (make-posn 0 60)))
 (list (make-posn 0 60)))

;; [Listof Posn] -> [Listof Posn]
;; eliminates Posns whose y-coordinate is > 100
(define (keep-good lop)
  (local (; Posn -> Boolean
          ; return false if y-coordinate of p is > 100
          (define (good? p)
            (not (> (posn-y p) 100))))
    (filter good? lop)))

;; _Sample Problem_: Design a function that determines whether
;; any of a list of Posns is close to some given position pt
;; where “close” means a distance of at most 5 pixels.

(define CLOSENESS 5)

; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(define (close-to p q d)
  (local ((define (within-distance a b)
            (< (abs (- a b)) 5)))
    (and (within-distance (posn-x p) (posn-x q))
         (within-distance (posn-y p) (posn-y q)))))

(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60))
         (make-posn 50 50))
 #true)
(check-expect
 (close? (list (make-posn 44 56) (make-posn 0 60))
         (make-posn 50 50))
 #false)

; [Listof Posn] Posn -> Boolean
; is any Posn on lop close to pt
(define (close? lop pt)
  (local ((define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))

;;;; 16.6 Designing with Abstractions

(define DOT (circle 3 "solid" "red"))
(define MT-SCENE (empty-scene 100 100))

(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))
; [Listof Posn] -> Image
; adds the Posns on lop to the empty scene
(define (dots lop)
  (local (; Posn Image -> Image
          ; adds a DOT at p to Scene
          (define (add-one-dot p scene)
            (place-image DOT
                         (posn-x p) (posn-y p)
                         scene)))
    (foldr add-one-dot MT-SCENE lop)))

;;;; 16.7 Finger Exercises: Abstraction

;; Exercise 267.

(define CONVERSION 1.06)

(check-expect (convert-euro (list 1.06 2.12))
              (list 1.00 2.00))

; [Listof Number] -> [Listof Number]
; Converts a list of US$ amounts into a list of € amounts
; based on an exchange rate of US$1.06 per € (on April 13, 2017).
(define (convert-euro lon)
  (local (; Number -> Number
          ; convert a single euro amount to usd
          (define (to-dollars euro)
            (* euro (/ 1 CONVERSION))))
    (map to-dollars lon)))

(check-expect (convertFC (list 32 212))
              (list 0 100))

; [Listof Number] -> [Listof Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements.
(define (convertFC lon)
  (local (; Number -> Number
          ; convert a single fahrenheit measure to celsius
          (define (f->c f)
            (* (- f 32) (/ 5 9))))
    (map f->c lon)))

(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))

; [Listof Posn] -> [Listof [Listof Number]]
; translates a list of Posns into a list of lists of pairs of numbers.
(define (translate lop)
  (local (; Posn -> [Listof Number]
          ; convert a Posn structure to a list
          (define (posn->pair p)
            (list (posn-x p) (posn-y p))))
    (map posn->pair lop)))

;; Exercise 268

; Inventory
(define-struct inventory
  [item-name
   item-description
   acquisition-price
   sales-price])

(check-expect (inv-sort (list (make-inventory "1" "1" 4 2)
                              (make-inventory "2" "2" 4 3)
                              (make-inventory "3" "3" 4 4)))
              (list (make-inventory "3" "3" 4 4)
                    (make-inventory "2" "2" 4 3)
                    (make-inventory "1" "1" 4 2)))

; [Listof Inventory] -> [Listof Inventory]
; sorts a list of inventory records by the difference between the two prices. 
(define (inv-sort loi)
  (local (; Inventory Inventory -> Boolean
          ; whether the difference in the two prices for a given
          ; inventory is less for inv1 than for inv2
          (define (inv-cmp inv1 inv2)
            (< (- (inventory-acquisition-price inv1) (inventory-sales-price inv1))
               (- (inventory-acquisition-price inv2) (inventory-sales-price inv2)))))
    (sort loi inv-cmp)))

;; Exercise 269

(check-expect (eliminate-expensive 4 (list (make-inventory "1" "1" 4 2)
                                           (make-inventory "2" "2" 4 3)
                                           (make-inventory "3" "3" 4 4)))
              (list (make-inventory "1" "1" 4 2)
                    (make-inventory "2" "2" 4 3)))

; Number [Listof Inventory] -> [Listof Inventory]
; produce a list of Inventory whose sales price is below ua
(define (eliminate-expensive ua loi)
  (local (; Inventory -> Boolean
          (define (not-expensive i)
            (< (inventory-sales-price i) ua)))
    (filter not-expensive loi)))

(check-expect (recall "namehere" (list (make-inventory "namehere" "1" 4 2)
                                       (make-inventory "howdy" "2" 4 3)
                                       (make-inventory "errbody" "3" 4 4)))
              (list (make-inventory "howdy" "2" 4 3)
                    (make-inventory "errbody" "3" 4 4)))

; String [Listof Inventory] -> [Listof Inventory]
; produce a list of Inventory records that do not use the name ty
(define (recall ty loi)
  (local (; Inventory -> Boolean
          ; is record not matching the name ty
          (define (not-of-name-ty i)
            (not (string=? ty (inventory-item-name i)))))
    (filter not-of-name-ty loi)))

(check-expect
 (selection (list "a" "d" "f") (list "b" "c" "d" "e" "f"))
 (list "d" "f"))

; [Listof String] [Listof String] -> [Listof String]
; select all those from the second one that are also on the first
(define (selection lon1 lon2)
  (local (; [Listof String] String -> Boolean
          (define (in-list? pseudo-lon item)
            (cond [(empty? pseudo-lon) #false]
                  [(string=? item (first pseudo-lon)) #true]
                  [else (in-list? (rest pseudo-lon) item)]))
          ; String -> Boolean
          (define (exist-in-lon1 lon-2-item)
            (in-list? lon1 lon-2-item)))
    (filter exist-in-lon1 lon2)))

;; Exercise 270

(check-expect
 (zero-thru-n-1 5)
 (list 0 1 2 3 4))

; Number -> [Listof Number]
; creates the list (list 0 ... (- n 1)) for any natural number n;
(define (zero-thru-n-1 n)
  (local (; Number -> Number
          ; return the input
          (define (identity-fn n) n))
    (build-list n identity-fn)))

(check-expect (one-thru-n 5) (list 1 2 3 4 5))

; Number -> [Listof Number]
; creates the list (list 1 ... n) for any natural number n;
(define (one-thru-n n)
  (build-list n add1))

(check-expect (build-frac 5)
              (list 1 1/2 1/3 1/4 1/5))

; Number -> [Listof (fractional) Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(define (build-frac n)
  (local (; Number -> (Fractional) Number
          ; return the reciprocal of the input
          (define (inverter n)
            (/ 1 (add1 n))))
    (build-list n inverter)))

(check-expect (build-evens 4)
              (list 0 2 4 6))

; Number -> [Listof Number]
; creates the list of the first n even numbers
(define (build-evens n)
  (local ((define (doubler x)
            (* 2 x)))
    (build-list n doubler)))

(check-expect
 (identityM2 1)
 (list (list 1)))

(check-expect
 (identityM2 3)
 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; Number -> [Listof [Listof Number]]
; creates a diagonal square of 0s and 1s; see exercise 262.
(define (identityM2 n)
  (local (; Number -> [Listof Number]
          ; return sublist fully populated
          (define (build-sublist current)
            (local (; Number -> Number
                    ; return value for sublist
                    (define (populate-sublist-from-current i)
                      (if (= i current) 1 0)))
              (build-list n populate-sublist-from-current))))
    (build-list n build-sublist)))

; define tabulate from exercise 250 using build-list.

; Number -> Number
(define (my-identity2 i) i)

(check-expect (tabulate 4 my-identity2)
              (list 0 1 2 3))

; Number [Number -> Number] -> [Listof Number]
; tabulates cb res from 0..n-1 in a list
(define (tabulate n cb)
  (build-list n cb))

;; Exercise 271

(check-expect (find-name "joe" (list "joe" "phil")) #true)
(check-expect (find-name "joe" (list "joey" "phil")) #true)
(check-expect (find-name "joseph" (list "joey" "phil")) #false)

; String [Listof String] -> Boolean
; determines whether any of the names are equal to or an extension of the name.
(define (find-name name lon)
  (local ((define (name=? x)
            (string-contains? name x)))
    (ormap name=? lon)))

(check-expect (starts-with-a? (list "joe" "armada")) #false)
(check-expect (starts-with-a? (list "ayaz" "armada")) #true)

; [Listof String] -> Boolean
(define (starts-with-a? lon)
  (local ((define (individual-holds? x)
            (string=? "a" (first (explode x)))))
    (andmap individual-holds? lon)))

;; Should you use ormap or andmap to define a function that
;; ensures that no name on some list exceeds a given width? 
;; -> Both are viable with logical negation

;; Exercise 272

(check-expect
 (equal? (append (list 1 2 3) (list 4 5 6 7 8))
         (list 1 2 3 4 5 6 7 8))
 #true)

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))

(check-expect (append-from-fold (list) (list 1 2 3)) '(1 2 3))
(check-expect (append-from-fold (list 1 2 3) (list)) '(1 2 3))

;; TODO How get variable number args and recursively call append-from-fold for n lists?
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

;; What happens if you replace foldr with foldl?
;; -> Flips the order in which l2 is cons'ed onto l1,
;;    but not the order of l1 itself (base case is already cons'ed together).

(check-expect (fold-sum (list 1 2 3 4)) 10)

; [Listof Number] -> Number
(define (fold-sum lon)
  (foldr + 0 lon))

(check-expect (fold-product (list 1 2 3 4)) 24)
; [Listof Number] -> Number
(define (fold-product lon)
  (foldr * 1 lon))

(define RECT (rectangle 10 10 "solid" "red"))

(check-expect (horizontal-image-compose (list RECT RECT RECT))
              (beside RECT (beside RECT (beside RECT empty-image))))

(define (horizontal-image-compose loi)
  (foldr beside empty-image loi))

(check-expect (vertical-image-compose (list RECT RECT RECT))
              (above RECT (above RECT (above RECT empty-image))))

(define (vertical-image-compose loi)
  (foldr above empty-image loi))

;; foldr vs. foldl

(define RECT1 (rectangle 10 10 "solid" "red"))
(define RECT2 (rectangle 20 20 "solid" "orange"))
(define RECT3 (rectangle 30 30 "solid" "yellow"))

(check-expect (foldr beside empty-image (list RECT1 RECT2 RECT3))
              (beside RECT1 (beside RECT2 (beside RECT3 empty-image))))

(check-expect (foldl beside empty-image (list RECT1 RECT2 RECT3))
              (beside RECT3 (beside RECT2 (beside RECT1 empty-image))))

(check-expect (foldr above empty-image (list RECT1 RECT2 RECT3))
              (above RECT1 (above RECT2 (above RECT3 empty-image))))

(check-expect (foldl above empty-image (list RECT1 RECT2 RECT3))
              (above RECT3 (above RECT2 (above RECT1 empty-image))))

;; Exercise 273

;; foldr signature
; [X Y -> Y] Y [Listof X] -> Y

;; map signature
; [X -> Y] [Listof X] -> [Listof Y]

(check-expect
 (map-from-fold add1 (list 3 4 5))
 (list 4 5 6))

(check-expect
 (map-from-fold add1 (list 3 4 5))
 (cons (add1 3)
       (cons (add1 4)
             (cons (add1 5) '()))))

; [X -> Y] [Listof X] -> [Listof Y]
(define (map-from-fold cb lo-any)
  (local (; X Y -> Y
          (define (cons-and-call item prev)
            (cons (cb item) prev)))
    (foldr cons-and-call '() lo-any)))

;; Exercise 274

(check-expect (prefixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")
                    empty))
(check-expect (prefixes (list "a" "b"))
              (list (list "a" "b")
                    (list "a")
                    empty))
(check-expect (prefixes (list "a"))
              (list (list "a")
                    empty))
(check-expect (prefixes (cons "a" empty))
              (cons (cons "a" empty)
                    (cons empty '())))
(check-expect (prefixes empty) (list empty))

; [Listof 1String] -> [Listof [Listof 1String]]
; consumes a list of 1Strings and produce a list of all prefixes
(define (prefixes lo1s)
  (local (; 1String [Listof 1String] -> [Listof 1String]
          (define (prefix-sublist item listo)
            (cond [(empty? listo) '()]
                  [(string=? item (first listo)) listo]
                  [else (prefix-sublist item (rest listo))]))
          ; 1String [Listof [Listof 1String]] -> [Listof [Listof 1String]]
          (define (prefix-helper item prev)
            (cons (reverse (prefix-sublist item (reverse lo1s))) prev)))
    (foldl prefix-helper (list empty) lo1s)))

(check-expect (suffixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list     "b" "c" "d")
                    (list         "c" "d")
                    (list             "d")
                    empty))
(check-expect (suffixes (list "a" "b"))
              (list (list "a" "b")
                    (list     "b")
                    empty))
(check-expect (suffixes (list "a"))
              (list (list "a")
                    empty))
(check-expect (suffixes (cons "a" empty))
              (cons (cons "a" empty)
                    (cons empty '())))
(check-expect (suffixes empty) (list empty))

; [Listof 1String] -> [Listof [Listof 1String]]
; consumes a list of 1Strings and produce a list of all suffixes
(define (suffixes lo1s)
  (local (; 1String [Listof 1String] -> [Listof 1String]
          (define (suffix-sublist item listo)
            (cond [(empty? listo) '()]
                  [(string=? item (first listo)) listo]
                  [else (suffix-sublist item (rest listo))]))
          ; 1String [Listof [Listof 1String]] -> [Listof [Listof 1String]]
          (define (suffix-helper item prev)
            (cons (suffix-sublist item lo1s) prev)))
    (foldr suffix-helper (list empty) lo1s)))

;;;; 16.8 Projects: Abstraction

;; Exercise 275

;; Exercise 276

;; Exercise 277

;; Exercise 278

"end"

(test);; DrRacket does something like this behind the scenes
