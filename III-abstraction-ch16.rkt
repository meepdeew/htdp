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

(inf.v2 (list 2 1 3))

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

(sup.v2 (list 2 1 3))

;; Exercise 265 (Need to move from Intermediate Student --> Intermediate Student with Lambda)

((local ((define (f x) (+ (* 4 (sqr x)) 3))) f)
 1)
;; 7

;; Exercise 266

((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)
;; 5

;;;; 16.5 Using Abstractions, by Example

;;;; 16.6 Designing with Abstractions

;;;; 16.7 Finger Exercises: Abstraction

;;;; 16.8 Projects: Abstraction

"end"

(test);; DrRacket does something like this behind the scenes
