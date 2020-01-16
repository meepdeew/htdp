;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname III-abstraction-ch17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require test-engine/racket-tests)

;;;; 17 Namesless Functions

(require 2htdp/image)
(require 2htdp/universe)

(define-struct ir
  [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of: 
; – '()
; – (cons IR Inventory)

; [Listof IR] Number -> Boolean
;; (define (find l th)
;;   (local (; IR -> Boolean
;;           (define (acceptable? ir)
;;             (<= (ir-price ir) th)))
;;     (filter acceptable? l)))

; [Listof IR] Number -> Boolean
(define (find l th)
  (filter (lambda (ir) (<= (ir-price ir) th)) l))

;;;; 17.1 Functions from lambda

;; (lambda (variable-1 ... variable-N) expression)

(check-expect ((lambda (x) (expt 10 x)) 2)
              100)

(check-expect
 ((lambda (name rst) (string-append name ", " rst)) "Robby" "etc.")
 "Robby, etc.")

(check-expect
 ((lambda (ir) (<= (ir-price ir) th))
  (make-ir "bear" 10))
 #true)

(define th 20)

(check-expect
 (map (lambda (x) (expt 10 x)) '(1 2 3))
 (list 10 100 1000))

(check-expect
 (foldl (lambda (name rst)
          (string-append name ", " rst))
        "etc."
        '("Matthew" "Robby"))
 "Robby, Matthew, etc.")

(check-expect
 (filter (lambda (ir) (<= (ir-price ir) th))
         (list (make-ir "bear" 10)
               (make-ir "doll" 33)))
 (list (make-ir "bear" 10)))

;; Exercise 279

;; Valid
; (lambda (x y) (x y y))

;; Validity depends on language pack selected
; (lambda () 10)

;; Valid - identity
; (lambda (x) x)

;; Valid
; (lambda (x y) x)

;; Invalid - not well-formed
(check-error
 (lambda x 10))

;; Exercise 280

(check-expect
 ((lambda (x y) (+ x (* x y)))
  1 2)
 3)

;; Intermediate Student with Lambda gives:
;; define: not allowed in an expression context
;; but works ok on Advanced Student.
;; (kind of like earlier local expressions were off by one section)
(check-expect
 ((lambda (x y)
    (+ x
       (local ((define z (* y y)))
         (+ (* 3 z) (/ 1 x)))))
  1 2)
 14)

(check-expect
 ((lambda (x y)
    (+ x
       ((lambda (z)
          (+ (* 3 z) (/ 1 z)))
        (* y y))))
  1 2)
 13.25)

;; Exercise 281

; 1. consumes a number and decides whether it is less than 10;
(check-expect
 ((lambda (num) (< num 10)) 5)
 #true)

; 2. multiplies two given numbers and turns the result into a string;
(check-expect
 ((lambda (x y) (string-append (number->string (* x y)) ".")) 2 4)
 "8.")

; 3. consumes a natural number and returns 0 for evens and 1 for odds;
(check-expect
 ((lambda (nat) (if (= 0 (modulo nat 2)) 0 1)) 6)
 0)

; 4. consumes two inventory records and compares them by price;
(check-expect
 ((lambda (ir1 ir2) (<= (ir-price ir1) (ir-price ir2)))
  (make-ir "name1" 12) (make-ir "name2" 15))
 #true)

; 5. adds a red dot at a given Posn to a given Image.

(define emt
  (empty-scene 100 100))

(define dot
  (circle 3 "solid" "red"))

(check-expect
 (place-image
  dot
  5 5
  emt)
 ((lambda (p img)
    (place-image dot (posn-x p) (posn-y p) img))
  (make-posn 5 5) emt))

;;;; 17.2 Computing with lambda

;; Exercise 282

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
    (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

(check-expect
 (compare (random 100000))
 #true)

(check-expect
 (f-plain (f-plain 42))
 4200)

(check-expect 
 (f-lambda (f-lambda 42))
 ((lambda (x) (* 10 x))
 ((lambda (x) (* 10 x))
  42)))

(check-expect
 ((lambda (x) (* 10 x)) 2)
 (* 10 2))

(check-expect
 ((lambda (name rst) (string-append name ", " rst)) "Robby" "etc.")
 (string-append "Robby" ", " "etc."))

(check-expect
 ((lambda (ir) (<= (ir-price ir) th)) (make-ir "bear" 10))
 (<= (ir-price (make-ir "bear" 10)) th))

;; Exercise 283

(check-expect
 (map (lambda (x) (* 10 x))
      '(1 2 3))
 (list (* 10 1) (* 10 2) (* 10 3)))

(check-expect
 (foldl (lambda (name rst)
          (string-append name ", " rst))
        "etc."
        '("Matthew" "Robby"))
 "Robby, Matthew, etc.")

(check-expect
 (filter (lambda (ir) (<= (ir-price ir) th))
         (list (make-ir "bear" 10)
               (make-ir "doll" 33)))
 (list (make-ir "bear" 10)))

;; Exercise 284

;; ((lambda (x) x) (lambda (x) x))
;; ((lambda (x) (x x)) (lambda (x) x))
;; ((lambda (x) (x x)) (lambda (x) (x x))) ; recurses infinitely

;;;; 17.3 Abstracting with lambda

(define DOT dot)
(define BACKGROUND (empty-scene 100 100))

;; (define (dots lop)
;;   (local (; Posn Image -> Image
;;           (define (add-one-dot p scene) ...))
;;     (foldr add-one-dot BACKGROUND lop)))

(define (dots lop)
  (foldr (lambda (a-posn scene)
           (place-image DOT
                        (posn-x a-posn)
                        (posn-y a-posn)
                        scene))
         BACKGROUND lop))

; [Listof Posn] -> [Listof Posn]
(define (add-3-to-all lop)
  (map (lambda (p)
         (make-posn (+ (posn-x p) 3) (posn-y p)))
       lop))

; [Listof Posn] -> [Listof Posn]
(define (keep-good lop)
  (filter (lambda (p) (<= (posn-y p) 100)) lop))

(define CLOSENESS 5)
; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(define (close-to p q d)
  (local ((define (within-distance a b)
            (< (abs (- a b)) 5)))
    (and (within-distance (posn-x p) (posn-x q))
         (within-distance (posn-y p) (posn-y q)))))

; [Listof Posn] -> Boolean
(define (close? lop pt)
  (ormap (lambda (p) (close-to p pt CLOSENESS))
         lop))

;; Exercise 285


(define CONVERSION 1.06)

(check-expect (convert-euro (list 1.06 2.12))
              (list 1.00 2.00))

; [Listof Number] -> [Listof Number]
; Converts a list of US$ amounts into a list of € amounts
; based on an exchange rate of US$1.06 per € (on April 13, 2017).
(define (convert-euro lon)
  (map (lambda (euro)
         (* euro (/ 1 CONVERSION))) lon))

(check-expect (convertFC (list 32 212))
              (list 0 100))

; [Listof Number] -> [Listof Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements.
(define (convertFC fahrenheits)
  (map (lambda (fahrenheit)
         (* (- fahrenheit 32) (/ 5 9)))
       fahrenheits))

(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))

; [Listof Posn] -> [Listof [Listof Number]]
; translates a list of Posns into a list of lists of pairs of numbers.
(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p)))
       lop))

;; Exercise 286


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
  (sort loi (lambda (inv1 inv2)
              (< (- (inventory-acquisition-price inv1) (inventory-sales-price inv1))
               (- (inventory-acquisition-price inv2) (inventory-sales-price inv2))))))

;; Exercise 287

(check-expect (eliminate-expensive 4 (list (make-inventory "1" "1" 4 2)
                                           (make-inventory "2" "2" 4 3)
                                           (make-inventory "3" "3" 4 4)))
              (list (make-inventory "1" "1" 4 2)
                    (make-inventory "2" "2" 4 3)))

; Number [Listof Inventory] -> [Listof Inventory]
; produce a list of Inventory whose sales price is below ua
(define (eliminate-expensive ua loi)
  (filter (lambda (i)
            (< (inventory-sales-price i) ua)) loi))

(check-expect (recall "namehere" (list (make-inventory "namehere" "1" 4 2)
                                       (make-inventory "howdy" "2" 4 3)
                                       (make-inventory "errbody" "3" 4 4)))
              (list (make-inventory "howdy" "2" 4 3)
                    (make-inventory "errbody" "3" 4 4)))


; String [Listof Inventory] -> [Listof Inventory]
; produce a list of Inventory records that do not use the name ty
(define (recall ty loi)
  (filter (lambda (i)
            (not (string=? ty (inventory-item-name i)))) loi))


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
                  [else (in-list? (rest pseudo-lon) item)])))
    (filter (lambda (lon-2-item)
              (in-list? lon1 lon-2-item)) lon2)))

;; Exercise 288

(check-expect
 (zero-thru-n-1 5)
 (list 0 1 2 3 4))

; Number -> [Listof Number]
; creates the list (list 0 ... (- n 1)) for any natural number n;
(define (zero-thru-n-1 n)
  (build-list n (lambda (n) n)))

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
  (build-list n (lambda (n) (/ 1 (add1 n)))))

(check-expect (build-evens 4)
              (list 0 2 4 6))

; Number -> [Listof Number]
; creates the list of the first n even numbers
(define (build-evens n)
  (build-list n (lambda (x) (* 2 x))))


(check-expect
 (identityM2 1)
 (list (list 1)))

(check-expect
 (identityM2 3)
 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; Number -> [Listof [Listof Number]]
; creates a diagonal square of 0s and 1s; see exercise 262.
(define (identityM2 n)
  (build-list
   n (lambda (current)
       (build-list n (lambda (i) (if (= i current) 1 0))))))

; define tabulate from exercise 250 using build-list.

(check-expect (tabulate 4 (lambda (i) i))
              (list 0 1 2 3))

; Number [Number -> Number] -> [Listof Number]
; tabulates cb res from 0..n-1 in a list
(define (tabulate n cb)
  (build-list n cb))

;; Exercise 289

(check-expect (find-name "joe" (list "joe" "phil")) #true)
(check-expect (find-name "joe" (list "joey" "phil")) #true)
(check-expect (find-name "joseph" (list "joey" "phil")) #false)

; String [Listof String] -> Boolean
; determines whether any of the names are equal to or an extension of the name.
(define (find-name name lon)
  (ormap (lambda (x) (string-contains? name x)) lon))

(check-expect (starts-with-a? (list "joe" "armada")) #false)
(check-expect (starts-with-a? (list "ayaz" "armada")) #true)

; [Listof String] -> Boolean
(define (starts-with-a? lon)
  (andmap (lambda (x) (string=? "a" (first (explode x)))) lon))

;; Exercise 290

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

;; Exercise 291

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
  (foldr (lambda (item prev)
           (cons (cb item) prev)) '() lo-any))


;;;; 17.4 Specifying with lambda

;;;; 17.5 Representing with lambda

"end"

(test);; DrRacket does something like this behind the scenes
