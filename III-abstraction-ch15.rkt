;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname III-abstraction-ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)

;;;; 15 Designing Abstractions

(require 2htdp/image)

;;;; 15.1 Abstractions from Examples

; List-of-numbers -> List-of-numbers
; converts a list of Celsius
; temperatures to Fahrenheit
(define (cf* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (C2F (first l))
      (cf* (rest l)))]))

; Number -> Number
; converts one Celsius
; temperature to Fahrenheit
(define (C2F c)
  (+ (* 9/5 c) 32))

(check-expect (cf* '(10 20 30))
              (list (C2F 10) (C2F 20) (C2F 30)))

; [List-of IR] -> List-of-strings
; Inventory -> List-of-strings
; extracts the names of
; toys from an inventory
(define (names i)
  (cond
    [(empty? i) '()]
    [else
     (cons
      (IR-name (first i))
      (names (rest i)))]))

(define-struct IR
  [name price])
; An IR is a structure:
;   (make-IR String Number)

; An Inventory is one of:
; - '()
; - (cons IR Inventory)

;; Q: Why define Inventory rather than just saying [List-of IR]

(check-expect (names (list (make-IR "yo" 32) (make-IR "dude" 43)))
              (list "yo" "dude"))


(define (map1 k g)
  (cond [(empty? k) '()]
        [else (cons (g (first k))
                    (map1 (rest k) g))]))

; List-of-numbers -> List-of-numbers
(define (cf*-from-map1 l)
  (map1 l C2F))

; Inventory -> List-of-strings
(define (names-from-map1 i)
  (map1 i IR-name))

(check-expect (cf* (list 100 0 -40))
              (list 212 32 -40))

(check-expect (names
               (list
                (make-IR "doll" 21.0)
                (make-IR "bear" 13.0)))
              (list "doll" "bear"))

(check-expect (cf*-from-map1 (list 100 0 -40))
              (list 212 32 -40))

(check-expect (names-from-map1
               (list
                (make-IR "doll" 21.0)
                (make-IR "bear" 13.0)))
              (list "doll" "bear"))

(check-expect (add-1-to-each '(1 2 3 4))
              '(2 3 4 5))

; List-of-numbers -> List-of-numbers
(define (add-1-to-each l)
  (map1 l add1))

;; Exercise 250

; Number -> [List-of Number]
; tabulates sin between n
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond [(= n 0) (list (sin 0))]
        [else (cons (sin n)
                    (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond [(= n 0) (list (sqrt 0))]
        [else (cons (sqrt n) (tab-sqrt (sub1 n)))]))


(define (tabulate n cb)
  (cond [(= n 0) (list (cb 0))]
        [else (cons (cb n) (tabulate (sub1 n) cb))]))

; Number -> [List-of Number]
; tabulates sqrt between n
; and 0 (incl.) in a list
(define (tab-sqrt* n)
  (tabulate n sqrt))

; Number -> [List-of Number]
; tabulates sin between n
; and 0 (incl.) in a list
(define (tab-sin* n)
  (tabulate n sin))

(check-expect (tab-sqr 4)
              (list (sqr 4) (sqr 3) (sqr 2) (sqr 1) (sqr 0)))

(define (tab-sqr n)
  (tabulate n sqr))

(define (tab-tan n)
  (tabulate n tan))

;; Exercise 251

; [List-of Number] -> Number
; computes the sum of
; the numbers on l
(define (sum l)
  (cond [(empty? l) 0]
        [else (+ (first l)
                 (sum (rest l)))]))

(check-expect (sum '(1 2 3 4))
              10)

; [List-of Number] -> Number
; computes the product of
; the numbers on l
;; (define (product l)
;;   (cond [(empty? l) 1]
;;         [else (* (first l)
;;                  (product (rest l)))]))

(check-expect (product '(1 2 3 4))
              24)

; [List-of Any] fn Any -> Any
(define (fold1 l cb base-case)
  (cond [(empty? l) base-case]
        [else (cb (first l)
                  (fold1 (rest l) cb base-case))]))

(check-expect (fold1 '(1 2 3 4) + 0)
              10)

(check-expect (fold1 '(1 2 3 4) + 0)
              (+ 1 (+ 2 (+ 3 (+ 4 0)))))

(check-expect (fold1 '(1 2 3 4) * 1)
              24)

(check-expect (fold1 '(1 2 3 4) * 1)
              (* 1 (* 2 (* 3 (* 4 1)))))

;; Exercise 252

; Posn Image -> Image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; graphical constants
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))


; [List-of Number] -> Number
(define (product l)
  (cond [(empty? l) 1]
        [else (* (first l)
                 (product (rest l)))]))

; [List-of Posn] -> Image
(define (image* l)
  (cond [(empty? l) emt]
        [else (place-dot (first l)
                         (image* (rest l)))]))

; Q: Supposed to be the same implemetation as 251's fold1?
(define (fold2 l cb base-case)
  (cond [(empty? l) base-case]
        [else (cb (first l)
                  (fold2 (rest l) cb base-case))]))




(check-expect (fold2 '(2 3 4 5)
                     * 1)
              (product '(2 3 4 5)))

(check-expect (fold2 '(2 3 4 5) * 1)
              (* 2 (* 3 (* 4 (* 5 1)))))

(check-expect (fold2 (list (make-posn 2 1)
                           (make-posn 3 4))
                     place-dot emt)
              (image* (list (make-posn 2 1)
                            (make-posn 3 4))))

(check-expect (fold2 (list (make-posn 2 1) (make-posn 3 4)) place-dot emt)
              (place-dot (make-posn 2 1) (place-dot (make-posn 3 4) emt)))

;; 15.2 Similarities in Signatures


; Number Boolean -> String
(define (f n b) "hello world")

; A parametrized signature, such as:
; [X Y] [List-of X] -> [List-of Y]

; can  be instantiated as (for `cf*`):
; [List-of Number] -> [List-of Number]

; or as (for `names`):
; [List-of IR] -> [List-of String]



; We never declared a signature for `map1`

; Here are the signatures of two functions we wrote
; that make use of abstract function `map1`:

; List-of-numbers -> List-of-numbers
;(define (cf*-from-map1 l) (map1 l C2F))
; and C2F was: Number -> Number

; Inventory -> List-of-strings
;(define (names-from-map1 i) (map1 i IR-name))
; and IR-name was: IR -> String


; [X Y] [List-of X] [X -> Y] -> [List-of Y]


; [List-of Number] -> Number
; [List-of Posn] -> Image

; [List-of Number] Number [Number Number -> Number]
; -> Number
(define (pr* l bs jn)
  (cond
    [(empty? l) bs]
    [else
     (jn (first l)
         (pr* (rest l)
              bs
              jn))]))

; [List-of Posn] Image [Posn Image -> Image]
; -> Image
(define (im* l bs jn)
  (cond
    [(empty? l) bs]
    [else
     (jn (first l)
         (im* (rest l)
              bs
              jn))]))

; [X Y] [List-of X] Y [X Y -> Y] -> Y
; (define fold2 (...) ...)
; Although mine had base-case and callback order flipped.

; Exercise 253

;; See question, asked here:
;; https://www.reddit.com/r/Racket/comments/ec5zuu/htdp_exercise_253_describe_collections_with_at/

; 1)
; [Number -> Boolean]
(check-expect (even? 4) #true)
(check-expect (even? 1) #false)
(check-error (even? 'foo))

; 2)
; [Boolean String -> Boolean]
(check-expect (equal? #true "true") #false)

; 3)
; [Number Number Number -> Number]
(check-expect (+ 1 2 3) 6)
(check-error (+ 1 2 'foo))

; 4)
; [Number -> [List-of Number]]
(check-expect (list 5) '(5))

; 5)
; [[List-of Number] -> Boolean]
(check-expect (list? '(5)) #true)


; Exercise 254

;; From Chapter 11
(check-expect (insert 2 (list 1 3)) (list 1 2 3))
(check-expect (insert 2 (list 1)) (list 1 2))
(check-expect (insert 2 (list 3)) (list 2 3))
(check-expect (insert 2 (list)) (list 2))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond [(empty? alon) (list n)]
        [(< n (first alon)) (cons n alon)]
        [else (cons (first alon)
                    (insert n (rest alon)))]))

(check-expect (ho-insert 3 (list 1 4) <) (list 1 3 4))
(check-expect (ho-insert 3 (list 4 5) <) (list 3 4 5))
(check-expect (ho-insert 6 (list 4) <) (list 4 6))
(check-expect (ho-insert 6 (list 4 5) <) (list 4 5 6))
(check-expect (ho-insert 3 (list) <) (list 3))

;; [Number [List-of Number] [Number Number -> Boolean] -> [List-of Number]]
(define (ho-insert n lon cb)
  (cond [(empty? lon) (list n)]
        [(cb n (first lon)) (cons n lon)]
        [else (cons (first lon)
                    (ho-insert n (rest lon) cb))]))


(check-expect (sort-n '(2 7 1 4) <) '(1 2 4 7))
(check-expect (sort-n '(4 1) <) '(1 4))
(check-expect (sort-n '(3) <) '(3))
(check-expect (sort-n '() <) empty)


;; [[List-of Number] [Number Number -> Boolean] -> [List-of Number]]
(define (sort-n lon cb)
  (cond [(empty? lon) empty]
        [else (ho-insert (first lon)
                         (sort-n (rest lon) cb)
                         cb)]))

(check-expect (sort-s '("b" "c" "a") string<?) '("a" "b" "c"))
(check-expect (sort-s '("f" "e" "b" "c" "a") string<?)
              '("a" "b" "c" "e" "f"))

;; [[List-of String] [String String -> Boolean] -> [List-of String]]
(define (sort-s los cb)
  (cond [(empty? los) empty]
        [else (ho-insert (first los)
                         (sort-s (rest los) cb)
                         cb)]))

(define (IR-compare ir1 ir2)
  (< (IR-price ir1)
     (IR-price ir2)))

;; [[List-of IR] [IR IR -> Boolean] -> [List-of IR]]
(define (sort-ir loir cb)
  (sort-generic loir IR-compare empty))

;; [[List-of X] [X X -> Boolean] -> [List-of X]]
(define (sort-generic listo cb base-case)
  (cond [(empty? listo) base-case]
        [else (ho-insert (first listo)
                         (sort-s (rest listo) cb)
                         cb)]))

; Exercise 255

(check-expect (map-n (list 1 2 3) add1) (list 2 3 4))

;; [[Listof Number] [Number -> Number] -> [Listof Number]]
(define (map-n lon cb)
  (cond [(empty? lon) empty]
        [else (cons (cb (first lon))
                    (map-n (rest lon) cb))]))

(define (my-identity str) str)

(check-expect (map-n (list "a" "b" "c") my-identity) (list "a" "b" "c"))

;; [[Listof String] [String -> String] -> [Listof String]]
(define (map-s los cb)
  (cond [(empty? los) empty]
        [else (cons (cb (first los))
                    (map-s (rest los) cb))]))

;; [[Listof X] [X -> X] -> [Listof X]]
(define (map-generic listo cb base-case)
  (cond [(empty? listo) base-case]
        [else (cons (cb (first listo))
                    (map-generic (rest listo) cb))]))

;; (define (map1 k g)
;;   (map-generic k g empty))



;;;; 15.3 Single Point of Control

;; Form an abstraction instead of copying and modifying any code.

;;;; 15.4 Abstractions from Templates

;; [X Y] [[List-of X] Y [X Y -> Y] -> Y]
(define (reduce l base combine)
  (cond
    [(empty? l) base]
    [else (combine (first l)
                   (reduce (rest l) base combine))]))

;; [List-of Number] -> Number
;; (define (sum lon)
;;   (reduce lon 0 +))

;; [List-of Number] -> Number
;; (define (product lon)
;;   (reduce lon 1 *))


"end"

(test);; DrRacket does something like this behind the scenes
