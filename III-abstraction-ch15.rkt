;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname III-abstraction-ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
;;;; 14 Similarities Everywhere

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

; ]List-of IR] -> List-of-strings
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

; [Number -> Boolean]
(check-expect (even? 4) #true)
(check-expect (even? 1) #false)
(check-error (even? 'foo))

; [Boolean String -> Boolean]
(check-expect (boolean=?))


; [Number Number Number -> Number]
(check-expect (+ 1 2 3) 6)
(check-error (+ 1 2 'foo))

; [Number -> [List-of Number]]

; [[List-of Number] -> Boolean]


; Exercise 254

(check-expect (sort-n '(2 7 1 4) <) '(1 2 4 7))

;; [Listof Number [Number Number -> Boolean] -> Listof Number]
(define (sort-n lon cb)
  ())

(check-expect)

;; [Listof String [String String -> Boolean] -> Listof String]
(define (sort-s los cb)
  ())

;; [Listof IR [IR IR -> Boolean] -> Listof IR]
(define (sort-ir loir cb)
  ())

;; [Listof X [X X -> Boolean] -> Listof X]
(define (sort-generic listo cb)
  ())

; Exercise 255









;; C-(		paredit-backward-slurp-sexp
;; C-)		paredit-forward-slurp-sexp

;; C-{		paredit-backward-barf-sexp
;; C-}		paredit-forward-barf-sexp

;; <C-M-left>	paredit-backward-slurp-sexp
;; <C-M-right>	paredit-backward-barf-sexp
;; <C-left>	paredit-forward-barf-sexp
;; <C-right>	paredit-forward-slurp-sexp


"end"

(test);; DrRacket does something like this behind the scenes
