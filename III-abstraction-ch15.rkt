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
        [else (cons (sqrt n) (sub1 n))]))


(define (tabulate n cb)
  (cond [(= n 0) (list (cb 0))]
        [else (cons (cb n) (sub1 n))]))

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
(define (fold1 l cb r4bc)
  (cond [(empty? l) r4bc]
        [else (cb (first l)
                  (fold1 (rest l) cb r4bc))]))

(check-expect (fold1 '(1 2 3 4) + 0)
              10)
(check-expect (fold1 '(1 2 3 4) * 1)
              24)

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

(define (fold2 l cb base-case)
  (cond [(empty? l) base-case]
        [else (cb (first l)
                  (fold2 (rest l) cb base-case))]))




(check-expect (fold2 '(2 3 4 5)
                     * 1)
              (product '(2 3 4 5)))

(check-expect (fold2 (list (make-posn 2 1)
                           (make-posn 3 4))
                     place-dot emt)
              (image* (list (make-posn 2 1)
                            (make-posn 3 4))))

;; 15.2 Similarities in Signatures

"end"

(test);; DrRacket does something like this behind the scenes
