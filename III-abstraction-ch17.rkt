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

((lambda (x) x) (lambda (x) x))

((lambda (x) (x x)) (lambda (x) x))

; infinite recursion
; ((lambda (x) (x x)) (lambda (x) (x x)))

;;;; 17.3 Abstracting with lambda

;;;; 17.4 Specifying with lambda

;;;; 17.5 Representing with lambda

"end"

(test);; DrRacket does something like this behind the scenes
