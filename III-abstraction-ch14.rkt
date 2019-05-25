;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname III-abstraction-ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;; 14 Similarities Everywhere

(require 2htdp/image)

;;;; 14.1 Similarities in Functions


; Los -> Boolean
; does l contain "dog"
(define (contains-dog? l)
  (cond [(empty? l) #false]
        [else (or (string=? (first l) "dog")
                  (contains-dog? (rest l)))]))

; Los -> Boolean
; does l contain "cat"
(define (contains-cat? l)
  (cond [(empty? l) #false]
        [else (or (string=? (first l) "cat")
                  (contains-cat? (rest l)))]))

; Los -> Boolean
; determines whether l contain the string s
(define (contains? s l)
  (cond [(empty? l) #false]
        [else (or (string=? (first l) s)
                  (contains? s (rest l)))]))

;; Exercise 235
; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; does l contain "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contain "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))

;; Exercise 236
; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (cond [(empty? l) '()]
        [else
         (cons
          (add1 (first l))
          (add1* (rest l)))]))

; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond [(empty? l) '()]
        [else
         (cons
          (+ (first l) 5)
          (plus5 (rest l)))]))

;; Note: Racket-mode run tests with C-c C-t
(check-expect (add1* '()) '())
(check-expect (add1* '(95)) '(96))
(check-expect (add1* '(1 2 3 4)) '(2 3 4 5))
(check-expect (plus5 '()) '())
(check-expect (plus5 '(94)) '(99))
(check-expect (plus5 '(1 2 3 4)) '(6 7 8 9))
(check-expect (plus3 '()) '())
(check-expect (plus3 '(3)) '(6))
(check-expect (plus3 '(3 4)) '(6 7))
(check-expect (plus3* '()) '())
(check-expect (plus3* '(3)) '(6))
(check-expect (plus3* '(3 4)) '(6 7))

(define (add-n l n)
  (cond [(empty? l) '()]
        [else
         (cons (+ (first l) n)
               (add-n (rest l) n))]))

; Lon -> Lon
; adds 3 to each item on l
(define (plus3 l)
  (add-n l 3))

(define (add-n* l n op)
  (cond [(empty? l) '()]
        [else
         (cons (op (first l) n)
               (add-n* (rest l) n op))]))

(define (plus3* l)
  (add-n* l 3 +))

(check-expect (minus2 '()) '())
(check-expect (minus2 '(5)) '(3))
(check-expect (minus2 '(6 5)) '(4 3))

; Lon -> Lon
; adds -2 to each item on l
(define (minus2 l)
  (add-n l -2))


;;;; 14.2 Different Similarities

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (cond [(empty? l) '()]
        [else
         (cond [(< (first l) t)
                (cons (first l)
                      (small (rest l) t))]
               [else (small (rest l) t)])]))

; Lon Number -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (cond [(empty? l) '()]
        [else
         (cond [(> (first l) t)
                (cons (first l)
                      (large (rest l) t))]
               [else (large (rest l) t)])]))

(define (extract R l t)
  (cond [(empty? l) '()]
        [else (cond [(R (first l) t)
                     (cons (first l)
                           (extract R (rest l) t))]
                    [else (extract R (rest l) t)])]))

(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5) (small '(3) 5))
(check-expect (extract < '(1 6 4) 5)
              (small '(1 6 4) 5))

; Lon Number -> Lon
(define (small-1 l t)
  (extract < l t))

; Lon Number -> Lon
(define (large-1 l t)
  (extract > l t))

;; Exercise 237

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(check-expect (extract squared>? (list 3 4 5) 10) '(4 5))
(check-expect (squared>? 3 10) #false)
(check-expect (squared>? 4 10) #true)
(check-expect (squared>? 5 10) #true)

; Nelon -> Number
; determines the smallest
; number on l
(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else (if (< (first l)
                     (inf (rest l)))
                  (first l)
                  (inf (rest l)))]))

; Nelon -> Number
; determines the largest
; number on l
(define (sup l)
  (cond [(empty? (rest l)) (first l)]
        [else (if (> (first l)
                     (sup (rest l)))
                  (first l)
                  (sup (rest l)))]))

;; Exercise 238

; Nelon -> Number
; determines the _____
; number on l
(define (infsup l t)
  (cond [(empty? (rest l)) (first l)]
        [else (if (t (first l)
                     (infsup (rest l) t))
                  (first l)
                  (infsup (rest l) t))]))

(define (inf-1 l)
  (infsup l <))

(define (sup-1 l)
  (infsup l >))

(define l1 (list 25 24 23 22 21 20 19 18 17 16 15
                 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
 
(define l2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14
                 15 16 17 18 19 20 21 22 23 24 25))

;; use max
(define (minf l cb)
  (cond [(empty? (rest l)) (first l)]
        [else (minf (cons (cb (first l) (second l))
                          (rest (rest l)))
                    cb)]))

(define (inf-2 l)
  (minf l min))

(define (sup-2 l)
  (minf l max))


;;;; 14.3 Similarities in Data Definitions

(define-struct ir [name price])
; An IR is a structure:
;    (make-ir String Number)

; A List-of-numbers-again is one of:
; - '()
; - (cons Number List-of-numbers-again)

(define-struct point [hori veri])

; A Pair-boolean-string is a structure:
;   (make-point Boolean String)

; A Pair-number-image is a structure:
;   (make-point Number Image)

; A [CP H V] is a structure:
;   (make-point H V)

;; Exercise 239

; A [List X Y] is a structure:
;   (cons X (cons Y '()))

; A [List Number Number] is a structure:
; - (cons Number (cons Number '()))
(check-expect (list 1 2) (cons 1 (cons 2 '())))

; A [List Number 1String] is a structure:
; - (cons Number (cons 1String '()))
(check-expect (list 1 "A") (cons 1 (cons "A" '())))

; A [List String Boolean] is a structure:
; - (cons String (cons Boolean '()))
(check-expect (list "Hello" #true) (cons "Hello" (cons #true '())))

; [List-of [CP Boolean Image]]

;; Exercise 240

(define-struct layer [stuff])

; An LStr is one of:
; - String
; - (make-layer LStr)

; An LNum is one of:
; - Number
; - (make-layer LNum)



;;;; 14.4 Functions Are Values

;;;; 14.5 Computing with Functions

"end of thing"
