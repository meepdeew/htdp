;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname III-abstraction-ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
;;;; 16 Using Abstractions

(require 2htdp/image)

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

;;;; 16.3 Local Definitions Add Expressive Power

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
