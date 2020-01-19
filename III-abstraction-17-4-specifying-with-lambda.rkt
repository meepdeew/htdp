;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname III-abstraction-ch17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require test-engine/racket-tests)

;;;; 17 Namesless Functions

(require 2htdp/image)
(require 2htdp/universe)

;;;; 17.4 Specifying with lambda

;(check-expect (sort-cmp '("c" "b") string<?) '("b" "c"))
(check-satisfied (sort-cmp '("c" "b") string<?)
                 (sorted string<?))
;(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <)
                 (sorted <))

; [Listof Number] [Number Number -> Boolean] -> [Listof Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [Listof Number] -> [Listof Number]
          ; produces the sorted version (variant) of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon)
                       (isort (rest alon)))]))
          ; Number [Listof Number] -> [Listof Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -272.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

; NEList-of-temperatures -> Boolean
(define (sorted>? lst)
  (cond [(empty? lst) #true]
        [(empty? (rest lst)) #true]
        [else (if (> (first lst) (first (rest lst)))
                   (sorted>? (rest lst))
                   #false)]))

(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

; [X X -> Boolean] -> [ [Listof X] -> Boolean ]
; produces a function that determines whether
; some list is sorted according to cmp
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond [(empty? l) #true]
                    [(empty? (rest l)) #true]
                    [else (if (cmp (first l) (first (rest l)))
                              (sorted/l (rest l))
                              #false)])))
      (sorted/l l0))))

;; Exercise 292

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

; [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether l is sorted according to cmp
(define (sorted? cmp l)
  ((sorted cmp) l))


(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

; [Listof X] [X X -> Boolean] -> [[Listof X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? k l0))))

(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
(check-expect (contains? '(1 3) '(1 2 3 4)) #false)

; [Listof X] [Listof X] -> Boolean
; are all items in list k members of list l
(define (contains? l k)
  (andmap (lambda (in-k)
            (member? in-k l)) k))

; [Listof Number] -> [Listof Number]
; produces a sorted version of l
(define (sort-cmp/worse l)
  (local ((define sorted (sort-cmp l <)))
    (cons (- (first sorted) 1) sorted)))

(define (build-list-of-random-numbers n)
  (if (> n 0)
      (cons (random 1000) (build-list-of-random-numbers (- n 1)))
      '()))

(define a-list (build-list-of-random-numbers 5))

(check-satisfied (sort-cmp a-list <)
                 (sorted-variant-of a-list <))

;; (define l1 '(3 2 5 3))
;; (check-satisfied (sort-cmp l1 <)
;;                  (sorted-variant-of l1 <))

;; Exercise 293

; A [Maybe X] is one of: 
; – #false 
; – X

(check-expect (find 3 '(4 3 1)) 3)
(check-expect (find 7 '(4 3 1)) #false)

;;;; TODO Typo? Should sig not be `X [List-of X] -> [Maybe X]`?

; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; [Listof X] [X X -> Boolean] -> [[Listof X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
(define (found item1 list1)
  (lambda (item2 list2)
    (and (equal? item1 item2)
         (contains? list1 list2)
         (contains? list2 list1))))

;; Exercise 294
;; Exercise 295

;;;; 17.5 Representing with lambda

;; Exercise 296
;; Exercise 297
;; Exercise 298
;; Exercise 299

"end 17.4"

(test);; DrRacket does something like this behind the scenes when run via IDE
