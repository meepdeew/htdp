;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname III-abstraction-ch17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require test-engine/racket-tests)

;;;; 17 Namesless Functions

(require 2htdp/image)
(require 2htdp/universe)

;; ################## From Ex 186 ###############

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

;; ##############################################


;;;; 17.4 Specifying with lambda


; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version (variant) of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon)
                       (isort (rest alon)))]))
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

(check-expect (sort-cmp '("c" "b") string<?)
              '("b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <)
              '(1 2 3 4 5 6))

; the check-satisfied model of exercise 186 becomes reformated as follows

(check-satisfied (sort-cmp '("c" "b") string<?)
                 (sorted-curry string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <)
                 (sorted-curry <))

; [X X -> Boolean] -> [ [NEList-of X] -> Boolean ]
; produces a function that determines whether
; some list is sorted according to cmp
(define (sorted-curry cmp)
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

(check-expect [(sorted-curry string<?) '("b" "c")] #true)
(check-expect [(sorted-curry <) '(1 2 3 4 5 6)] #true)
(check-expect [(sorted-curry >) '(5 4 3 2 1)] #true)
(check-expect [(sorted-curry <=) '(1 1 2 2 2)] #true)


;; Exercise 292


(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

; [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether l is sorted according to cmp
(define (sorted? cmp l)
  ((sorted-curry cmp) l))


; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort-cmp/bad l) '(9 8 7 6 5 4 3 2 1 0))

;; Passes, even though sort-cmp/bad doesn
(check-expect (sort-cmp/bad '(8 1 2 3 5 4 7 6 9))
              '(9 8 7 6 5 4 3 2 1 0))


; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? k l0))))

(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

(check-satisfied (sort-cmp '(2 1 3) <)
                 (sorted-variant-of '(2 1 3) <))


;; Source code for `check-satisfied` lives in:
;; /usr/share/racket/pkgs/htdp-lib/test-engine

(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
(check-expect (contains? '(1 3) '(1 2 3 4)) #false)

; [List-of X] [List-of X] -> Boolean
; are all items in list k members of list l
(define (contains? l k)
  (andmap (lambda (in-k)
            (member? in-k l)) k))

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
(define (sort-cmp/worse l)
  (local ((define sorted (sort-cmp l <)))
    (cons (- (first sorted) 1) sorted)))


(define (build-list-of-random-numbers n)
  (if (> n 0)
      (cons (random 1000) (build-list-of-random-numbers (- n 1)))
      '()))

(check-expect (dedupe '(1 2 2 3 3 3)) '(1 2 3))

; [List-of X] -> [List-of X]
; remove repeat occurences for an instance within the input
(define (dedupe lst)
  (cond [(empty? lst) '()]
        [else (if (member? (first lst) (rest lst))
                  (dedupe (rest lst))
                  (cons (first lst) (dedupe (rest lst))))]))

;; Still random content, but no random chance for duplicate entries.
(define a-list (dedupe (build-list-of-random-numbers 500)))

(check-satisfied (sort-cmp a-list <)
                 (sorted-variant-of a-list <))

(define l1 '(3 2 5 3))
(define l2 '(2 3 3 5))
(check-expect (apply (sorted-variant-of l2 <)
                     (list (sort-cmp l1 <)))
              #false)

;; Exercise 293
;; Develop `found?`, a specification for the find function.

; A [Maybe X] is one of: 
; – #false 
; – X

;; (check-expect (find 3 '(4 3 1)) 3)
;; (check-expect (find 7 '(4 3 1)) #false)

(check-satisfied (find 3 '(1 2 3))
                 (same-subelt-of 3 '(1 2 3)))

;;;; TODO Typo? Should sig not be `X [List-of X] -> [Maybe X]`?
;; If it returns a list, is that because it wraps it in a list
;; or because the input list's members were themselves lists?

; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x)
         l
         (find x (rest l)))]))

(define (found? ...)
  ...)

; X [List-of X] -> [X [List-of X] -> Boolean]
; are all items in list1 members of list l0
(define (same-subelt-of item1 list1)
  (lambda (item2 list2)
    (local ((define res1 (found? item1 list1))
            (define res2 (found? item2 list2))
            (define same-inputs
              (and (equal? item1 item2)
                   (contains? list1 list2)
                   (contains? list2 list1)))
            (define same-outputs (equal? res1 res2)))
      res1)))

(check-satisfied (find 3 '(1 2 3 4)) (found? ))


;; TODO
; Make phrases for the sort/variant-of specification, like:

; "CSists call `sorted-variant-of` a specification of a sorting function.
; the specification is the triple And thing.

; Develop `sorted-variant-of?`, a specification for `sort-cmp`.
;;;;; -> But `sorted-variant-of` uses the predicate
;;;;;         -> `sorted?`, which applies
;;;;;             -> sorted-curry
;;;;; -> And `sorted-variant-of?` uses `contains?`

;;;;; -> Then `sort-cmp` locally defines:
;;;;;          -> isort
;;;;;          -> insert

(define (sorted? cmp l)
  ((sorted-curry cmp) l))


; Use ``, to formulate a check-satisfied test for ``.

; Develop `?`, a specification for ``.
; Use ``, to formulate a check-satisfied test for ``.

; Develop `found?` a specification for the `find` function.
; Use `found?` to formulate a check-satisfied test for `find`.

; Develop `is-index?`, a specification for the `index` function.
; Use `is-index?` to formulate a check-satisfied test for index.

; Developer `n-inside-playground?`, a specification for the `random-posns` function.
; 

;; Exercise 294
;; Exercise 295

;;;; 17.5 Representing with lambda

;; Exercise 296
;; Exercise 297
;; Exercise 298
;; Exercise 299

"end 17.4"

(test);; DrRacket does something like this behind the scenes when run via IDE
