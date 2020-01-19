(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

;;;; 11 Design by Composition

;;;; 11.1 The `list' Function

;; ex 181

(check-expect
 (cons "a" (cons "b" (cons "c" (cons "d" '()))))
 (list "a" "b" "c" "d"))

(check-expect
 (cons (cons 1 (cons 2 '())) '())
 (list (list 1 2)))

(check-expect
 (cons "a" (cons (cons 1 '()) (cons #false '())))
 (list "a" (list 1) #false))

(check-expect
 (cons (cons "a" (cons 2 '())) (cons "hello" '()))
 (list (list "a" 2) "hello"))




(check-expect
 (cons (cons 1 (cons 2 '()))
      (cons (cons 2 '())
            '()))
 (list (list 1 2) (list 2)))

;; ex 182

(check-expect
 (list 0 1 2 3 4 5)
 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))

(check-expect
 (list (list "he" 0) (list "it" 1) (list "lui" 14))
 (cons (cons "he" (cons 0 '()))
       (cons (cons "it" (cons 1 '()))
             (cons (cons "lui" (cons 14 '()))
                   '()))))

(check-expect
 (list 1 (list 1 2) (list 1 2 3))
 (cons 1
       (cons (cons 1 (cons 2 '()))
             (cons (cons 1 (cons 2 (cons 3 '())))
                   '()))))

;; ex 183, reformulate using:
;; cons
(check-expect
 (cons "a" (list 0 #false))
 (cons "a" (cons 0 (cons #false empty))))

(check-expect
 (list (cons 1 (cons 13 '())))
 (cons (cons 1 (cons 13 empty)) empty))

(check-expect
 (cons (list 1 (list 13 '())) '())
 (cons (cons 1 (cons (cons 13 (cons '() empty)) empty)) empty))


(check-expect
 (list '() '() (cons 1 '()))
 (cons empty (cons empty (cons (cons 1 empty) empty))))

(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (cons "a"
       (cons (cons 1 empty)
             (cons #false
                   (cons '() empty)))))

;; list
(check-expect
 (cons "a" (list 0 #false))
 (list "a" 0 #false))

(check-expect
 (cons (list 1 (list 13 '())) '())
 (list (list 1 (list 13 empty))))

(check-expect
 (list '() '() (cons 1 '()))
 (list empty empty (list 1)))

(check-expect
 (cons "a" (cons (list 1) (list #false '())))
 (list "a" (list 1) #false empty))



;; Exercise 184

(check-expect
 (list (string=? "a" "b") #false)
 (list #false #false))

(check-expect
 (list (string=? "a" "b") #false)
 (cons #false (cons #false empty)))

(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (list 30 200 1/2))

(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (cons 30 (cons 200 (cons 1/2 empty))))

(check-expect
 (list "dana" "jane" "mary" "laura")
 (list "dana" "jane" "mary" "laura"))

(check-expect
 (list "dana" "jane" "mary" "laura")
 (cons "dana" (cons "jane" (cons "mary" (cons "laura" empty)))))

;;;; 11.2 Composing Functions

;; Exercise 185

(check-expect (first (list 1 2 3)) 1)

(check-expect (rest (list 1 2 3)) (list 2 3))

(check-expect (second (list 1 2 3)) 2)

;;;; 11.3 Auxiliary Functions that Recur

(check-expect (sort> '()) '())
(check-expect (sort>
               (cons 12 (cons 20 (cons -5 '()))))
               (cons 20 (cons 12 (cons -5 '()))))
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

; List-of-numbers -> List-of-numbers
; rearranges alon in descending order

(define (sort> alon)
  (cond [(empty? alon) empty]
        [else (insert (first alon)
                      (sort> (rest alon)))]))

;(sort> (list 12 20 -5))
;(first alon) ; 12
;(rest alon) ; (list 20 -5)
;
;(sort> (list 20 -5))
;(first alon) ; 20
;(rest alon) ; (list -5)
;
;(sort> (list -5))
;(first alon) ; -5
;(rest alon)  ; empty
;
;(sort> empty)
;(insert 12 (insert 20 (insert -5 '())))

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 2)) (list 5 2))
(check-expect (insert 5 (list 7)) (list 7 5))
(check-expect (insert 3 (list 5 4 2 1)) (list 5 4 3 2 1))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert 7 (list 6 5 4)) (list 7 6 5 4))
(check-expect (insert 0 (list 6 2 1 -1)) (list 6 2 1 0 -1))

;(insert 0 (list 6 2 1 -1))
;(cons 6 (insert 0 (list 2 1 -1)))

(define (insert n alon)
  (cond [(empty? alon) (cons n empty)]
        [(< (first alon) n) (cons n alon)]
        [else (cons (first alon) (insert n (rest alon)))]))

;; Exercise 186

(check-satisfied (sort> (list 3)) sorted>?)
(check-satisfied (sort> (list 2 3)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

(check-expect (same-elts? (list 1 2 3)
                          (list 3 1 2))
              #true)
(check-expect (same-elts? (list 1 2 4)
                          (list 3 1 2))
              #false)
; List-of-numbers List-of-numbers -> Boolean
; every element in lst1 also in lst2?
(define (same-elts? lst1 lst2)
  (cond [(empty? lst1) #true]
        [else (if (not (member? (first lst1) lst2))
                  #false
                  (same-elts? (rest lst1) lst2))]))

;;; TODO (check-satisfied (sort>/bad (list 1 2 3)) same-elts?)


;; Exercise 187

(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

(check-expect (sortgp> (list (make-gp "alice" 27)
                             (make-gp "bob" 12)
                             (make-gp "carl" 32)))
              (list (make-gp "carl" 32)
                    (make-gp "alice" 27)
                    (make-gp "bob" 12)))

; A-list-of-GamePlayers -> A-list-of-GamePlayers
(define (sortgp> alogp)
  (cond [(empty? alogp) empty]
        [else (insertgp (first alogp)
                      (sortgp> (rest alogp)))]))

(check-expect (insertgp (make-gp "meep" 12) empty)
              (list (make-gp "meep" 12)))

(check-expect (insertgp (make-gp "joe" 11)
                        (list (make-gp "max" 32)))
              (list (make-gp "max" 32) (make-gp "joe" 11)))

(check-expect (insertgp (make-gp "max" 32)
                        (list (make-gp "joe" 11)))
              (list (make-gp "max" 32) (make-gp "joe" 11)))

(check-expect (insertgp (make-gp "max" 12)
                        (list (make-gp "joe" 15)
                              (make-gp "ham" 7)))
              (list (make-gp "joe" 15)
                    (make-gp "max" 12)
                    (make-gp "ham" 7)))

(check-expect (insertgp (make-gp "max" 2)
                        (list (make-gp "joe" 15)
                              (make-gp "ham" 7)))
              (list (make-gp "joe" 15)
                    (make-gp "ham" 7)
                    (make-gp "max" 2)))

; GamePlayer -> A-list-of-GamePlayers
(define (insertgp gp alogp)
  (cond [(empty? alogp) (cons gp empty)]
        [(gp1-lt-gp2 (first alogp) gp)
         (cons gp alogp)]
        ;[(< (gp-score (first alogp)) (gp-score gp))
        ; (cons gp alogp)]
        [else (cons (first alogp)
                    (insertgp gp (rest alogp)))]))

; GamePlayer GamePlayer -> Boolean
(define (gp1-lt-gp2 gp1 gp2)
  (< (gp-score gp1) (gp-score gp2)))

;; Exercise 188

(define-struct email [from date message])
; An Email Message is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

(define em1 (make-email "mee" 123 "Anybody listening?"))
(define em2 (make-email "joe" 717 "Hello World"))
(define em3 (make-email "tim" 516 "Basic Student Language"))

(check-expect (sort>-email-by-name (list em1 em2 em3))
              (list em3 em1 em2))

(check-expect (sort>-email-by-date (list em1 em2 em3))
              (list em2 em3 em1))

;; TFW No higher order functions...

(define (sort>-email-by-name loe)
  (sort>-param loe "name"))

(define (sort>-email-by-date loe)
  (sort>-param loe "date"))

(define (sort>-param alon msg)
  (cond [(empty? alon) empty]
        [else
         (cond [(string=? msg "name")
                (insert-param-name (first alon)
                      (sort>-param (rest alon) msg))]
               [(string=? msg "date")
                (insert-param-date (first alon)
                      (sort>-param (rest alon) msg))])]))

(define (insert-param-name n alon)
  (cond [(empty? alon) (cons n empty)]
        [(string<? (email-from (first alon)) (email-from n)) (cons n alon)]
        [else (cons (first alon)
                    (insert-param-name n (rest alon)))]))

(define (insert-param-date n alon)
  (cond [(empty? alon) (cons n empty)]
        [(< (email-date (first alon)) (email-date n)) (cons n alon)]
        [else (cons (first alon)
                    (insert-param-date n (rest alon)))]))


;; Exercise 189

(check-expect (search 5 (list 1 3 2 5 4)) #true)
(check-expect (search 5 (list 1 3 2 6 4)) #false)

; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond [(empty? alon) #false]
        [else (or (= (first alon) n)
                  (search n (rest alon)))]))

(check-expect (search-sorted 5 empty) #false)
(check-expect (search-sorted 5 (list 3)) #false)
(check-expect (search-sorted 5 (list 5)) #true)
(check-expect (search-sorted 5 (list 7)) #false)
(check-expect (search-sorted 5 (list 6 4)) #false)
(check-expect (search-sorted 5 (list 6 5)) #true)
(check-expect (search-sorted 5 (list 5 4)) #true)
(check-expect (search-sorted 5 (list 7 6)) #false)
(check-expect (search-sorted 5 (list 4 3)) #false)
(check-expect (search-sorted 5 (list 8 6 5 3 1)) #true)
(check-expect (search-sorted 5 (list 8 6 4 3 1)) #false)

; Number List-of-numbers -> Boolean
; number occurs in a sorted list of numbers
; Assume: input list sorted in decr. order
(define (search-sorted n lon)
  (cond [(empty? lon) #false]
        [(< (first lon) n) #false]
        [(= (first lon) n) #true]
        [(> (first lon) n)
         (search-sorted n (rest lon))]
        [else (error "wot hpn?")]))


;; Exercise 190

(check-expect (prefixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")
                    empty))
(check-expect (prefixes (list "a" "b"))
              (list (list "a" "b")
                    (list "a")
                    empty))
(check-expect (prefixes (list "a"))
              (list (list "a")
                    empty))
(check-expect (prefixes (cons "a" empty))
              (cons (cons "a" empty)
                    (cons empty '())))
(check-expect (prefixes empty) (list empty))

; (listof 1String) -> (listof (listof 1String))
; generate all prefixes
(define (prefixes lst)
  (cond [(empty? lst) (cons empty empty)]
        [else (cons lst
                    (prefixes (all-but-last lst)))]))

(check-error  (all-but-last empty))
(check-expect (all-but-last (list "a")) empty)
(check-expect (all-but-last (list "a" "b")) (list "a"))
(check-expect (all-but-last (list "a" "b" "c"))
              (list "a" "b"))

; (listof 1String) -> (listof 1String)
(define (all-but-last lst)
  (cond [(empty? lst) (error "rm last of empty list")]
        [(empty? (rest lst)) empty]
        [else (cons (first lst)
                   (all-but-last (rest lst)))]))

(check-expect (suffixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list     "b" "c" "d")
                    (list         "c" "d")
                    (list             "d")
                    empty))
(check-expect (suffixes (list "a" "b"))
              (list (list "a" "b")
                    (list     "b")
                    empty))
(check-expect (suffixes (list "a"))
              (list (list "a")
                    empty))
(check-expect (suffixes (cons "a" empty))
              (cons (cons "a" empty)
                    (cons empty '())))
(check-expect (suffixes empty) (list empty))

; (listof 1String) -> (listof (listof 1String))
; generate all suffixes
(define (suffixes lst)
  (cond [(empty? lst) (cons empty empty)]
        [else (cons lst
                    (suffixes (rest lst)))]))

;;;; 11.4 Auxiliary Functions that Generalize

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)

; or
; A Polygon is one of:
; - (cons Posn (cons Posn (cons Posn '())))
; - (cons Posn Polygon)

(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))

(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

; a plain background image
(define MTS (empty-scene 50 50))

(check-expect
 (render-poly MTS triangle-p)
 (scene+line
  (scene+line
   (scene+line MTS 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))

(check-expect
 (render-poly MTS square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MTS 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

;scene+line img x x x x "red"
; Image Polygon -> Image
; renders the given polygon p into img
#;
(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list 1 2)) 2)
(check-expect (last (list 1)) 1)
; NELoP -> Posn
(define (last p)
  (cond [(empty? (rest p)) (first p)]
        [else (last (rest p))]))

(check-expect (render-line MTS
                           (make-posn 10 20)
                           (make-posn 30 40))
              (scene+line MTS
                          (posn-x (make-posn 10 20))
                          (posn-y (make-posn 10 20))
                          (posn-x (make-posn 30 40))
                          (posn-y (make-posn 30 40))
                          "red"))
; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line im
              (posn-x p) (posn-y p)
              (posn-x q) (posn-y q) "red"))

; An NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

#;
(check-expect (connect-dots MTS triangle-p)
              (scene+line
               (scene+line MTS 20 10 20 20 "red")
               20 20 30 20 "red"))

;; Exercise 191
#;
(check-expect (connect-dots MTS square-p)
              (scene+line
               (scene+line
                (scene+line MTS 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red"))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
#;
(define (connect-dots img p)
  (cond [(empty? (rest p)) MTS]
        [else (render-line
               (connect-dots img (rest p))
               (first p)
               (second p))]))

;; Exercise 193
#;
(define (render-poly img p)
  (connect-dots img (cons (last p) p)))
#;
(define (render-poly img p)
  (connect-dots img (add-at-end p (first p))))

;; Exercise 194

(define (render-poly img p)
  (connect-dots img p (first p)))

(define (connect-dots img p p0)
  (cond [(empty? (rest p))
         (render-line MTS (first p) p0)]
        [else (render-line
               (connect-dots img (rest p) p0)
               (first p)
               (second p))]))
