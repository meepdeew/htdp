;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname intermezzo-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 231

(check-expect '(1 "a" 2 #false 3 "c")
              (list '1 '"a" '2 '#false '3 '"c"))
(check-expect
 '(1 "a" 2 #false 3 "c")
 (cons '1 (cons '"a" (cons '2 (cons '#false (cons '3 (cons '"c" '())))))))

(check-expect '() (list))
(check-expect '() empty)

(check-expect '(("alan" 1000) ("barb" 2000) ("carl" 1500))
              (list (list '"alan" '1000)
                    (list '"barb" 2000)
                    (list '"carl" 1500)))
(check-expect '(("alan" 1000)
                ("barb" 2000)
                ("carl" 1500))
              (cons (cons '"alan" (cons '1000 empty))
                    (cons (cons '"barb" (cons '2000 empty))
                          (cons (cons '"carl" (cons 1500 empty)) empty))))

(define x 42)
(check-expect '(40 41 x 42 43)
              (list 40 41 'x 42 43))

(check-expect `(1 2 3)
              (list 1 2 3))

(check-expect `("a" "b" "c")
              (list "a" "b" "c"))

(check-expect `(#true "hello world" 42)
              (list #true "hello world" 42))

(check-expect `(40 41 ,x 43 44)
              (quasiquote (40 41 (unquote x) 43 44)))

(check-expect `(1 ,(+ 1 1) 3)
              (quasiquote (1 (unquote (+ 1 1)) 3)))



(check-expect `(1 ,(+ 1 1) 3)
              (list `1 `,(+ 1 1) `3))

(check-expect (list `1 `,(+ 1 1) `3)
              (list 1 (+ 1 1) 3))

(check-expect (list 1 (+ 1 1) 3)
              (list 1 2 3))

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
    (head
     (title ,title)
     (meta ((http-equiv "content-type")
            (content "text-html"))))
    (body
     (h1 ,title)
     (p "I, " ,author ", made this page."))))

;; Exercise 232

(check-expect `(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,(string-append "carl" " , the great") 1500)
                ("dawn" 2300))
              (list (list "alan" (* 2 500))
                    (list "barb" 2000)
                    (list (string-append "carl" " , the great") 1500)
                    (list "dawn" 2300)))

(define title "ratings")

(check-expect `(html
                (head
                 (title ,title))
                (body
                 (h1 ,title)
                 (p "A second web page")))
              (list `html
                    `(head
                      (title ,title))
                    `(body
                      (h1 ,title)
                      (p "A second web page"))))
(check-expect (list `html
                    `(head
                      (title ,title))
                    `(body
                      (h1 ,title)
                      (p "A second web page")))
              (list `html
                    (list `head (list `title title))
                    (list `body
                          (list `h1 title)
                          (list `p "A second web page"))))

;;; Unquote Splice

;; if make-row produces a list
;; this list becomes the second item of a lis

(define make-row.v1 identity)

(check-expect `(tr
                ,(make-row.v1
                  '(3 4 5)))
              ; is short for
              (list `tr
                    (make-row.v1
                     (list 3 4 5))))

; (listof Number) -> (listof String)
(define (lon->los lst)
  (cond [(empty? lst) empty]
        [else (cons (number->string (first lst))
                    (make-row.v2 (rest lst)))]))

; (listof Number) -> (listof String)
(define (make-row.v2 lst)
  (lon->los lst))

;; we may want to splice such a nested list into the outer one
;; (list 'tr "3" "4" "5") instead of
;; (list 'tr (list "3" "4" "5"))

(check-expect (cons 'tr (make-row.v2 '(3 4 5)))
              (list 'tr "3" "4" "5"))

(check-expect (cons `tr (make-row.v2 '(3 4 5)))
              (list 'tr "3" "4" "5"))

(check-expect `(tr ,@(make-row.v2 '(3 4 5)))
              (cons `tr (make-row.v2 '(3 4 5))))

(check-expect `(tr ,@(make-row.v2 '(3 4 5)))
              (list 'tr "3" "4" "5"))

; so backquote can translate into
; (list `other `things ...) or
; (cons `other `things ...)

(check-expect (list "a" "b")
              (cons "a" (list "b")))
(check-expect (cons "a" (cons "b" (cons "c" empty)))
              (cons "a" (list "b" "c")))

; (define (make-row lst) lst)

; so if
; "quote-spliced"
; `(tr ,@(make-row '(3 4 5)))
; translates into
; (cons 'tr (make-row '(3 4 5)))

; and
; "unquoted"
; `(tr ,(make-row '(3 4 5)))
; translates into
; (list 'tr (make-row '(3 4 5)))

; what is a quasiquoted expression contains
; both an unquote and an unquote-splice?
; ex
; "unquoted then quote-spliced"
; `(tra ,(make-row '(3 4 5)) trb ,@(make-row '(3 4 5)))
; (list 'tra (make-row '(3 4 5)) 'trb (make-row '(3 4 5)))
; or
; "quote-spliced then unquoted"
; `(tra ,@(make-row '(3 4 5)) trb ,(make-row '(3 4 5)))
; (cons 'tra (cons (make-row '(3 4 5)) (cons 'trb (make-row '(3 4 5)))))



; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from 1
(define (make-row l)
  (cond [(empty? l) empty]
        [else (cons (make-cell (first l))
                    (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

; (make-table '(1 2 3 4) '(2.8 -1.1 3.4 1.3))
#;
`(table ((border "1"))
        (tr (td "1") (td "2") (td "3") (td "4"))
        (tr (td "2.8") (td "-1.1") (td "3.4") (td "1.3")))


;; Exercise 233

(check-expect
 `(0 ,@'(1 2 3) 4)
 (list 0 1 2 3 4))


(check-expect
 `(("alan" ,(* 2 500))
   ("barb" 2000)
   (,@'("carl" ", the great") 1500)
   ("dawn" 2300))
 (list (list "alan" (* 2 500))
       (list "barb" 2000)
       (list "carl" ", the great" 1500)
       (list "dawn" 2300)))

(check-expect
 `(html
   (body
    (table ((border "1"))
           (tr ((width "200"))
               ,@(make-row '(1 2)))
           (tr ((width "200"))
               ,@(make-row '(99 65))))))
 (list 'html
       (list 'body
             (list 'table (list (list 'border "1"))
                   (list 'tr (list (list 'width "200"))
                         (list 'td "1") (list 'td "2"))
                   (list 'tr (list (list 'width "200"))
                         (list 'td "99") (list 'td "65"))))))


;; Exercise 234

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from 1
(define (make-ranking-row l)
  (list 'tr
        (make-ranking-idx (first l))
        (make-ranking-cell (second l))))

; Number -> ... nested list ...
; creates the 1st cell for an HTML table from a number
(define (make-ranking-idx n)
  `(td , (number->string n)))

; String -> ... nested list ...
; creates the 2nd cell for an HTML table from a string
(define (make-ranking-cell n)
  `(td , n))

(check-expect
 (make-ranking one-list)
 `(table ((border "1"))
         (tr (td "1") (td "Asia: Heat of the Moment"))
         (tr (td "2") (td "U2: One"))
         (tr (td "3") (td "The White Stripes: Seven Nation Army"))))

; (listof String) -> ... nested list ...
; consumes a list of ranked song titles
; produces a list representation of an HTML table
(define (make-ranking lts)
  `(table ((border "1"))
          ,@(make-ranking-trs (ranking lts))))

; (listof (listof Number String)) -> (listof (list td))
(define (make-ranking-trs lol)
  (cond [(empty? lol) empty]
        [else (cons (make-ranking-row (first lol))
                    (make-ranking-trs (rest lol)))]))

; (listof String) -> (listof (listof Number String))
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; (listof String) -> (listof (listof Number String))
(define (add-ranks los)
  (cond [(empty? los) empty]
        [else (cons (list (length los) (first los))
                    (add-ranks (rest los)))]))

