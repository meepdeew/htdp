;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname arrangements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require test-engine/racket-tests)

(define (insert-at-end item listo)
  (reverse (cons item (reverse listo))))

; 1String Word Word ->  gross?
(define (insert-in-word letter w-left w-used)
  (cond
    [(empty? w-left) (list (append w-used (list letter)))]
    [else
     (append (list (append w-used (list letter) w-left))
             (insert-in-word letter (rest w-left)
                             (insert-at-end (first w-left) w-used)))]))

; !String Word -> 
(define (insert-everywhere/in-this-word letter w)
  (insert-in-word letter w (list)))

; 1String List-of-words ->
(define (insert-everywhere/in-all-words letter w)
  (cond [(empty? w) '()]
        [else (append (insert-everywhere/in-this-word letter (first w))
                      (insert-everywhere/in-all-words letter (rest w)))]))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words
           (first w) (arrangements (rest w)))]))


;; insert-in-word tests

(check-expect (insert-in-word "x" (list "e") (list "r"))
              (list (list "r" "x" "e")
                    (list "r" "e" "x")))

(check-expect (insert-in-word "x" (list "r" "e") (list))
              (list (list "x" "r" "e")
                    (list "r" "x" "e")
                    (list "r" "e" "x")))

(check-expect (insert-in-word "x" (list "r") (list))
              (list (list "x" "r") (list "r" "x")))

(check-expect (insert-in-word "x" (list) (list "r"))
              (list (list "r" "x")))

;; insert-everywhere/in-this-word tests

(check-expect (insert-everywhere/in-this-word "x" (list "r" "e"))
              (list (list "x" "r" "e")
                    (list "r" "x" "e")
                    (list "r" "e" "x")))
(check-expect (insert-everywhere/in-this-word "x" (list "a"))
              (list (list "x" "a")
                    (list "a" "x")))
(check-expect (insert-everywhere/in-this-word "x" (list))
              (list (list "x")))

;; inser-everywhere/in-all-words tests
(check-expect
 (insert-everywhere/in-all-words "d" (list (list "e" "r") (list "r" "e")))
 (list (list "d" "e" "r")(list "e" "d" "r")(list "e" "r" "d")
       (list "d" "r" "e")(list "r" "d" "e")(list "r" "e" "d")))

(check-expect
 (insert-everywhere/in-all-words "q" (list (list "a")))
 (list (list "q" "a") (list "a" "q")))

(check-expect
 (insert-everywhere/in-all-words "o" (cons (list) '()))
 (list (list "o")))

;; So, as long as you're given the list of subpermutations, it holds.

; Word of length 5

(check-expect
 (arrangements (list "h" "o" "w" "d" "y"))
 (insert-everywhere/in-all-words "h" (arrangements (list "o" "w" "d" "y"))))

(check-expect
 (insert-everywhere/in-all-words "h" (arrangements (list "o" "w" "d" "y")))
 (insert-everywhere/in-all-words
  "h"
  (list
  (list "o" "w" "d" "y") (list "w" "o" "d" "y") (list "w" "d" "o" "y") (list "w" "d" "y" "o")
  (list "o" "d" "w" "y") (list "d" "o" "w" "y") (list "d" "w" "o" "y") (list "d" "w" "y" "o")
  (list "o" "d" "y" "w") (list "d" "o" "y" "w") (list "d" "y" "o" "w") (list "d" "y" "w" "o")
  (list "o" "w" "y" "d") (list "w" "o" "y" "d") (list "w" "y" "o" "d") (list "w" "y" "d" "o")
  (list "o" "y" "w" "d") (list "y" "o" "w" "d") (list "y" "w" "o" "d") (list "y" "w" "d" "o")
  (list "o" "y" "d" "w") (list "y" "o" "d" "w") (list "y" "d" "o" "w") (list "y" "d" "w" "o"))))

(check-expect
 (insert-everywhere/in-all-words
  "h"
  (list
  (list "o" "w" "d" "y") (list "w" "o" "d" "y") (list "w" "d" "o" "y") (list "w" "d" "y" "o")
  (list "o" "d" "w" "y") (list "d" "o" "w" "y") (list "d" "w" "o" "y") (list "d" "w" "y" "o")
  (list "o" "d" "y" "w") (list "d" "o" "y" "w") (list "d" "y" "o" "w") (list "d" "y" "w" "o")
  (list "o" "w" "y" "d") (list "w" "o" "y" "d") (list "w" "y" "o" "d") (list "w" "y" "d" "o")
  (list "o" "y" "w" "d") (list "y" "o" "w" "d") (list "y" "w" "o" "d") (list "y" "w" "d" "o")
  (list "o" "y" "d" "w") (list "y" "o" "d" "w") (list "y" "d" "o" "w") (list "y" "d" "w" "o")))
 (list
  (list "h" "o" "w" "d" "y") (list "o" "h" "w" "d" "y") (list "o" "w" "h" "d" "y")
  (list "o" "w" "d" "h" "y") (list "o" "w" "d" "y" "h")
  (list "h" "w" "o" "d" "y") (list "w" "h" "o" "d" "y") (list "w" "o" "h" "d" "y")
  (list "w" "o" "d" "h" "y") (list "w" "o" "d" "y" "h")
  (list "h" "w" "d" "o" "y") (list "w" "h" "d" "o" "y") (list "w" "d" "h" "o" "y")
  (list "w" "d" "o" "h" "y") (list "w" "d" "o" "y" "h")
  (list "h" "w" "d" "y" "o") (list "w" "h" "d" "y" "o") (list "w" "d" "h" "y" "o")
  (list "w" "d" "y" "h" "o") (list "w" "d" "y" "o" "h")
  (list "h" "o" "d" "w" "y") (list "o" "h" "d" "w" "y") (list "o" "d" "h" "w" "y")
  (list "o" "d" "w" "h" "y") (list "o" "d" "w" "y" "h")
  (list "h" "d" "o" "w" "y") (list "d" "h" "o" "w" "y") (list "d" "o" "h" "w" "y")
  (list "d" "o" "w" "h" "y") (list "d" "o" "w" "y" "h")
  (list "h" "d" "w" "o" "y") (list "d" "h" "w" "o" "y") (list "d" "w" "h" "o" "y")
  (list "d" "w" "o" "h" "y") (list "d" "w" "o" "y" "h")
  (list "h" "d" "w" "y" "o") (list "d" "h" "w" "y" "o") (list "d" "w" "h" "y" "o")
  (list "d" "w" "y" "h" "o") (list "d" "w" "y" "o" "h")
  (list "h" "o" "d" "y" "w") (list "o" "h" "d" "y" "w") (list "o" "d" "h" "y" "w")
  (list "o" "d" "y" "h" "w") (list "o" "d" "y" "w" "h")
  (list "h" "d" "o" "y" "w") (list "d" "h" "o" "y" "w") (list "d" "o" "h" "y" "w")
  (list "d" "o" "y" "h" "w") (list "d" "o" "y" "w" "h")
  (list "h" "d" "y" "o" "w") (list "d" "h" "y" "o" "w") (list "d" "y" "h" "o" "w")
  (list "d" "y" "o" "h" "w") (list "d" "y" "o" "w" "h")
  (list "h" "d" "y" "w" "o") (list "d" "h" "y" "w" "o") (list "d" "y" "h" "w" "o")
  (list "d" "y" "w" "h" "o") (list "d" "y" "w" "o" "h")
  (list "h" "o" "w" "y" "d") (list "o" "h" "w" "y" "d") (list "o" "w" "h" "y" "d")
  (list "o" "w" "y" "h" "d") (list "o" "w" "y" "d" "h")
  (list "h" "w" "o" "y" "d") (list "w" "h" "o" "y" "d") (list "w" "o" "h" "y" "d")
  (list "w" "o" "y" "h" "d") (list "w" "o" "y" "d" "h")
  (list "h" "w" "y" "o" "d") (list "w" "h" "y" "o" "d") (list "w" "y" "h" "o" "d")
  (list "w" "y" "o" "h" "d") (list "w" "y" "o" "d" "h") 
  (list "h" "w" "y" "d" "o") (list "w" "h" "y" "d" "o") (list "w" "y" "h" "d" "o")
  (list "w" "y" "d" "h" "o") (list "w" "y" "d" "o" "h") 
  (list "h" "o" "y" "w" "d") (list "o" "h" "y" "w" "d") (list "o" "y" "h" "w" "d")
  (list "o" "y" "w" "h" "d") (list "o" "y" "w" "d" "h")
  (list "h" "y" "o" "w" "d") (list "y" "h" "o" "w" "d") (list "y" "o" "h" "w" "d")
  (list "y" "o" "w" "h" "d") (list "y" "o" "w" "d" "h")
  (list "h" "y" "w" "o" "d") (list "y" "h" "w" "o" "d") (list "y" "w" "h" "o" "d")
  (list "y" "w" "o" "h" "d") (list "y" "w" "o" "d" "h")
  (list "h" "y" "w" "d" "o") (list "y" "h" "w" "d" "o") (list "y" "w" "h" "d" "o")
  (list "y" "w" "d" "h" "o") (list "y" "w" "d" "o" "h")
  (list "h" "o" "y" "d" "w") (list "o" "h" "y" "d" "w") (list "o" "y" "h" "d" "w")
  (list "o" "y" "d" "h" "w") (list "o" "y" "d" "w" "h")
  (list "h" "y" "o" "d" "w") (list "y" "h" "o" "d" "w") (list "y" "o" "h" "d" "w")
  (list "y" "o" "d" "h" "w") (list "y" "o" "d" "w" "h")
  (list "h" "y" "d" "o" "w") (list "y" "h" "d" "o" "w") (list "y" "d" "h" "o" "w")
  (list "y" "d" "o" "h" "w") (list "y" "d" "o" "w" "h")
  (list "h" "y" "d" "w" "o") (list "y" "h" "d" "w" "o") (list "y" "d" "h" "w" "o")
  (list "y" "d" "w" "h" "o") (list "y" "d" "w" "o" "h")))

;; ; Word of length 4

(check-expect
 (arrangements (list "o" "w" "d" "y"))
 (insert-everywhere/in-all-words "o" (arrangements (list "w" "d" "y"))))

(check-expect
 (insert-everywhere/in-all-words "o" (arrangements (list "w" "d" "y")))
 (insert-everywhere/in-all-words
  "o" (list (list "w" "d" "y") (list "d" "w" "y") (list "d" "y" "w")
            (list "w" "y" "d") (list "y" "w" "d") (list "y" "d" "w"))))

(check-expect
 (insert-everywhere/in-all-words
  "o" (list (list "w" "d" "y") (list "d" "w" "y") (list "d" "y" "w")
            (list "w" "y" "d") (list "y" "w" "d") (list "y" "d" "w")))
 (list
  (list "o" "w" "d" "y") (list "w" "o" "d" "y") (list "w" "d" "o" "y") (list "w" "d" "y" "o")
  (list "o" "d" "w" "y") (list "d" "o" "w" "y") (list "d" "w" "o" "y") (list "d" "w" "y" "o")
  (list "o" "d" "y" "w") (list "d" "o" "y" "w") (list "d" "y" "o" "w") (list "d" "y" "w" "o")
  (list "o" "w" "y" "d") (list "w" "o" "y" "d") (list "w" "y" "o" "d") (list "w" "y" "d" "o")
  (list "o" "y" "w" "d") (list "y" "o" "w" "d") (list "y" "w" "o" "d") (list "y" "w" "d" "o")
  (list "o" "y" "d" "w") (list "y" "o" "d" "w") (list "y" "d" "o" "w") (list "y" "d" "w" "o")))

;; ; Word of length 3

(check-expect (arrangements (list "w" "d" "y"))
              (insert-everywhere/in-all-words "w" (arrangements (list "d" "y"))))

(check-expect (insert-everywhere/in-all-words "w" (arrangements (list "d" "y")))
              (insert-everywhere/in-all-words "w" (list (list "d" "y") (list "y" "d"))))

(check-expect (insert-everywhere/in-all-words "w" (list (list "d" "y") (list "y" "d")))
              (list (list "w" "d" "y") (list "d" "w" "y") (list "d" "y" "w")
                    (list "w" "y" "d") (list "y" "w" "d") (list "y" "d" "w")))

;; ; Word of length 2

(check-expect (arrangements (list "d" "y"))
              (insert-everywhere/in-all-words "d" (arrangements (list "y"))))

(check-expect (insert-everywhere/in-all-words "d" (arrangements (list "y")))
              (insert-everywhere/in-all-words "d" (list (list "y"))))

(check-expect (insert-everywhere/in-all-words "d" (list (list "y")))
              (list (list "d" "y") (list "y" "d")))

;; ; Word of length 1

(check-expect (arrangements (list "y"))
              (insert-everywhere/in-all-words "y" (arrangements (list))))

(check-expect (insert-everywhere/in-all-words "y" (arrangements (list)))
              (insert-everywhere/in-all-words "y" (list (list))))

(check-expect (insert-everywhere/in-all-words "y" (list (list)))
              (list (list "y")))

;; ; empty (no recurse)

(check-expect (arrangements (list))
              (list (list)))

(test)

