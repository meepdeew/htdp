(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

;;;; 9.2 Non-empty Lists

;; Exercise 145

; An NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

(check-expect (sorted>? empty) #true)
(check-expect (sorted>? (list 1)) #true)

(check-expect (sorted>? (list 1 2)) #false)
(check-expect (sorted>? (list 2 1)) #true)

(check-expect (sorted>? (list 1 2 3)) #false)
(check-expect (sorted>? (list 1 3 2)) #false)
(check-expect (sorted>? (list 2 3 1)) #false)
(check-expect (sorted>? (list 2 1 3)) #false)
(check-expect (sorted>? (list 3 1 2)) #false)
(check-expect (sorted>? (list 3 2 1)) #true)

; NEList-of-temperatures -> Boolean
(define (sorted>? lst)
  (cond [(empty? lst) #true]
        [(empty? (rest lst)) #true]
        [else (if (> (first lst) (first (rest lst)))
                   (sorted>? (rest lst))
                   #false)]))


;;;; 10.3 Lists in Lists, Files

; A LOS is one of:
; - '()
; - (cons String LOS)

; A LLS is one of:
; - '()
; - (cons LOS LLS)

(define line0 (cons "hello" (cons "world" empty)))
(define line1 empty)

(define lls0 empty)
(define lls1 (cons line0 (cons line1 empty)))

(check-expect (words-on-line lls0) empty)
(check-expect (words-on-line lls1) (cons 2 (cons 0 empty)))

; LLS -> List-of-numbers
; determines the number of words on each line
(define (words-on-line lls)
  (cond [(empty? lls) empty]
        [else (cons (length (first lls))
               ;(count-words (first lls))
                   (words-on-line (rest lls)))]))

(check-expect (count-words (cons "hello" (cons "world" empty))) 2)
(check-expect (count-words empty) 0)
; LOS -> Number
(define (count-words los)
  (cond [(empty? los) 0]
        [else (+ 1 (count-words (rest los)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistics file-name)
  (words-on-line
   (read-words/line file-name)))

(check-expect (collapse-line line0) "hello world")
(check-expect (collapse-line line1) "")

(define (collapse-line los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [else (string-append (first los)
                             " "
                             (collapse-line (rest los)))]))

; converts a list of lines into a string.
; The strings should be separated by blank spaces (" ").
; The lines should be separated with a newline ("\n").
(define (collapse lls)
  (cond [(empty? lls) "\n"]
        [else (string-append (collapse-line (first lls))
                             "\n"
                             (collapse (rest lls)))]))

;;; Exercise 176

; A Matrix is one of:
;   - (cons Row '())
;   - (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
;   - '()
;   - (cons Number Row)



(define row1 (cons 11 (cons 12 empty)))
(define row2 (cons 21 (cons 22 empty)))
(define mat1 (cons row1 (cons row2 empty)))

(check-expect (transpose (list (list 1)
                               (list 2)))
              (list (list 1 2)))
(check-expect (transpose (list (list 1 2 3)
                               (list 4 5 6)
                               (list 7 8 9)))
              (list (list 1 4 7)
                    (list 2 5 8)
                    (list 3 6 9)))
(define wor1 (cons 11 (cons 21 empty)))
(define wor2 (cons 12 (cons 22 empty)))
(define tam1 (cons wor1 (cons wor2 empty)))

(check-expect (transpose mat1) tam1)
; Matrix -> Matrix
; transposes the given matrix along the diagonal
(define (transpose lln)
  (cond
    [(empty? (first lln)) empty]
    [else (cons (first* lln)
                (transpose (rest* lln)))]))

(check-expect (first* mat1) (list 11 21))
(check-expect (first* tam1) (list 11 12))
; Matrix -> (listof Number)
; produces the first column as a list of numbers
(define (first* lst)
  (cond [(empty? lst) empty]
        [else (cons (first (first lst))
                    (first* (rest lst)))]))

(define rr1 (cons 12 empty))
(define rr2 (cons 22 empty))
(define rm1 (cons rr1 (cons rr2 empty)))
(define rw1 (cons 21 empty))
(define rw2 (cons 22 empty))
(define rt1 (cons rw1 (cons rw2 empty)))

(check-expect (rest* mat1) rm1)
(check-expect (rest* tam1) rt1)
; Matrix -> Matrix
; removes the first column
(define (rest* lst)
  (cond [(empty? lst) empty]
        [else (cons (rest (first lst))
                    (rest* (rest lst)))]))

;;;; 10.4 A Graphical Editor, Revisited

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" empty)))))
(define all
  (cons "a" (cons "l" (cons "l" empty))))
(define lla
 (cons "l" (cons "l" (cons "a" empty))))

; data example 1:
;(make-editor all good)

; data example 2:
;(make-editor lla good)

; (listof 1String) -> String
;(implode (list "a" "l" "l" " " "g" "r" "a" "v" "y"))

; String -> (listof 1String)
;(explode "howdy")



; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect (rev (cons "a" empty)) (cons "a" empty))
(check-expect (rev (cons "a" (cons "b" empty)))
              (cons "b" (cons "a" empty)))
(check-expect
 (rev (cons "a" (cons "b" (cons "c" empty))))
 (cons "c" (cons "b" (cons "a" empty))))

(define (rev l)
  (cond [(empty? l) empty]
        [else (add-at-end (rev (rest l)) (first l))]))

(check-expect (add-at-end empty "f") (cons "f" empty))

(check-expect (add-at-end (cons "d" empty) "e")
              (cons "d" (cons "e" empty)))

(check-expect (add-at-end (cons "a" (cons "b" empty)) "c")
              (cons "a" (cons "b" (cons "c" empty))))

; Lo1s 1String -> Lo1s
(define (add-at-end l s)
  (cond [(empty? l) (cons s empty)]
        [else (cons (first l)
                   (add-at-end (rest l) s))]))


(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))

; String String -> Editor
(define (create-editor str1 str2)
  (make-editor (explode str1) (explode str2)))


(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))

(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))

(check-expect (editor-kh (create-editor "abc" "fgh") "left")
              (create-editor "ab" "cfgh"))
(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))

(check-expect (editor-kh (create-editor "abc" "fgh") "right")
              (create-editor "abcf" "gh"))
(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))

(check-expect (editor-kh (create-editor "hello" " world") "\b")
              (create-editor "hell" " world"))

(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed ke)
  (cond [(key=? ke "\b")    (editor-del ed)]
        [(key=? ke "left")  (editor-lft ed)]
        [(key=? ke "right") (editor-rgt ed)]
        [(key=? ke "\t") ed]
        [(key=? ke "\r") ed]
        [(= (string-length ke) 1) (editor-ins ed ke)]
        [else (error "editor-kh called with invalid arg")]))
;(index "editor-kh")

(check-expect (remove-last-elt empty) empty)
(check-expect (remove-last-elt (list "a"))
              empty)
(check-expect (remove-last-elt (list "a" "b"))
              (list "a"))
(check-expect (remove-last-elt (list "a" "b" "c"))
              (list "a" "b"))

(define (remove-last-elt lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) empty]
        [else (cons (first lst)
                    (remove-last-elt (rest lst)))]))

(check-expect (get-last-elt empty) empty)
(check-expect (get-last-elt (list "a")) "a")
(check-expect (get-last-elt (list "a" "b")) "b")
(check-expect (get-last-elt (list "a" "b" "c")) "c")

(define (get-last-elt lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (first lst)]
        [else (get-last-elt (rest lst))]))

(check-expect (editor-lft (make-editor empty empty))
              (make-editor empty empty))

(check-expect (editor-lft (make-editor (list "a") empty))
              (make-editor empty (list "a")))

(check-expect (editor-lft (make-editor (list "a" "b") empty))
              (make-editor (list "a") (list "b")))

; Editor -> Editor
(define (editor-lft ed)
  (if (= (length (editor-pre ed)) 0)
      ed
      (make-editor
       (remove-last-elt (editor-pre ed))
       (cons (get-last-elt (editor-pre ed))
             (editor-post ed)))))

(check-expect (editor-rgt (make-editor empty empty))
              (make-editor empty empty))

(check-expect (editor-rgt (make-editor empty (list "a")))
              (make-editor (list "a") empty))

(check-expect (editor-rgt (make-editor empty (list "a" "b")))
              (make-editor (list "a") (list "b")))

; Editor -> Editor
(define (editor-rgt ed)
  (if (= (length (editor-post ed)) 0)
      ed
      (make-editor
       (add-at-end (editor-pre ed) (first (editor-post ed)))
       (rest (editor-post ed)))))


(check-expect (editor-del (make-editor empty empty))
              (make-editor empty empty))

(check-expect (editor-del (make-editor (cons "a" empty) empty))
              (make-editor empty empty))

(check-expect (editor-del (make-editor (cons "a" (cons "b" empty)) empty))
              (make-editor (cons "a" empty) empty))

(check-expect (editor-del (make-editor (cons "a" (cons "b" (cons "c" empty))) empty))
              (make-editor (cons "a" (cons "b" empty)) empty))


(check-expect (editor-del (make-editor empty empty))
              (make-editor empty empty))
(check-expect (editor-del (make-editor (list "a") empty))
              (make-editor empty empty))
(check-expect (editor-del (make-editor (list "a" "b") empty))
              (make-editor (list "a") empty))

; Editor -> Editor
(define (editor-del ed)
  (if (= (length (editor-pre ed)) 0)
      ed
      (make-editor
       (remove-last-elt (editor-pre ed))
       (editor-post ed))))


(check-expect (editor-ins (make-editor empty empty) "e")
              (make-editor (cons "e" empty) empty))
(check-expect
 (editor-ins (make-editor (cons "d" empty)
                          (cons "f" (cons "g" empty))) "e")
              (make-editor (cons "d" (cons "e" empty))
                           (cons "f" (cons "g" empty))))

; Editor KeyEvent(1String) -> Editor
; insert the 1String k between pre and post
(define (editor-ins ed k)
  (make-editor (add-at-end (editor-pre ed) k)
               (editor-post ed)))

; Editor -> Image
; renders an editor as an image of the two texts
; seperated by the cursor
(define (editor-render e)
    (overlay/align
     "left" "center"
     (beside (text (implode (editor-pre e)) 11 "black")
             CURSOR
             (text (implode (editor-post e)) 11 "black"))
     (empty-scene 200 20)))
