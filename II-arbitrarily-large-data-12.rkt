;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname II-arbitrarily-large-data-ch12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/itunes)

;;;; 12 Projects: Lists

;;;; 12.1 Real-World Data: Dictionaries

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

;; Exercise 195

(check-expect (starts-with# "a" empty) 0)
(check-expect (starts-with# "a" (list "apple")) 1)
(check-expect (starts-with# "a" (list "grapple")) 0)
(check-expect (starts-with# "a" (list "apple" "grapple")) 1)
(check-expect (starts-with# "a" (list "grapple" "apple")) 1)
(check-expect (starts-with# "b" (list "banana" "bunch")) 2)
(check-expect (starts-with# "b" (list "apple" "banana" "bunch")) 2)
(check-expect (starts-with# "b" (list "banana" "apple" "bunch")) 2)
(check-expect (starts-with# "b" (list "banana" "bunch" "apple")) 2)
(check-expect (starts-with# "b" (list "grapple" "banana" "apple" "bunch" "toast")) 2)

; Letter Dictionary -> Number
(define (starts-with# ltr dict)
  (cond [(empty? dict) 0]
        [else (+ (if (string=? ltr (string-ith (string-downcase (first dict)) 0)) 1 0)
                   (starts-with# ltr (rest dict)))]))

;(starts-with# "e" AS-LIST);8736
;(starts-with# "z" AS-LIST);949


; Exercise 196

(check-expect
 (count-by-letter (list "apple"))
 (list (make-res "a" 1) (make-res "b" 0) (make-res "c" 0) (make-res "d" 0)
       (make-res "e" 0) (make-res "f" 0) (make-res "g" 0) (make-res "h" 0)
       (make-res "i" 0) (make-res "j" 0) (make-res "k" 0) (make-res "l" 0)
       (make-res "m" 0) (make-res "n" 0) (make-res "o" 0) (make-res "p" 0)
       (make-res "q" 0) (make-res "r" 0) (make-res "s" 0) (make-res "t" 0)
       (make-res "u" 0) (make-res "v" 0) (make-res "w" 0) (make-res "x" 0)
       (make-res "y" 0) (make-res "z" 0)))

(check-expect
 (count-by-letter (list "grapple" "banana" "apple" "bunch" "toast"))
 (list (make-res "a" 1) (make-res "b" 2) (make-res "c" 0) (make-res "d" 0)
       (make-res "e" 0) (make-res "f" 0) (make-res "g" 1) (make-res "h" 0)
       (make-res "i" 0) (make-res "j" 0) (make-res "k" 0) (make-res "l" 0)
       (make-res "m" 0) (make-res "n" 0) (make-res "o" 0) (make-res "p" 0)
       (make-res "q" 0) (make-res "r" 0) (make-res "s" 0) (make-res "t" 1)
       (make-res "u" 0) (make-res "v" 0) (make-res "w" 0) (make-res "x" 0)
       (make-res "y" 0) (make-res "z" 0)))

(define-struct res [letter count])
; A Letter-Count is a structure (make-res 1String Number)
; interpretation (make-res ltr num) means there were num
; occurrences of ltr found in the given input dictionary.

; Dictionary -> (listof Letter-Counts)
(define (count-by-letter dict)
  (loop-through-letters LETTERS dict))

; (listof 1Strings) Dictionary -> (listof Letter-Counts)
(define (loop-through-letters ltrs dict)
  (cond [(empty? ltrs) empty]
        [else (cons (make-res-for-ltr (first ltrs) dict)
                    (loop-through-letters (rest ltrs) dict))]))

; 1String Dictionary -> Letter-Count
(define (make-res-for-ltr ltr dict)
  (make-res ltr (starts-with# ltr dict)))

;(count-by-letter AS-LIST)
;(list (make-res "a" 17096) (make-res "b" 11070) (make-res "c" 19901)
;      (make-res "d" 10896) (make-res "e" 8736) (make-res "f" 6860)
;      (make-res "g" 6861) (make-res "h" 9027) (make-res "i" 8799)
;      (make-res "j" 1642) (make-res "k" 2281) (make-res "l" 6284)
;      (make-res "m" 12616) (make-res "n" 6780) (make-res "o" 7849)
;      (make-res "p" 24461) (make-res "q" 1152) (make-res "r" 9671)
;      (make-res "s" 25162) (make-res "t" 12966) (make-res "u" 16387)
;      (make-res "v" 3440) (make-res "w" 3944) (make-res "x" 385)
;      (make-res "y" 671) (make-res "z" 949))


;; Exercise 197

(check-expect
 (most-frequent (list "apple"))
 (make-res "a" 1))

(check-expect
 (most-frequent (list "grapple" "banana" "apple" "bunch" "toast"))
 (make-res "b" 2))

; Dictionary -> Letter-Count
(define (most-frequent dict)
  (max-lc (count-by-letter dict)))


;; Slooow. TODO Come back to see difference with local variables
;(most-frequent AS-LIST) ; (make-res "s" 25162)



(check-expect
 (max-lc
  (list (make-res "a" 1) (make-res "b" 0) (make-res "c" 0) (make-res "d" 0)
        (make-res "e" 0) (make-res "f" 0) (make-res "g" 0) (make-res "h" 0)
        (make-res "i" 0) (make-res "j" 0) (make-res "k" 0) (make-res "l" 0)
        (make-res "m" 0) (make-res "n" 0) (make-res "o" 0) (make-res "p" 0)
        (make-res "q" 0) (make-res "r" 0) (make-res "s" 0) (make-res "t" 0)
        (make-res "u" 0) (make-res "v" 0) (make-res "w" 0) (make-res "x" 0)
        (make-res "y" 0) (make-res "z" 0)))
 (make-res "a" 1))

(check-expect
 (max-lc
  (list (make-res "a" 1) (make-res "b" 2) (make-res "c" 0) (make-res "d" 0)
        (make-res "e" 0) (make-res "f" 0) (make-res "g" 1) (make-res "h" 0)
        (make-res "i" 0) (make-res "j" 0) (make-res "k" 0) (make-res "l" 0)
        (make-res "m" 0) (make-res "n" 0) (make-res "o" 0) (make-res "p" 0)
        (make-res "q" 0) (make-res "r" 0) (make-res "s" 0) (make-res "t" 1)
        (make-res "u" 0) (make-res "v" 0) (make-res "w" 0) (make-res "x" 0)
        (make-res "y" 0) (make-res "z" 0)))
 (make-res "b" 2))

; (listof Letter-Count) -> Letter-Count
(define (max-lc lolc)
  (cond [(empty? lolc) (make-res "term" 0)]
        [else (if (>= (res-count (first lolc))
                     (res-count (max-lc (rest lolc))))
                  (first lolc)
                  (max-lc (rest lolc)))]))



(check-expect (max-me (list 2 3 1 5 2 9 2)) 9)
(check-expect (max-me (list 4)) 4)
(check-expect (max-me (list 4 2)) 4)
(check-expect (max-me (list 3 6)) 6)
(check-expect (max-me (list 3 4 2)) 4)
(check-expect (max-me (list 3 2 4)) 4)
(check-expect (max-me (list 2 3 4)) 4)
(check-expect (max-me (list 2 4 3)) 4)
(check-expect (max-me (list 4 2 3)) 4)
; (listof Number) -> Number
(define (max-me lon)
  (cond [(empty? lon) 0]
        [else (if (>= (first lon)
                      (max-me (rest lon)))
                  (first lon)
                  (max-me (rest lon)))]))

;(max-me (list 2 3))
;(if (>= 2 (if (>= 3 0) 3 0)) 2 (if (>= 3 0) 3 0))



;; Exercise 198

(check-expect (words-by-letter (list "ab" "ba" "ce" "de" "feet"))
              (list (list "ab") (list "ba") (list "ce") (list "de")
                    empty (list "feet") empty empty empty empty
                    empty empty empty empty empty empty
                    empty empty empty empty empty empty
                    empty empty empty empty))

; Dictionary -> (listof Dictionary)
(define (words-by-letter dict)
  (words-all-letters LETTERS dict))

(check-expect (words-all-letters LETTERS (list "ab" "ba" "ce" "de" "feet"))
              (list (list "ab") (list "ba") (list "ce") (list "de")
                    empty (list "feet") empty empty empty empty
                    empty empty empty empty empty empty
                    empty empty empty empty empty empty
                    empty empty empty empty))

; (listof 1String) Dictionary -> (listof Dictionary)
(define (words-all-letters ltrs dict)
  (cond [(empty? ltrs) empty]
        [else (cons (words-single-letter (first ltrs) dict)
                    (words-all-letters (rest ltrs) dict))]))

(check-expect (words-single-letter "a" (list "abe" "jam" "ale"))
              (list "abe" "ale"))
(check-expect (words-single-letter "b" (list "abe" "jam" "ale"))
              empty)
(check-expect (words-single-letter "j" (list "abe" "jam" "ale"))
              (list "jam"))
; 1String Dictionary -> Dictionary
(define (words-single-letter ltr dict)
  (cond [(empty? dict) empty]
        [else
         (if (string=? ltr (string-downcase (string-ith (first dict) 0)))
             (cons (first dict)
                   (words-single-letter ltr (rest dict)))
             (words-single-letter ltr (rest dict)))]))



(check-expect
 (most-frequent.v2 (list "apple"))
 (make-res "a" 1))

(check-expect
 (most-frequent.v2 (list "grapple" "banana" "apple" "bunch" "toast"))
 (make-res "b" 2))

; Passing but time consuming
;(check-expect (most-frequent AS-LIST)
;              (most-frequent.v2 AS-LIST))

; Dictionary -> Letter-Count
(define (most-frequent.v2 dict)
  (max-letter-count
   (count-for-letter
    (words-by-letter dict))
   (make-res "tmp" 0)))

(check-expect (count-for-letter (list (list "all" "abra")
                                      (list "column" "cool" "columbia")))
              (list (make-res "a" 2)
                    (make-res "c" 3)))

; (listof Dictionary) -> (listof Letter-Count)
(define (count-for-letter lod)
  (cond [(empty? lod) empty]
        [else
         (if (empty? (first lod))
             (count-for-letter (rest lod))
             (cons (make-res
                    (string-downcase (string-ith (first (first lod)) 0))
                    (length (first lod)))
                   (count-for-letter (rest lod))))]))


(check-expect (max-letter-count (list (make-res "a" 5)
                                      (make-res "b" 20))
                                (make-res "tmp" 0))
              (make-res "b" 20))

(check-expect (max-letter-count (list (make-res "a" 21)
                                      (make-res "b" 20))
                                (make-res "tmp" 0))
              (make-res "a" 21))

(check-expect (max-letter-count (list (make-res "a" 5)
                                      (make-res "b" 20)
                                      (make-res "c" 2))
                                (make-res "tmp" 0))
              (make-res "b" 20))
; (listof Letter-Count) -> Letter-Count
(define (max-letter-count lolc acc)
  (cond [(empty? lolc) acc]
        [else (if (>= (res-count (first lolc)) (res-count acc))
                  (max-letter-count (rest lolc) (first lolc))
                  (max-letter-count (rest lolc) acc))]))


;;;; 12.2 Real-World Data: iTunes

; the 2htdp/itunes library documentation, part 1:

; An LTrack is one of:
; - '()
; - (cons Track LTracks)

;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's
; title, its producing artist, to which album it belongs,
; its playing time in milliseconds, its position within the
; album, the date it was added, how often it has been
; played, and the date when it was last played

;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0 and 23), minute
; (between 0 and 59), and second (also between 0 and 59).

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
;(define (create-track name artist album time
;                      track# added play# played)
;  ...)

; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs
; otherwise it produces #false
;(define (create-date y mo day h m s)
;  ...)

; String -> LTracks
; creates a list-of-tracks representation from the
; text in file-name (an XML export from iTunes)
;(define (read-itunes-as-tracks file-name)
;  ...)

; modify the following to use your chosen name
(define ITUNES-LOCATION "/Users/dale/mytunes.xml")
; Only contains Radiohead's album, In Rainbows (10 songs)

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

;[name artist album time track# added play# played]

; Exercise 200

(check-expect (total-time itunes-tracks) 2558976)

; LTracks -> N
(define (total-time lts)
  (cond [(empty? lts) 0]
        [else (+ (track-time (first lts))
                 (total-time (rest lts)))]))

; Exercise 201

(check-expect (select-all-album-titles itunes-tracks)
              (list "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"
                    "In Rainbows"))

; LTracks -> (listof String)
; The function consumes an LTracks and produces the
; list of album titles as a List-of-strings.
(define (select-all-album-titles lts)
  (cond [(empty? lts) empty]
        [else
         (cons (track-album
                (first lts))
                (select-all-album-titles (rest lts)))]))


(check-expect (create-album-titles-set itunes-tracks)
              (list "In Rainbows"))

; LTracks -> (listof String)
(define (create-album-titles-set lts)
  (create-set (select-all-album-titles lts)))


(check-expect (create-set (list "In Rainbows"
                                "In Rainbows"))
              (list "In Rainbows"))

;(check-expect (create-set empty) (list empty))
(check-expect (create-set (list "a")) (list "a"))
(check-expect (create-set (list "a" "a")) (list "a"))
(check-expect (create-set (list "a" "a" "a")) (list "a"))
(check-expect (create-set (list "a" "b" "a" "d" "e" "b"))
              (list "a" "d" "e" "b"))

; (listof String) -> (listof Strings)
; remove dupes (from front)
(define (create-set los)
  (cond [(empty? los) empty]
        [else
         (if (member? (first los)
                      (create-set (rest los)))
             (create-set (rest los))
             (cons (first los)
                   (create-set (rest los))))]))

(check-expect (select-album-titles/unique itunes-tracks)
              (list "In Rainbows"))

; LTracks -> (listof String)
(define (select-album-titles/unique lts)
  (create-album-titles-set lts))


;; Exercise 202

(check-expect (select-album "In Rainbows"
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                 129724 6 (create-date 2018 2 11 0 29 45)
                 7 (create-date 2018 8 30 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                 242641 3 (create-date 2018 2 11 0 29 45)
                 4 (create-date 2018 8 30 18 7 22))))
 (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                 129724 6 (create-date 2018 2 11 0 29 45)
                 7 (create-date 2018 8 30 17 30 55))))

; String LTracks -> LTracks
(define (select-album album lts)
  (cond [(empty? lts) empty]
        [else
         (if (string=? album (track-album (first lts)))
             (cons (first lts) (select-album album (rest lts)))
             (select-album album (rest lts)))]))



;; Exercise 203


(check-expect
 (select-album-date
  "In Rainbows"
  (create-date 2017 9 28 17 30 55)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 empty)

(check-expect
 (select-album-date
  "In Rainbows"
  (create-date 2019 9 28 17 30 55)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 empty)

(check-expect
 (select-album-date
  "Blink-182"
  (create-date 2017 9 28 17 30 55)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 (list (create-track "I Miss You" "Blink 182" "Blink-182"
                     242641 3 (create-date 2018 2 11 0 29 45)
                     4 (create-date 2018 8 30 13 7 22))))

(check-expect
 (select-album-date
  "Blink-182"
  (create-date 2019 9 28 17 30 55)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 empty)

; String Date LTracks -> LTracks
; list of tracks that belong to the given album
; and have been played after the given date.

(define (select-album-date album dt lts)
  (select-date dt (select-album album lts)))

(check-expect
 (select-date
  (create-date 2017 9 28 17 30 55)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 (list (create-track "I Miss You" "Blink 182" "Blink-182"
                     242641 3 (create-date 2018 2 11 0 29 45)
                     4 (create-date 2018 8 30 13 7 22))))

(check-expect
 (select-date
  (create-date 2017 9 28 17 30 54)
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
       (create-track "I Miss You" "Blink 182" "Blink-182"
                     242641 3 (create-date 2018 2 11 0 29 45)
                     4 (create-date 2018 8 30 13 7 22))))

; Date LTracks -> LTracks
; have been played after the given date
(define (select-date dt lts)
  (cond [(empty? lts) empty]
        [else (if (not (d1-lt-d2 (track-played (first lts)) dt))
                  (cons (first lts) (select-date dt (rest lts)))
                  (select-date dt (rest lts)))]))

; year
(check-expect (d1-lt-d2 (create-date 2017 2 17 5 47 32)
                        (create-date 2019 2 17 5 47 32)) #true)
(check-expect (d1-lt-d2 (create-date 2019 2 17 5 47 32)
                        (create-date 2017 2 17 5 47 32)) #false)
; month
(check-expect (d1-lt-d2 (create-date 2017 2 17 5 47 32)
                        (create-date 2017 3 17 5 47 32)) #true)
(check-expect (d1-lt-d2 (create-date 2017 3 17 5 47 32)
                        (create-date 2017 2 17 5 47 32)) #false)
; day
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 47 32)
                        (create-date 2017 2 17 5 47 32)) #true)
(check-expect (d1-lt-d2 (create-date 2017 2 17 5 47 32)
                        (create-date 2017 2 15 5 47 32)) #false)
; hour
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 47 32)
                        (create-date 2017 2 15 7 47 32)) #true)
(check-expect (d1-lt-d2 (create-date 2017 2 15 7 47 32)
                        (create-date 2017 2 15 5 47 32)) #false)
; minute
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 47 32)
                        (create-date 2017 2 15 5 48 32)) #true)
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 48 32)
                        (create-date 2017 2 15 5 47 32)) #false)
; second
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 47 31)
                        (create-date 2017 2 15 5 47 32)) #true)
(check-expect (d1-lt-d2 (create-date 2017 2 15 5 47 32)
                        (create-date 2017 2 15 5 47 31)) #false)


; Hint You must design a function that consumes
; two Dates and determines whether the first
; occurs before the second.

(define (d1-lt-d2 d1 d2)
  (cond [(< (date-year d1) (date-year d2)) #true]
        [(> (date-year d1) (date-year d2)) #false]
        [(< (date-month d1) (date-month d2)) #true]
        [(> (date-month d1) (date-month d2)) #false]
        [(< (date-day d1) (date-day d2)) #true]
        [(> (date-day d1) (date-day d2)) #false]
        [(< (date-hour d1) (date-hour d2)) #true]
        [(> (date-hour d1) (date-hour d2)) #false]
        [(< (date-minute d1) (date-minute d2)) #true]
        [(> (date-minute d1) (date-minute d2)) #false]
        [(< (date-second d1) (date-second d2)) #true]
        [(> (date-second d1) (date-second d2)) #false]
        [else #true]))

;; Exercise 204

(check-expect
 (select-albums
  (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55))
        (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22))))
 (list (list (create-track "Faust Arp" "Radiohead" "In Rainbows"
                      129724 6 (create-date 2018 2 11 0 29 45)
                      7 (create-date 2017 9 28 17 30 55)))
       (list (create-track "I Miss You" "Blink 182" "Blink-182"
                      242641 3 (create-date 2018 2 11 0 29 45)
                      4 (create-date 2018 8 30 13 7 22)))))

; LTracks -> (listof LTracks)
; Partition LTrack list by album name
(define (select-albums lts)
  (select-albums-wrapper
   (select-album-titles/unique lts)
   lts))

; (listof String) LTracks -> (listof LTracks)
(define (select-albums-wrapper loa lts)
  (cond [(empty? loa) empty]
        [else
         (cons (select-album (first loa) lts)
               (select-albums-wrapper (rest loa) lts))]))

;(select-album-titles/unique lts); (list "alb1" "alb2")

; the 2htdp/itunes library documentation, part 2:

; An LLists is one of:
; - '()
; - (cons LAssoc LLists)

; An LAssoc is one of:
; - '()
; - (cons Association LAssoc)

; An Association is a list of two items:
;   (cons String (cons BSDN '()))

; A BSDN is one of:
; - Boolean
; - Number
; - String
; - Date

; String -> LLists
; creates a list of lists representation for all tracks in
; file-name, which must be an XML export from iTunes
;(define (read-itunes-as-lists file-name)
;  ...)

; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

;; Exercise 205

(define ASSOC1 (list "Track ID" 183))
(define ASSOC2 (list "Name" "Born a Crime"))
(define ASSOC3 (list "Artist" "Trevor Noah"))
(define ASSOC4 (list "Album Artist" "Trevor Noah"))
(define ASSOC5 (list "Genre" "Audiobook"))
(define ASSOC6 (list "Kind" "Audible file"))
(define ASSOC7 (list "Size" 254678069))
(define ASSOC8 (list "Total Time" 31493596))
(define ASSOC9 (list "Protected" #true))

(define LASSOC1 (list ASSOC1 ASSOC2 ASSOC3
                      ASSOC4 ASSOC5 ASSOC6
                      ASSOC7 ASSOC8 ASSOC9))

(define ASSOC10 (list "Track ID" 197))
(define ASSOC11 (list "Name" "15 Step"))
(define ASSOC12 (list "Artist" "Radiohead"))
(define ASSOC13 (list "Album" "In Rainbows"))
(define ASSOC14 (list "Genre" "Alternative Rock"))
(define ASSOC15 (list "Kind" "MPEG audio file"))
(define ASSOC16 (list "Size" 7620597))
(define ASSOC17 (list "Total Time" 237322))

(define LASSOC2 (list ASSOC10 ASSOC11 ASSOC12
                      ASSOC13 ASSOC14 ASSOC15
                      ASSOC16 ASSOC17))

(define LLIST1 (list LASSOC1 LASSOC2))

;; Exercise 206

(check-expect (find-association "Artist" LASSOC2 "Default")
              "Radiohead")
(check-expect (find-association "Meep" LASSOC2 "Default")
              "Default")

; String LAssoc Any -> Association
; produce the first Association whose first item is
; equal to key, or default if there is no Association.
(define (find-association key lassoc default)
  (cond [(empty? lassoc) default]
        [else (if (string=? key (first (first lassoc)))
                  (first (rest (first lassoc)))
                  (find-association key (rest lassoc) default))]))

;; Exercise 207

(check-expect (total-time/list LLIST1)
              (+ 31493596 237322))

; LList -> N
; produces the total amount of play time

(define (total-time/list llist)
  (cond [(empty? llist) 0]
        [else (+ (find-association "Total Time" (first llist) 0)
                 (total-time/list (rest llist)))]))

;; Exercise 208

(check-expect (create-set (list "a")) (list "a"))
(check-expect (create-set (list "a" "a")) (list "a"))

(check-expect (boolean-attributes LLIST1)
              (list "Protected"))

; LLists -> (listof String)
(define (boolean-attributes llist)
  (create-set (boolean-attributes-wrapper llist)))

(check-expect (boolean-attributes-wrapper LLIST1)
              (list "Protected"))

; LLists -> (listof String)
(define (boolean-attributes-wrapper llist)
  (cond [(empty? llist) empty]
        [else (append (get-bool-attrs-from-lassoc (first llist))
                    (boolean-attributes-wrapper (rest llist)))]))

(check-expect (get-bool-attrs-from-lassoc LASSOC1) (list "Protected"))
(check-expect (get-bool-attrs-from-lassoc LASSOC2) empty)

; LAssoc -> (listof String)
(define (get-bool-attrs-from-lassoc lassoc)
  (cond [(empty? lassoc) empty]
        [else (if (boolean? (second (first lassoc)))
                  (cons (first (first lassoc))
                        (get-bool-attrs-from-lassoc (rest lassoc)))
                   (get-bool-attrs-from-lassoc (rest lassoc)))]))

;;;; 12.3 Word Games, Composition Illustrated
;;;; & 12.4 Word Games, the Heart of the Problem




; A Word is one of:
; - empty
; - (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - empty
; - (cons Word List-of-words)
; interpretation a ListOfWords is a list of Words



;; Exercise 209
(check-expect (string->word "cat") (list "c" "a" "t"))

; String -> Word
; converts s to the chosen word representation
(define (string->word s)
  (explode s))

(check-expect (word->string (list "c" "a" "t")) "cat")

; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))

;; Exercise 210

(check-expect (words->strings (list (list "c" "a" "t")
                                   (list "a" "c" "t")))
              (list "cat" "act"))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(define (words->strings low)
  (cond [(empty? low) empty]
        [else (cons (word->string (first low))
                    (words->strings (rest low)))]))

;; Exercise 211

(check-expect (in-dictionary (list "cat" "cta" "act" "atc" "tac" "tca"))
              (list "cat" "act"))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(define (in-dictionary los)
  (cond [(empty? los) empty]
        [else (if (member? (first los) AS-LIST)
                  (cons (first los)
                        (in-dictionary (rest los)))
                  (in-dictionary (rest los)))]))


(check-member-of (alternative-words "cat")
                 (list "cat" "act")
                 (list "act" "cat"))

(check-member-of (alternative-words "rat")
                 (list "rat" "tar" "art" "tra")
                 (list "rat" "art" "tra" "tar")
                 (list "art" "rat" "tar" "tra")
                 (list "art" "tar" "rat" "tra")
                 (list "tar" "rat" "art" "tra")
                 (list "tar" "art" "rat" "tra"))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

; String -> List-of-strings
; finds all words that use the same letters as s
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))


;; Exercise 213

(check-expect (insert-at-end "a" '())
              (list "a"))
(check-expect (insert-at-end "a" empty)
              (cons "a" empty))

(check-expect (insert-at-end "b" (list "a"))
              (list "a" "b"))
(check-expect (insert-at-end "b" (cons "a" empty))
              (cons "a" (cons "b" empty)))

(check-expect (insert-at-end "C" (list "a" "b"))
              (list "a" "b" "C"))
(check-expect (insert-at-end "C" (cons "a" (cons "b" empty)))
              (cons "a" (cons "b" (cons "C" empty))))
(check-expect (insert-at-end "D" (cons "a" (cons "b" (cons "c" empty))))
              (cons "a" (cons "b" (cons "c" (cons "D" empty)))))

; Any (listof Any) -> (listof Any)
(define (insert-at-end elm lst)
  (cond [(empty? lst) (cons elm empty)]
        ;[(empty? (rest lst)) (cons elm empty)]
        [else (cons (first lst)
                    (insert-at-end elm (rest lst)))]))


(check-expect (arrangements empty) (list empty))
(check-expect (arrangements '()) (list '()))

(check-expect (arrangements (list "a" "c" "t"))
              (list (list "a" "c" "t")
                    (list "c" "a" "t")
                    (list "c" "t" "a")
                    (list "a" "t" "c")
                    (list "t" "a" "c")
                    (list "t" "c" "a")))

(check-expect (arrangements (list "d" "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))



(check-expect (arrangements (list "a"))
              (list (list "a")))
(check-expect (arrangements (list "a" "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (arrangements (list "c" "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")
                    (list "c" "t" "a")
                    (list "t" "c" "a")
                    (list "t" "a" "c")))
(check-expect (arrangements (list "d" "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))
(check-expect (arrangements (list "e" "r"))
              (list (list "e" "r")
                    (list "r" "e")))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond [(empty? w) (list empty)]
        [else (insert-everywhere/in-all-words (first w)
                    (arrangements (rest w)))]))

(check-expect (insert-everywhere/in-all-words
               "d" (list (list "e" "r") (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))
(check-expect (insert-everywhere/in-all-words "e" (list (list "r")))
              (list (list "e" "r") (list "r" "e")))

; 1String Words -> Words
(define (insert-everywhere/in-all-words ltr ws)
  (cond [(empty? ws) empty]
        [else (append (insert-everywhere ltr (first ws))
                      (insert-everywhere/in-all-words ltr (rest ws)))]))

(check-expect (insert-everywhere "d" (list "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))

; 1String Word -> Words
(define (insert-everywhere ltr w)
  (insert-ltr-everywhere ltr empty w))

(check-expect (insert-ltr-everywhere "a" '() (list "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")))

; 1String (listof 1String) (listof 1String) -> (listof (listof 1String))
(define (insert-ltr-everywhere ltr w v)
  (cond [(empty? v) (cons (append w (list ltr)) empty)]
        [else
         (cons (append w (list ltr) v)
               (insert-ltr-everywhere ltr (insert-at-end (first v) w) (rest v)))]))


;; Exercise 214

(check-expect ;; If that's what the dictionary says...
 (alternative-words "dear")
 (list "dear" "daer" "dare" "ared" "read"))

;;;; Next few sections are world games

;;;; 12.8 Finite State Machines

; a (b|c)* d
; a (b|c)* a

; An FSM is one of:
;   - '()
;   - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

;; Exercise 226

(check-expect (state=? "red" "RED") #true)
(check-expect (state=? "meep" "reD") #false)

; FSM-State FSM-State -> Boolean
(define (state=? in1 in2)
  (and (image-color? in1) (image-color? in2)
       (string=? (string-downcase in1)
                 (string-downcase in2))))


(define fsm-traffic (list (make-transition "red" "green")
                          (make-transition "green" "yellow")
                          (make-transition "yellow" "red")))

;; Exercise 227

(define bws (list "black" "white"))
(define fsm-bw (list (make-transition (first bws) (first (rest bws)))
                     (make-transition (first (rest bws)) (first bws))))

(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
; SimulationState.v2 -> Image
; renders a world state as an image
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "green") "q")
 (make-fs fsm-traffic "yellow"))
(check-expect
 (find-next-state (make-fs fsm-traffic "yellow") "a")
 (make-fs fsm-traffic "red"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(define (find-next-state sims ke)
  (make-fs (fs-fsm sims)
           (find (fs-fsm sims) (fs-current sims))))

;; Exercise 228

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "black") "not found: black")

; FSM-State FSM -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(define (find lot clr)
  (cond [(empty? lot) (string-append "not found: " clr)]
        [else (if (state=? clr (transition-current (first lot)))
                  (transition-next (first lot))
                  (find (rest lot) clr))]))

;(cons (make-transition Color Color) self)

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))


(define-struct fs [fsm current])
; A SimulationState.v2 is a structure:
;   (make-fs FSM FSM-State)

(simulate.v2 fsm-traffic "red")








