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
