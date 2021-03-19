;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname III-abstraction-ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
;;;; 4 Intervals, Enumerations, and Itemizations

(require 2htdp/image)
(require 2htdp/universe)

;;;; 4.7 Finite State Worlds

; TrafficLight -> TrafficLight
; yields the next state, given the current state cs
(define (tl-next cs) cs)

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (empty-scene 90 30))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

(traffic-light-simulation "red")

(check-expect (tl-render "red") image)
(check-expect (tl-render "yellow") image)

"end"

(test);; DrRacket does something like this behind the scenes
