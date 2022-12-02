#lang racket

; Notes:
; - instead of enumerating all cases, you could work with permutations:
;   It's indexing into XYZ, at an offset (especially in the second task).
; - It would've been a hell of a lot easier to just translate ABC and
;   XYZ into :rock :paper :scissors along with the transformation to
;   (struct strategy)

(require racket/string)

(define sample
  "A Y
B X
C Z")

(define input-file (file->string "input-02"))

(struct strategy (theirs ours)
  #:transparent)

(define (score-xyz ours)
  (match ours
    [(== "X") 1]    ; rock
    [(== "Y") 2]    ; paper
    [(== "Z") 3]))  ; scissors

(define (score-ours strat) (score-xyz (strategy-ours strat)))

(define (play-game strat)
  (match strat
    [(== (strategy "C" "X")) 'win]
    [(== (strategy "A" "Y")) 'win]
    [(== (strategy "B" "Z")) 'win]
    [(== (strategy "A" "X")) 'draw]
    [(== (strategy "B" "Y")) 'draw]
    [(== (strategy "C" "Z")) 'draw]
    [_ 'loss]))

(define (score-game outcome)
  (match outcome
    [(== 'win) 6]
    [(== 'draw) 3]
    [(== 'loss) 0]))

(define (score-first strat)
  (+ (score-ours strat)
     (score-game (play-game strat))))

(define (run-strategies input score-f)
  (apply + (map
            (lambda (str)
              (let ([split (string-split str " ")])
                (score-f (strategy (first split) (second split)))))
            (string-split input "\n"))))

(run-strategies input-file score-first)

(define (score-second strat)
  (let ([desired-outcome (match (strategy-ours strat)
                           [(== "X") 'loss]
                           [(== "Y") 'draw]
                           [(== "Z") 'win])])
    (+ (score-game desired-outcome)
       (match (cons (strategy-theirs strat) desired-outcome)
         [(== (cons "A" 'win)) (score-xyz "Y")]
         [(== (cons "B" 'win)) (score-xyz "Z")]
         [(== (cons "C" 'win)) (score-xyz "X")]
         [(== (cons "A" 'draw)) (score-xyz "X")]
         [(== (cons "B" 'draw)) (score-xyz "Y")]
         [(== (cons "C" 'draw)) (score-xyz "Z")]
         [(== (cons "A" 'loss)) (score-xyz "Z")]
         [(== (cons "B" 'loss)) (score-xyz "X")]
         [(== (cons "C" 'loss)) (score-xyz "Y")]))))

(run-strategies input-file score-second)
