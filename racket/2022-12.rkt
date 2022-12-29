#lang racket
(require rackunit)

(define sample
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(struct pos (x y) #:transparent)

(define (string->terrain str)
  (map (compose (curry map (lambda (char)
                             (if (eq? char #\E)
                                 (+ 1 (char->integer #\z))
                                 (char->integer char)))) string->list)
       (string-split str "\n")))

(struct terrain (vertices x-length))
(define (string->terrain2 str)
  (let ([rows (string-split str "\n")])
    (terrain (flatten (map (compose (curry map char->integer) string->list) rows) ) (string-length (first rows)))))

(require racket/trace)

(define (height-at p terrain)
  (if (or ((pos-x p) . >= . (length (first terrain)))
          ((pos-y p) . >= . (length terrain))
          ((pos-x p) . < . 0)
          ((pos-y p) . < . 0))
      256
      (let ([real-height (list-ref (list-ref terrain (pos-y p)) (pos-x p))])
        (if (eq? real-height (char->integer #\S))
            200
            real-height))))

(define (possible-paths p terrain current-path exclude)
  (filter
   (lambda (target)
     (and (not (set-member? exclude target))
          ((height-at target terrain) . <= . (+ 1 (height-at p terrain)))
          (not (member target current-path))))
   (list
    (pos (+ (pos-x p) 1) (pos-y p))
    (pos (- (pos-x p) 1) (pos-y p))
    (pos (pos-x p) (- (pos-y p) 1))
    (pos (pos-x p) (+ (pos-y p) 1)))))

;(check-equal? (possible-paths (pos 0 0) (string->terrain sample) '() (mutable-set))
;              (list (pos 1 0) (pos 0 1)))
;(check-equal? (possible-paths (pos 1 1) (string->terrain sample) (list (pos 1 0)) (mutable-set))
;              (list (pos 2 1) (pos 0 1) (pos 1 2)))
;(check-equal? (possible-paths (pos 3 2) (string->terrain sample) (list (pos 2 2)) (mutable-set))
;              (list (pos 3 1) (pos 3 3)))
;(check-equal? (possible-paths (pos 4 2) (string->terrain sample) (list (pos 3 2) (pos 4 3) (pos 4 1)) (mutable-set))
;              (list (pos 5 2)))

(define (find-start terrain)
  (define (find-start-in-row row) (index-of row (char->integer #\S)))
  (let* ([y (index-where terrain find-start-in-row)]
         [x (find-start-in-row (list-ref terrain y))])
    (pos x y)))

;(check-equal? (find-start (string->terrain sample)) (pos 0 0))

(define (find-path from terrain)
  (define exclude (mutable-set from))
  (define (step-forward history edge)
    (iter (cons edge history)))
  (define (pursue-paths edges history)
    (if (eq? edges '())
        (begin
          (set-add! exclude (first history))
          #f)
        (let ([paths (filter identity
                             (map (curry step-forward history) edges))])
          (if (eq? paths '())
              (begin
                (set-add! exclude (first history))
                #f)
              (argmin length paths)))))
  (define (iter history)
    (if (eq? (height-at (first history) terrain) (+ 1 (char->integer #\z)))
        history
        (pursue-paths (possible-paths (first history) terrain (rest history) exclude) history)))
  (iter (list from)))

(define (pos-to-index pos terrain)
  (+ (pos-x pos) (* (length (first terrain)) (pos-y pos))))

(define (find-neighbours terrain index)
  (let* ([z (char->integer #\z)]
         [E (char->integer #\E)]
         [S (char->integer #\S)]
         [x (terrain-x-length terrain)]
         [h (curry list-ref (terrain-vertices terrain))]
         [l (and (not (eq? 0 (modulo index x))) (- index 1))]
         [r (and (not (eq? (- x 1) (modulo index x))) (+ index 1))]
         [u (and (not (< index x)) (- index x))]
         [d (and (not (> (+ index x) (- (length (terrain-vertices terrain)) 1))) (+ index x))])
    (filter (lambda (j)
              (and j
                   (or (not (eq? (h j) E)) (eq? (h index) z))
                   (or
                    (eq? (h index) S)
                    (<= (h j) (+ 1 (h index))))))
            (list l r u d))))

(let ([terrain (string->terrain2 sample)])
  (check-equal? (find-neighbours terrain 0) (list 1 8))
  (check-equal? (find-neighbours terrain 7) (list 6 15))
  (check-equal? (find-neighbours terrain 39) (list 38 31))
  (check-equal? (find-neighbours terrain 9) (list 8 10 1 17)))

(struct data (dist prev) #:transparent)

(define (dijkstra terrain from)
  (let* ([dist (make-vector (length (terrain-vertices terrain)) +inf.0)]
         [prev (make-vector (length (terrain-vertices terrain)) #f)]
         [queue (range 0 (length (terrain-vertices terrain)))])
    (vector-set! dist from 0)
    (define (loop queue)
      (if (null? queue) (data dist prev)
          (let* ([u (argmin (curry vector-ref dist) queue)]
                 [new-queue (remove u queue)])
            (for ([v (filter (curryr member queue) (find-neighbours terrain u))])
              (let ([alt (+ 1 (vector-ref dist u))])
                (when (< alt (vector-ref dist v))
                  (begin
                    (vector-set! prev v u)
                    (vector-set! dist v alt)
                    (data dist prev)))))
            (loop new-queue))))
    (loop queue)))

(define (run-task str)
  (let* ([t (string->terrain2 str)]
         [start (index-of (terrain-vertices t) (char->integer #\S))]
         [goal (index-of (terrain-vertices t) (char->integer #\E))])
    (vector-ref (data-dist (dijkstra t start)) goal)))

(define (run-task-2 str)
  (let* ([t (string->terrain2 str)]
         [goal (index-of (terrain-vertices t) (char->integer #\E))]
         [starting-points (filter-map
                           (lambda (h i) (if (or (eq? h (char->integer #\a))
                                                 (eq? h (char->integer #\S))) i #f))
                           (terrain-vertices t)
                           (range 0 (length (terrain-vertices t))))])
    (apply min (map (compose (curryr vector-ref goal) data-dist (curry dijkstra t))
                    starting-points))))

(check-equal? (run-task sample) 31)
(check-equal? (run-task-2 sample) 29)

(define (vertex-height-colour v)
  (match v
    [69 (list 0 255 0)]
    [83 (list 255 0 0)]
    [_  (make-list 3 (* (- v 97) 9))]))

(define (vertex-distance-from-start terrain)
  (let ([extent (length (terrain-vertices terrain))])
    (lambda (v)
      (map floor (make-list 3 (* v (/ 255 extent)))))))

(require slideshow)
(define (draw-terrain terrain colour-f)
  (define (draw-line vs rows)
    (if (null? vs) (reverse rows)
        (let ([row (map (compose (curry colorize (filled-rectangle 20 20)) colour-f)
                        (take vs (terrain-x-length terrain)))])
          (draw-line (drop vs (terrain-x-length terrain)) (cons (apply hc-append row) rows)))))
  (apply vc-append (draw-line (terrain-vertices terrain) '())))

; (draw-terrain (string->terrain2 (file->string "input-12")) vertex-height-colour)

