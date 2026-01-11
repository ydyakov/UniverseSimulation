#lang racket

;; ======================================
;; НАСТРОЙКИ НА СВЕТА
;; ======================================

(define WIDTH 30)
(define HEIGHT 15)
(define ALIVE "█")
(define DEAD  " ")

;; ======================================
;; ПРЕДСТАВЯНЕ НА ДАННИ
;; ======================================
;; Клетка = (x . y)
;; Вселена = списък от живи клетки

(define initial-universe
  '((14 . 7) (15 . 7) (16 . 7))) ; blinker

;; ======================================
;; ЛОГИКА НА GAME OF LIFE
;; ======================================

(define (neighbors cell)
  (define x (car cell))
  (define y (cdr cell))
  (list (cons (- x 1) (- y 1))
        (cons x       (- y 1))
        (cons (+ x 1) (- y 1))
        (cons (- x 1) y)
        (cons (+ x 1) y)
        (cons (- x 1) (+ y 1))
        (cons x       (+ y 1))
        (cons (+ x 1) (+ y 1))))

(define (alive-neighbors cell universe)
  (length (filter (λ (c) (member c universe))
                  (neighbors cell))))

(define (alive-next? cell universe)
  (define n (alive-neighbors cell universe))
  (if (member cell universe)
      (or (= n 2) (= n 3))
      (= n 3)))

(define (step universe)
  (define candidates
    (remove-duplicates
     (append universe
             (apply append (map neighbors universe)))))
  (filter (λ (c) (alive-next? c universe))
          candidates))

;; ======================================
;; ТЕКСТОВА ВИЗУАЛИЗАЦИЯ
;; ======================================

(define (alive? x y universe)
  (member (cons x y) universe))

(define (draw-universe universe)
  (for ([y (in-range HEIGHT)])
    (for ([x (in-range WIDTH)])
      (display (if (alive? x y universe) ALIVE DEAD)))
    (newline)))

;; ======================================
;; КОНЗОЛА + ВРЕМЕ
;; ======================================

(define (clear-console)
  (display "\033[2J\033[H"))

(define (simulate universe steps)
  (when (> steps 0)
    (clear-console)
    (draw-universe universe)
    (sleep 1)
    (simulate (step universe) (- steps 1))))

;; ======================================
;; СТАРТ
;; ======================================

(simulate initial-universe 50)
