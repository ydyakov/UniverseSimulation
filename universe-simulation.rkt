#lang racket

;; ======================================
;; НАСТРОЙКИ НА СВЕТА
;; ======================================

(define WIDTH 80)
(define HEIGHT 50)
(define ALIVE "██")
(define DEAD  "  ")

;; ======================================
;; ПРЕДСТАВЯНЕ НА ДАННИ
;; ======================================
;; Клетка = (x . y)
;; Вселена = списък от живи клетки

;; ======================================
;; ВСЕЛЕНИ 3 СТАНДАРТНИ И ЕДНА ПРОИЗВОЛНА
;; ======================================

(define blinker-universe
  '((14 . 7) (15 . 7) (16 . 7)))

(define glider-universe
  '((1 . 0)
    (2 . 1)
    (0 . 2) (1 . 2) (2 . 2)))

(define exploder-universe
  '((10 . 5) (12 . 5)
    (10 . 6) (12 . 6)
    (10 . 7) (12 . 7)
    (11 . 8)))

(define random-universe
  (for/list ([i 80])
    (cons (random WIDTH) (random HEIGHT))))


;; ======================================
;; ФУНКЦИЯ ЗА ННФО СЕКЦИЯТА
;; ======================================

(define (print-info)
  (clear-console)
  (displayln "============================================")
  (displayln "")
  (displayln "      ТЕМА: Симулация на вселена")
  (displayln "      Автор: Сиана Дякова")
  (displayln "      Вид вселена: Game of Life")
  (displayln "")
  (displayln "      Симулацията представлява дискретна вселена от клетки, съставена от двумерна решетка от клетки.")
  (displayln "      Всяка клетка може да бъде в едно от две състояния: жива или мъртва")
  (displayln "      Състоянието на всяка клетка в следващото поколение се определя по следните правила:")
  (displayln "      Жива клетка: остава жива, ако има 2 или 3 живи съседи")
  (displayln "      Жива клетка: умира, ако има по-малко от 2 (изолация) или повече от 3 (пренаселеност) живи съседи")
  (displayln "      Мъртва клетка: става жива, само ако има точно 3 живи съседи")
  (displayln "")
  (displayln "      *** За по-добра визуализация моля максимизирайте конзоноя прозорец :) ")
  (displayln "")
  (displayln "        Натиснете Enter за стартиране ")
  (displayln "")
  (displayln "============================================")
  (read-line)
  (clear-console)
  )

;; ======================================
;; ФУНКЦИЯ ЗА ИНСТРУКЦИИ И UI МЕНЮ
;; ======================================

(define (print-menu)
  (clear-console)
  (displayln "==============================")
  (displayln "Изберете начална вселена:")
  (displayln "1 - Blinker")
  (displayln "2 - Glider")
  (displayln "3 - Exploder")
  (displayln "4 - Random")
  (displayln "5 - Зареждане от файл")
  (displayln "==============================")
  (display "Вашият избор: "))


;; ======================================
;; ФУНКЦИЯ ЗА ЗАРЕЖДАНЕ НА BIG-BANG ОТ ФАЙЛ
;; ======================================
(define (load-universe filename)
  (define lines (file->lines filename))
  (apply append
         (for/list ([y (in-range (length lines))])
           (define line (list-ref lines y))
           (for/list ([x (in-range (string-length line))]
                      #:when (char=? (string-ref line x) #\█))
             (cons x y)))))

;; ======================================
;; ФУНКЦИЯ (UI) ЗА ИЗБОР НА ВСЕЛЕНА
;; ======================================
(define (choose-universe)
  (print-menu)
  (define choice (read))
  (cond
    [(= choice 1) blinker-universe]
    [(= choice 2) glider-universe]
    [(= choice 3) exploder-universe]
    [(= choice 4) random-universe]
    [(= choice 5)
      (display "Въведи име на файл: ")
      (load-universe (read-line))]
    [else
     (displayln "Невалиден избор. Опитай пак.")
     (choose-universe)]))


;; ======================================
;; ИЗЧИСТВАНЕ НА КОНСОЛАТА
;; ======================================
(define (clear-console)
  (display "\033[2J\033[H"))

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

(define (alive? x y universe)
  (member (cons x y) universe))

;; ======================================
;; ТЕКСТОВА ВИЗУАЛИЗАЦИЯ
;; ======================================


(define (draw-universe universe steps)
  (clear-console)
  (displayln (format "популация # ~a" steps))
  (newline)
  (for ([y (in-range HEIGHT)])
    (for ([x (in-range WIDTH)])
      (display (if (alive? x y universe) ALIVE DEAD)))
    (newline)))

;; ======================================
;; СИМУЛАЦИЯ - ВРЕМЕ
;; ======================================
  
(define (simulate universe steps)
  (when (< steps 50)
    (clear-console)
    (draw-universe universe steps)
    (sleep 1)
    (simulate (step universe) (+ steps 1))))

;; ======================================
;; СТАРТ
;; ======================================

(print-info)
(define selected-universe (choose-universe))
(simulate selected-universe 1)

