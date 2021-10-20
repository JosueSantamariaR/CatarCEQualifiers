;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Genetic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
------------------------------------Primera generacion para ambos equipos------------------------------------
Def: Estas funciones se encargan de crear cada jugador con sus posicion, equipo y habilidades.
     Tambien se encargan de crear el equipo segun la formacion requerida por el usuario.
     Se generan por separado cada primera generacion de cada equipo, las habilidades son random.
|#

;-----------------------Crea habilidades, posicion y habilidades random------------------------
(define (jugador-1er tipo num)
  (cond((equal? 1 tipo)
        (list (list 5 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "red"))
       ((equal? 2 tipo)
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "red"))
       ((equal? 3 tipo)
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "red"))
       ))

(define (jugador-2do tipo num)
  (cond ((equal? 1 tipo)
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "blue"))
        ((equal? 2 tipo)
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "blue"))
        ((equal? 3 tipo)
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "blue"))
        (else
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "blue"))
        )
  
;--------------------------Verifica formacion y le pone un numero al jugador---------------------
(define (inic-aux1 num formacion)
  (cond((zero? num) '())
       ((equal? 1 num)
        (cons (jugador-1er 1 1) (inic-aux1 (- num 1) formacion)))
       ((and (> num 1) (< num (+ 2 (car formacion))))
        (cons (jugador-1er 2 num) (inic-aux1 (- num 1) formacion)))
       ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
        (cons (jugador-1er 3 num) (inic-aux1 (- num 1) formacion)))
       (else
        (cons (jugador-1er 4 num) (inic-aux1 (- num 1) formacion)))
       ))

(define (inic-aux2 num formacion)
  (cond ((zero? num) '())
        ((equal? 1 num)
         (cons (jugador-2do 1 1) (inic-aux2 (- num 1) formacion)))
        ((and (> num 1) (< num (+ 2 (car formacion))))
         (cons (jugador-2do 2 num) (inic-aux2 (- num 1) formacion)))
        ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
         (cons (jugador-2do 3 num) (inic-aux2 (- num 1) formacion)))
        (else
         (cons (jugador-2do 4 num) (inic-aux2 (- num 1) formacion)))
        ))

;--------------------------Obtiene la formacion para ir creando jugadores---------------------------
(define (inicializacion1 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux1 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

(define (inicializacion2 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux2 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))