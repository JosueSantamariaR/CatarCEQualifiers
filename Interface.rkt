#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


;Se crea la ventana y el mapa de pixeles con el mismo tamaño de la ventana
(define mainWindow (open-viewport "CatarCEQualifiers" 1220 600))
(define pixMap (open-pixmap "CatarCEQualifiers" 1220 640))



(define playersFirstGen '(((300 150) (375 50) 13) ((400 300) (150 150) 16) ((300 200) (100 450) 20)  ))

(define ball '((450 260) ))



(define (writeStrings points generation)
  (begin
    ;;(overlay/xy texto 430 600 linea)
    ((draw-string pixMap) (make-posn 830 200) (number->string (car points)) "white")
    ((draw-string pixMap) (make-posn 890 200) (number->string (cadr points)) "white")
    ((draw-string pixMap) (make-posn 1000 50) "Generation: " "white")
    ((draw-string pixMap) (make-posn 1200 215) (number->string generation) "white")
    ))



#|

--------------------------------------Definicion para dibujar la cancha----------------------------------------
Def: Se crea un rectangulo de 1220 x 720 de color negro y se asigna la posicion 0,0.
Se coloca la imagen que refleja la cancha.

|#
(define (drawField)
  (begin
    ((draw-solid-rectangle pixMap) (make-posn 0 0) 1220 720 "black")
    ((draw-pixmap pixMap) "background.jpg" (make-posn 0 0)) 
    ))
#|

--------------------------------------Definicion para dibujar en la ventana----------------------------------------
Def: Se copia lo que hay en la ventana y se limpia el mapa de pixeles.
|#
(define (drawWindow)
  (begin
    (copy-viewport pixMap mainWindow)
    ((clear-viewport pixMap))
    )
  )
#|

--------------------------------------Definiciones de los getters----------------------------------------
Def: Se definen los getters que se utilizan para los jugadores
|#

(define (posX jugador)
  (caar jugador))

(define (posY jugador)
  (cadar jugador))

(define (destinoX jugador)
  (caadr jugador))

(define (destinoY jugador)
  (car (cdadr jugador)))


(define (terminoMovimiento jugador)
  (cons (list (destinoX jugador) (destinoY jugador)) (cdr jugador)))

(define (hipotenusa jugador)
  (sqrt (+ (expt (- (destinoX jugador) (posX jugador)) 2)
          (expt (- (destinoY jugador) (posY jugador)) 2)))
  )

(define (movimientoX jugadores step diagonal angulo)
  (+ (posX (car jugadores)) (* step (cos (degrees->radians angulo))))
  )

(define (movimientoY jugadores step diagonal angulo)
  (+ (posY (car jugadores)) (* step (sin (degrees->radians angulo))))
  )



(define (getNextPlayer jugadores)
  (cond ((null? (cdr jugadores))
         (car jugadores))
        (else (cadr jugadores))
        ))


(define (pendiente jugador)
  (/ (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))

(define (obtenerAnguloAux jugador)
  (list (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))


(define (obtenerAngulo jugador)
  (cond
    ((and (< (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (- (radians->degrees (atan (pendiente jugador))) 180)
     )
    ((and (> (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (+ (radians->degrees (atan (pendiente jugador))) 180)
     )
    (else (radians->degrees (atan (pendiente jugador))))
    )
  )


(define (getPlayerNumber player);Obtener el numero para cada jugador de la lista.
  (cond((null? player) '())
       (else
        (getPlayerNumber_aux player 0)
  )))


(define (getPlayerNumber_aux player number)
  (cond((equal? number 4) (car player))
       (else
        (getPlayerNumber_aux (cdr player) (+ number 1)))))



(define (dibujarJugadores jugadores number)  
  (begin
    (cond ((null? jugadores) #t)
          (else (begin
                  (((draw-pixmap-posn "player2.png") pixMap) (make-posn (posX (car jugadores)) (posY (car jugadores))))
                  ((draw-string pixMap) (make-posn (+ (caaar jugadores) 13) (+ (cadar (car jugadores)) 15)) (number->string number) "white")
                  (dibujarJugadores (cdr jugadores) (+ 1 number))
                  )))
    ))



(define (start)
  (begin
    (drawField)
    (writeStrings '(1 0) 12)

    ;(moverJugadores players 0 '() (hipotenusa (car players)) (obtenerAngulo (car players)) )
    (writeStrings '(1 1) 17)
    (dibujarJugadores '(((300 250) (800 250))
                        ((300 300) (450 300))
                        ((300 350) (350 350))
                        ((300 400) (550 450))
                        ((350 410) (550 450))
                        ((350 420) (540 460))
                        ((350 430) (560 470))
                  
                        ) 1)
    
    (copy-viewport pixMap mainWindow)
    ((clear-viewport pixMap))
    
    ))

(start)