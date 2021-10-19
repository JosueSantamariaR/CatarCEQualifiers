#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)



#|
--------------------------------------Creacion de las ventanas----------------------------------------
Def: Se definen las funciones para crear una ventana de tipo viewport y una ventana de tipo pixmap, con
las mismas dimensiones.
|#

(define mainWindow (open-viewport "CatarCEQualifiers" 1220 600))
(define pixMap (open-pixmap "CatarCEQualifiers" 1220 640))



#|
--------------------------------------Definicion de elementos iniciales----------------------------------------
Def: Se crea la primera generacionde jugadores y la bola con la fuerza, distancia
|#
(define playersFirstGen '(((300 150) (375 50) 13)
                          ((400 300) (150 150) 16)
                          ((300 200) (100 450) 20)
                          ))
(define ball '((450 260) ))



#|
--------------------------------------Definicion de la funcion Write----------------------------------------
Def: Esta definicion nos permite escribir en la ventana de pixMap lo que son los marcadores y la generacion
del algoritmo genetico.
|#
(define (writeStrings points generation)
  (begin
   
    ((draw-string pixMap) (make-posn 1050 30) "Current Score: " "cyan")
    ((draw-string pixMap) (make-posn 990 60) (number->string (car points)) "cyan")
    ((draw-string pixMap) (make-posn 1190 60) (number->string (cadr points)) "cyan")
    ((draw-string pixMap) (make-posn 1020 150) "Generation: " "cyan")
    ((draw-string pixMap) (make-posn 1150 150) (number->string generation) "cyan")
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
Def: Se definen los getters que se utilizan para obtener la posicion inicial,final,movimiento final, la
hipotenusa
|#

(define (getX player)
  (caar player))

(define (getY player)
  (cadar player))

(define (getFinalX player)
  (caadr player))

(define (getFinalY player)
  (car (cdadr player)))


; Se utilizan las posiciones finales del jugadores para obtener el movimiento final
(define (finishMove player)
  (cons (list (getFinalX player) (getFinalY player)) (cdr player)))

;Se obtiene la hipotenusa para calcular el movimiento.
(define (moveLine player)
  (sqrt (+ (expt (- (getFinalX player) (getX player)) 2)
          (expt (- (getFinalY player) (getY player)) 2)))
  )

;Definicion para el movimiento del jugador en X.
(define (moveInX players step diagonal angle)
  (+ (getX (car players)) (* step (cos (degrees->radians angle))))
  )

;Definicion para el movimiento del jugador en Y.
(define (moveInY players step diagonal angle)
  (+ (getY (car players)) (* step (sin (degrees->radians angle))))
  )


;Definicion de la funcion para obtener el jugador siguiente
(define (getNextPlayer players)
  (cond ((null? (cdr players))
         (car players))
        (else (cadr players))
        ))


;Pendiente del jugador para la obtencion de los angulos
(define (slope player)
  (/ (- (getFinalY player) (getY player)) (- (getFinalX player) (getX player))))


;Funcion Auxiliar para el angulo.
(define (getAngleAux player)
  (list (- (getFinalY player) (getY player)) (- (getFinalX player) (getX player))))


;Funcion Auxiliar para el angulo.
(define (getAngle player)
  (cond
    ((and (< (car (getAngleAux player)) 0) (< (cadr (getAngleAux player)) 0))
     (- (radians->degrees (atan (slope player))) 180)
     )
    ((and (> (car (getAngleAux player)) 0) (< (cadr (getAngleAux player)) 0))
     (+ (radians->degrees (atan (slope player))) 180)
     )
    (else (radians->degrees (atan (slope player))))
    )
  )

;Obtener el numero para cada jugador de la lista.
(define (getPlayerNumber player)
  (cond((null? player) '())
       (else
        (getPlayerNumber_aux player 0)
  )))


;Obtener el numero de jugador.
(define (getPlayerNumber_aux player number)
  (cond((equal? number 4) (car player))
       (else
        (getPlayerNumber_aux (cdr player) (+ number 1)))))


#|
--------------------------------------Funcion Draw----------------------------------------
Def: Se define la funcion que nos permite dibujar los jugadores y el numero correspondiente a de cada
jugador.
|#

(define (drawPlayers players number)  
  (begin
    (cond ((null? players) #t)
          (else (begin
                  (((draw-pixmap-posn "player2.png") pixMap) (make-posn (getX (car players)) (getY (car players))))
                  ((draw-string pixMap) (make-posn (+ (caaar players) 10) (+ (cadar (car players)) 10)) (number->string number) "cyan")
                  (drawPlayers (cdr players) (+ 1 number))
                  )))
    ))




#|
--------------------------------------Funcion movePlayers ----------------------------------------
Def: Se define la funcion la cual nos permite mover los jugadores utilizando la diagonal y el angulo
para cada uno de los jugadores.
|#

(define (movePlayers players step newPlayers diagonal angle)
  (cond ((null? players) newPlayers)
        ((< diagonal step)
         (begin
           (movePlayers (cdr players) 0
                           (cons (finishMove (car players)) newPlayers)
                           (moveLine (getNextPlayer players))
                           (getAngle (getNextPlayer players))))
         )
        (else
         (begin
           (display step)
           (drawField)
           (((draw-pixmap-posn "player2.png") pixMap) (make-posn (moveInX players step diagonal angle) (moveInY players step diagonal angle)))
           (drawPlayers (append newPlayers (cdr players)) 1)

           (copy-viewport pixMap mainWindow)
           ((clear-viewport pixMap))

           (movePlayers players (+ step 10) newPlayers diagonal angle)
           ))
        ))




#|
--------------------------------------Funcion PlayGame ----------------------------------------
Def: Se define la funcion PlayGame para la ejecucion y muestra de las funciones anteriormente realizadas

|#

(define (playGame)
  (begin
    (drawField)
    (writeStrings '(1 1) 17)
    (movePlayers playersFirstGen 0 '() (moveLine (car playersFirstGen)) (getAngle (car playersFirstGen)) )

    #|

    (drawPlayers '(((230 250) (800 250))
                        ((230 300) (450 300))
                        ((230 350) (350 350))
                        ((280 250) (800 250))
                        ((280 300) (450 300))
                        ((280 350) (350 350))
                        ((330 350) (350 350))
                        ((330 250) (800 250))
                        ((330 300) (450 300))
                        ((320 350) (350 350))

                        ((700 250) (800 250))
                        ((700 300) (450 300))
                        ((700 350) (350 350))
                        ((750 250) (800 250))
                        ((750 300) (450 300))
                        ((750 350) (350 350))
                        ((800 350) (350 350))
                        ((800 250) (800 250))
                        ((800 300) (450 300))
                 
                  
                        ) 1)

    |#
    
    (copy-viewport pixMap mainWindow)
    ((clear-viewport pixMap))
    
    ))

(playGame)