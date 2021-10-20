#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


#|
------------------------------------Alinea los equipos------------------------------------
Def: Esta funcion se encarga de alinear cada jugador en los equipos, asi como segun su tipo
     lo posiciones donde debe de ir
|#

(define(alinear teams)
  (cond((null? teams) teams)
       (else (append (alinear2 (cadr teams)) (alinear1 (car teams))))))

(define(alinear1 equipo)
  (cond((null? equipo) equipo)
       (else (cons (alinear-player1 (car equipo)) (alinear1 (cdr equipo))))))

(define(alinear2 equipo)
  (cond((null? equipo) equipo)
       (else (cons (alinear-player2 (car equipo)) (alinear2 (cdr equipo))))))

(define(alinear-player1 player)
  (cond((null? player) player)
       ((equal? 1 (car(cdddr player)))
        (list (list 5 (+ 180 (random 180))) (list 0 0) (caddr player) 1 (cadr(cdddr player)) "red"))
       ((equal? 2 (car(cdddr player)))
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 2 (cadr(cdddr player)) "red"))
       ((equal? 3 (car(cdddr player)))
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 3 (cadr(cdddr player)) "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 4 (cadr(cdddr player)) "red"))))

(define(alinear-player2 player)
  (cond((null? player) player)
       ((equal? 1 (car(cdddr player)))
        (list (list 895 (+ 180 (random 180))) (list 0 0) (caddr player) 1 (cadr(cdddr player)) "blue"))
       ((equal? 2 (car(cdddr player)))
        (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 2 (cadr(cdddr player)) "blue"))
       ((equal? 3 (car(cdddr player)))
        (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 3 (cadr(cdddr player)) "blue"))
       (else
        (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr player) 4 (cadr(cdddr player)) "blue"))))



#|
------------------------------------Primera generacion para ambos teams------------------------------------
Def: Estas funciones se encargan de crear cada player con sus posicion, equipo y skills.
     Tambien se encargan de crear el equipo segun la formacion requerida por el usuario.
     Se generan por separado cada primera generacion de cada equipo, las skills son random.
|#

;-----------------------Crea skills, posicion y skills random------------------------

(define (player-1er tipo num)
  (cond((equal? 1 tipo)
        (list (list 5 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "red"))
       ((equal? 2 tipo)
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "red"))
       ((equal? 3 tipo)
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "red"))
       ))

(define (player-2do tipo num)
  (cond ((equal? 1 tipo)
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "blue"))
        ((equal? 2 tipo)
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "blue"))
        ((equal? 3 tipo)
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "blue"))
        (else
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "blue"))
        ))

;--------------------------Verifica formacion y le pone un numero al player---------------------

(define (inic-aux1 num formacion)
  (cond((zero? num) '())
       ((equal? 1 num)
        (cons (player-1er 1 1) (inic-aux1 (- num 1) formacion)))
       ((and (> num 1) (< num (+ 2 (car formacion))))
        (cons (player-1er 2 num) (inic-aux1 (- num 1) formacion)))
       ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
        (cons (player-1er 3 num) (inic-aux1 (- num 1) formacion)))
       (else
        (cons (player-1er 4 num) (inic-aux1 (- num 1) formacion)))
       ))

(define (inic-aux2 num formacion)
  (cond ((zero? num) '())
        ((equal? 1 num)
         (cons (player-2do 1 1) (inic-aux2 (- num 1) formacion)))
        ((and (> num 1) (< num (+ 2 (car formacion))))
         (cons (player-2do 2 num) (inic-aux2 (- num 1) formacion)))
        ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
         (cons (player-2do 3 num) (inic-aux2 (- num 1) formacion)))
        (else
         (cons (player-2do 4 num) (inic-aux2 (- num 1) formacion)))
        ))

;--------------------------Obtiene la formacion para ir creando playeres---------------------------

(define (inicialitation1 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux1 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

(define (inicialitation2 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux2 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))






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