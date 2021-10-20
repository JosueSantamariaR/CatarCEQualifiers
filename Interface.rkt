#lang racket/gui
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


;Se inicializa la libreria encargada del uso de la parte grafica.
(open-graphics)


#|
-------------------------------------Aptitud--------------------------------------
Def: Se 

|#

(define (fitness-eq equipo)
  (cond((null? equipo) equipo)
       (else( cons (fitness (car equipo)) (fitness-eq (cdr equipo))))))

(define (fitness player)
  (cond((null? player) player)
       (else (list(car player) (cadr player) (fitness-aux (caddr player)) (car(cdddr player)) (cadr(cdddr player))))))

(define (fitness-aux skills)
  (cond((null? skills) skills)
       (else(cons (suma (cdr skills)) (cdr skills)))))

(define(suma lista)
  (cond((null? lista) 0)
       (else(+ (car lista) (suma (cdr lista))))))


#|
-------------------------------------Aux-Formacion--------------------------------------
Def: 

|#

(define (obtenerFormacion playeres  num1 num2 num3)
  (cond((null? playeres) (list num1 num2 num3))
       ((equal? (obtenerNumTipo (car playeres)) 2) (obtenerFormacion (cdr playeres) (+ num1 1) num2 num3))
       ((equal? (obtenerNumTipo (car playeres)) 3) (obtenerFormacion (cdr playeres) num1 (+ num2 1) num3))
       ((equal? (obtenerNumTipo (car playeres)) 4) (obtenerFormacion(cdr playeres)  num1 num2 (+ num3 1)))
       (else
        (obtenerFormacion (cdr playeres) num1 num2 num3))))


#|
-------------------------------------Seleccion--------------------------------------
Def: 

|#

(define (seleccion-reproduction-aux teams)
  (append (seleccion-reproduction (obtenerFormacion (car teams) 0 0 0) (car teams))
          (seleccion-reproduction (obtenerFormacion (cadr teams) 0 0 0) (cadr teams)))
  )

(define (seleccion-reproduction formacion equipo)
  (cond((null? equipo) equipo)
       (else
        (asignar-caracts
         (mutation
          (reproduction
           (crear-hijos (+ 1 (car formacion) (cadr formacion) (caddr formacion)) (mejores(retornar-caracts equipo)))))
         equipo))))

;;3
(define (crear-hijos num padres)
  (cond((even? num) (append (crearhijos-aux (quotient num 2) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))
       (else(append (crearhijos-aux (+ 1 (quotient num 2)) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))))

;;3.1
(define(crearhijos-aux num padre)
  (cond((= num 1) (list padre))
       (else (append (list padre) (crearhijos-aux (- num 1) padre)))))

;;1
(define (retornar-caracts equipo)
  (cond((null? equipo) equipo)
       (else(cons (caddr(car equipo)) (retornar-caracts (cdr equipo))))))
;;2
(define (mejores lista)
  (cond((null? lista) lista)
       (else
        (list (mayor (car lista) (cdr lista))
              (mayor (car(eliminar (mayor (car lista) (cdr lista)) lista))
                     (eliminar (mayor (car lista) (cdr lista)) lista))))))
;;2.1
(define (mayor actual lista)
  (cond((null? lista) actual)
       ((> (car actual) (caar lista)) (mayor actual (cdr lista)))
       ((< (car actual) (caar lista)) (mayor (car lista) (cdr lista)))
       (else(mayor actual (cdr lista)))))
;;2.2
(define(eliminar num lista)
  (cond((null? lista) (list))
       ((equal? (car num) (caar lista)) (cdr lista))
       (else (cons (car lista) (eliminar num (cdr lista))))))

;Funciones extra para la seleccion----------------------------
(define(asignar-caracts carac equipo)
  (cond((null? equipo) '())
       (else (cons (asignar-aux (car carac) (car equipo)) (asignar-caracts (cdr carac) (cdr equipo))))))

(define(asignar-aux hab player)
  (cond((or (null? hab) (null? player)) player)
       (else (list(car player) (cadr player) hab (car(cdddr player)) (cadr(cdddr player)) (obtenerEquipo player)))))

#|
-------------------------------------Reproduccion--------------------------------------
Def: Obtiene los jugadores y de cada uno de ellos se obtiene las habilidades que se generaron en random
     estas habilidades se utilizan para crear nuevas e ir combinandolas entre el ultimo y el primero
|#

(define(reproduction lista)
  (cond((null? lista) lista)
       ((null? (cdr lista)) lista)
       (else(append (combinar (car lista) (ultimo (car lista) lista)) (reproduction (sin-ultimo(cdr lista)))))))

(define(combinar skills1 skills2)
  (cond((or (null? skills1) (null? skills2)) (list skills1 skills2))
       (else(cons (combinar-aux (+ 1 (random 5)) skills1 skills2) (list (combinar-aux (+ 1 (random 5)) skills1 skills2))))))

(define(combinar-aux num hab1 hab2)
  (cond((or (null? hab1) (= num 0)) hab2)
       (else(cons (car hab1) (combinar-aux (- num 1) (cdr hab1) (cdr hab2))))))

(define(ultimo actual lista)
  (cond((null? lista) actual)
       (else (ultimo (car lista) (cdr lista)))))

(define(sin-ultimo lista)
  (cond((null? (cdr lista)) '())
       (else(cons (car lista) (sin-ultimo (cdr lista))))))


#|
-------------------------------------Mutacion------------------------------------
Def: Comienza a tomar las habilidades del jugador para posteriormente con un random
     hacer diferentes operaciones y obtener un nuevo jugador con estos cambios.
|#
(define(mutation skills)
  (cond((null? skills) skills)
       (else(cons (mutation-aux (car skills)) (mutation (cdr skills))))))

(define (mutation-aux hab)
  (cond((< (random 100) 30) (mutar (+ 1 (random 4)) hab))
       (else hab)))

(define(mutar corte hab)
  (cond((zero? corte) (cons (random 11) (cdr hab)))
       (else (cons (car hab) (mutar (- corte 1) (cdr hab))))))


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