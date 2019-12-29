| package |
package := Package name: 'tetris'.
package paxVersion: 0;
	basicComment: 'Tetris para Dolphin Smalltalk
por Andres Otaduy
<aotaduy@frro.utn.edu.ar>

Un ejemplo de Uso de los GraphicObjects, el viejo y querido Tetris

Para un ejemplo evaluar :

TetrisPresenter show.

Por Andres Otaduy
Dudas y comentarios a:
<aotaduy@frro.utn.edu.ar>'.

package basicPackageVersion: ''.

"Add the package scripts"

"Add the class names, loose method names, global names, resource names"
package classNames
	add: #TetrisPresenter;
	yourself.

package methodNames
	add: #Point -> #rotate90CenterAt:;
	yourself.

package globalNames
	yourself.

package resourceNames
	yourself.

"Binary Global Names"
package binaryGlobalNames: (Set new
	yourself).
"Resource Names"
package allResourceNames: (Set new
	add: #TetrisPresenter -> 'Default view';
	add: #TetrisPresenter -> 'Default view conBotones';
	yourself).

"Add the prerequisite names"
package setPrerequisites: (IdentitySet new
	add: 'Dolphin';
	add: 'GraphicObjects';
	yourself).

package!

"Class Definitions"!

CompositePresenter subclass: #TetrisPresenter
	instanceVariableNames: 'drawing piezaActual procesoAnimacion mutex'
	classVariableNames: 'RandomGenerator'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
"Loose Methods"!

!Point methodsFor!

rotate90CenterAt: c
	"Answer a Point which is rotated 90 degrees, about the point c.
	
	"
	| offset |
	offset :=  self - c.
^ (offset y negated @ offset x) + c
! !
!Point categoriesFor: #rotate90CenterAt:!*-unclassified!public! !

"End of package definition"!



TetrisPresenter comment: ''!

TetrisPresenter guid: (GUID fromString: '{087264C0-F6F6-11D5-B8E3-D452E4C66E1F}')!

!TetrisPresenter categoriesForClass!Unclassified! !
!TetrisPresenter methodsFor!

abajo

	"Mueve hacia abajo en una celda a la pieza actual"

	 self translatePiezaActualBy: 0@self ladoBloque.!

arenaBounds
	"Devuelve un Rectangulo que representa los limites de la arena de juego"

	^(drawing view rectangle origin truncateTo: self ladoBloque)
		corner: (drawing view rectangle corner truncateTo: self ladoBloque )!

arrancarProcesoAnimacion
	"Inicia el proceso de animacion para un juego, este termina cuando la pila de bloques super el limite superior de la arena de juego"
	mutex := Mutex new.
	procesoAnimacion := [[
					self crearNuevaPieza.
					self moverNuevaPieza.
					piezaActual decompose.
					self eliminarFilas.
					self isTerminado] whileFalse] fork.!

bloqueAt: aPoint
		"Devuelve el bloque ubicado en las coordenas determinadas por aPoint. Donde aPoint son las coordenadas de la celda, en el formato fila columna"
		
	^drawing model graphicObjectAt: (aPoint transpose * self ladoBloque) - (self ladoBloque /2)!

bloqueClass

	"Devuelve la clase que se utilizara para los bloques"
	^GraphicRectangle!

cantidadColumnas

	"Devuelve la cantidad de columnas de la arena"
	^self dimensiones y!

cantidadFilas

	"Devuelve la cantidad de filas de la arena"
	^self dimensiones x!

crearNuevaPieza
	"Crea una pieza nueva, la agrega a la arena e inicializa la posicion donde esta empezara a caer"
	piezaActual := self nuevaPieza.
	drawing model add: piezaActual.
	piezaActual position: self arenaBounds center x @ 0.
					!

createComponents

	"super showComment 
		jejeje!! "
	super createComponents.
	drawing := self add: GraphicDrawingPresenter new name:'drawing'.


	!

createSchematicWiring
	"super showComment 
		jejeje!! "
	drawing when: #keyPressed: send: #onKeyPressed: to: self!

delay

	"Devuelve el retraso en millisegundos que tendra una pieza entre movimiento y movimiento"
	^200!

derecha
	"Mueve la pieza que esta cayendo un lugar hacia la derecha"
	self translatePiezaActualBy: self ladoBloque @ 0!

dimensiones
	"Devuelve un Point que determina la cantidad de filas y columnas del receptor"
		
	^(self arenaBounds extent / self ladoBloque)  transpose!

eliminarFilas
	"Elimina, en el caso que corresponda, todas las filas completas de la arena.
		Nota una fila esta completa cuando ocupa todo el ancho de la arena y esta puede cambiarse"
	|fila  rect|
	[(fila := self filaParaEliminar) notNil ] 
		whileTrue:[ 
			fila do:[:unBloque| unBloque delete].
			rect := Rectangle vertex: fila first topLeft vertex: self arenaBounds topRight.
			(drawing model subObjects select:[:unBloque| rect contains: unBloque bounds])
				do:[:unBloque| 
					(Delay forMilliseconds:10)wait. 					unBloque translateBy: 0@self ladoBloque].
	]
!

especificacionPiezas

		"Devuelve una coleccion con todas las especificaciones de la forma de las diferentes piezas del juego. 
		El formato de una especificacion es el siguiente. 
			1 espacio lleno.
			0 espacio vacio
			la especificacion es una coleccion que a su vez tiene una coleccion por cada fila de la pieza. vendria a ser una matriz. "

	^#(	(	( 1) 
			( 1) 
			( 1) 
			( 1) 
		)
	     	( 	(1 1)
			(1 1)
		)
		(	(1 0)
			(1 1)
			(0 1)
		)
		(	(0 1)
			(1 1)
			(1 0 )
		)

		(	(1 1)
			(0 1) 
			(0 1) 
		)
		(	(0 1)
			(1 1) 
			(0 1) 
		)

	).!

filaParaEliminar
	"Devuelve la primer fila que puede ser eliminada o sea la primera que esta completa contando desde abajo hacia arriba"
	|fila rect|
	(self cantidadFilas to: 1 by: -1)detect:[:i| 
		fila := ((1 to: self cantidadColumnas) 
				collect:[:j| self bloqueAt: i@j]) 
				select:[:aBloque| aBloque notNil].
	fila size >= self cantidadColumnas.
	]
	ifNone:[^nil].
	^fila!

isEnElPiso: unaPiezaMoviendose
	"Devuelve un Boolean que determina si la pieza ha tocado el fondo"
		
	^ unaPiezaMoviendose  bottom >=  (self arenaBounds bottom) or:[ 
		unaPiezaMoviendose subObjects anySatisfy:[:unBloque | 
				(drawing model graphicObjectAt: unBloque bottomCenter + 1 ) isKindOf: self bloqueClass 
		]
	]!

isTerminado
	"Devuelve un boolean que determina si el juego esta terminado (game over)"

	^drawing model subObjects anySatisfy:[:unBloque| unBloque position y <= (self arenaBounds top - 5)].!

izquierda
	"Mueve la pieza que esta cayendo un lugar hacia la izquierda"
	self translatePiezaActualBy: self ladoBloque negated @ 0.!

ladoBloque
	
	"Devuelve el lado del bloque en pixels"

	^16!

moverHastaElPiso: unaPieza

"	[self isEnElPiso: unaPieza] 
		whileFalse:[
			self translatePiezaActualBy: 0@ self ladoCuadro .
			]."
		!

moverNuevaPieza
		"Mueve la piezaActual hacia abajo hasta el que choque con el fondo de la arena o con algun bloque que este en su camino"

	[self isEnElPiso: piezaActual] 
		whileFalse:[
			self translatePiezaActualBy: 0@ self ladoBloque .
			Processor sleep: self delay.
			].
		!

nuevaPieza
		
		"Devuelve una pieza generada aleatoriamente segun las especificaciones de piezas"

	^self piezaParaEspecificacion: (self especificacionPiezas at: (self randomNumber * self  especificacionPiezas size) ceiling) !

nuevoBloque	
	"Devuelve un Bloque nuevo para armar una pieza"
	^(self bloqueClass new )extent: self ladoBloque asPoint!

onKeyPressed: aKeyEvent
	"No se porque no anda "
	
	|keyCode|

	keyCode := aKeyEvent code.
	keyCode == VK_LEFT ifTrue:[ self izquierda].
	keyCode == VK_RIGHT ifTrue:[ self derecha].
	keyCode == VK_DOWN ifTrue:[ self abajo].
	(keyCode == VK_UP or:[ keyCode == VK_SPACE ] )ifTrue:[ self rotar].
	
	^super onKeyPressed: aKeyEvent.
	


!

onViewClosed

	super onViewClosed.
	self terminarProcesoAnimacion
!

onViewOpened

	super onViewOpened.
	self arrancarProcesoAnimacion

!

piezaParaEspecificacion: unaEspecificacion
	
	"Devuelve una pieza para la especificacion unaEspecificacion.
	Nota : Ver #especificacionPiezas"
	|pieza puntoInsercion|
	pieza := GraphicCompositeObject new.
	unaEspecificacion inject: 0 into:[:y :fila|
		fila keysAndValuesDo:[:index :aNumber | 
			aNumber = 1 ifTrue:[ 
				(pieza add: self nuevoBloque) position: index -1 * self ladoBloque @y
			].
		].
		y + self ladoBloque
	].

	^pieza!

randomNumber
	
	"Devuelve un numero pseudoAleatorio entre 0 y 1"
	RandomGenerator isNil ifTrue:[ RandomGenerator := Random new].
	^RandomGenerator next!

rotar
	"Rota la pieza actual en 90 grados en el sentido de las manecillas del reloj"
	|centro oldPosition|
	mutex critical:[ centro := piezaActual center.
				oldPosition := piezaActual position.
	piezaActual subObjects do:[:unBloque | unBloque center:  (unBloque center rotate90CenterAt: centro)].

	piezaActual position: oldPosition].
	!

terminarProcesoAnimacion

	"Termina el proceso de animacion destructivamente"
	procesoAnimacion terminate.!

translatePiezaActualBy: aPoint

	"Mueve la pieza actual con el incremento aPoint
	Nota de implementacion: Se utiliza el mutex para no mover la pieza  mientras esta está bajando"
	mutex critical:[ piezaActual translateBy: aPoint]! !
!TetrisPresenter categoriesFor: #abajo!*-unclassified!public! !
!TetrisPresenter categoriesFor: #arenaBounds!*-unclassified!public! !
!TetrisPresenter categoriesFor: #arrancarProcesoAnimacion!*-unclassified!public! !
!TetrisPresenter categoriesFor: #bloqueAt:!*-unclassified!public! !
!TetrisPresenter categoriesFor: #bloqueClass!*-unclassified!public! !
!TetrisPresenter categoriesFor: #cantidadColumnas!*-unclassified!public! !
!TetrisPresenter categoriesFor: #cantidadFilas!*-unclassified!public! !
!TetrisPresenter categoriesFor: #crearNuevaPieza!*-unclassified!public! !
!TetrisPresenter categoriesFor: #createComponents!*-unclassified!public! !
!TetrisPresenter categoriesFor: #createSchematicWiring!*-unclassified!public! !
!TetrisPresenter categoriesFor: #delay!*-unclassified!public! !
!TetrisPresenter categoriesFor: #derecha!*-unclassified!public! !
!TetrisPresenter categoriesFor: #dimensiones!*-unclassified!public! !
!TetrisPresenter categoriesFor: #eliminarFilas!*-unclassified!public! !
!TetrisPresenter categoriesFor: #especificacionPiezas!*-unclassified!public! !
!TetrisPresenter categoriesFor: #filaParaEliminar!*-unclassified!public! !
!TetrisPresenter categoriesFor: #isEnElPiso:!*-unclassified!public! !
!TetrisPresenter categoriesFor: #isTerminado!*-unclassified!public! !
!TetrisPresenter categoriesFor: #izquierda!*-unclassified!public! !
!TetrisPresenter categoriesFor: #ladoBloque!*-unclassified!public! !
!TetrisPresenter categoriesFor: #moverHastaElPiso:!*-unclassified!public! !
!TetrisPresenter categoriesFor: #moverNuevaPieza!*-unclassified!public! !
!TetrisPresenter categoriesFor: #nuevaPieza!*-unclassified!public! !
!TetrisPresenter categoriesFor: #nuevoBloque!*-unclassified!public! !
!TetrisPresenter categoriesFor: #onKeyPressed:!*-unclassified!public! !
!TetrisPresenter categoriesFor: #onViewClosed!*-unclassified!public! !
!TetrisPresenter categoriesFor: #onViewOpened!*-unclassified!public! !
!TetrisPresenter categoriesFor: #piezaParaEspecificacion:!*-unclassified!public! !
!TetrisPresenter categoriesFor: #randomNumber!*-unclassified!public! !
!TetrisPresenter categoriesFor: #rotar!*-unclassified!public! !
!TetrisPresenter categoriesFor: #terminarProcesoAnimacion!*-unclassified!public! !
!TetrisPresenter categoriesFor: #translatePiezaActualBy:!*-unclassified!public! !

 
"Binary Globals"!

"Resources"!

(ResourceIdentifier class: TetrisPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
#[33 83 84 66 32 48 32 70 2 12 0 1 0 0 0 86 105 101 119 82 101 115 111 117 114 99 101 0 0 0 0 14 1 36 0 83 84 66 82 101 115 111 117 114 99 101 83 84 66 66 121 116 101 65 114 114 97 121 65 99 99 101 115 115 111 114 80 114 111 120 121 0 0 0 0 54 0 9 0 66 121 116 101 65 114 114 97 121 215 13 0 0 33 83 84 66 32 48 32 78 8 12 0 10 0 0 0 83 84 66 86 105 101 119 80 114 111 120 121 0 0 0 0 78 2 13 0 1 0 0 0 83 84 66 67 108 97 115 115 80 114 111 120 121 0 0 0 0 54 0 6 0 83 116 114 105 110 103 7 0 0 0 68 111 108 112 104 105 110 146 0 0 0 13 0 0 0 67 111 110 116 97 105 110 101 114 86 105 101 119 38 0 5 0 65 114 114 97 121 15 0 0 0 0 0 0 0 0 0 0 0 194 0 0 0 2 0 0 0 54 0 12 0 76 97 114 103 101 73 110 116 101 103 101 114 4 0 0 0 0 0 0 68 1 0 2 0 96 0 0 0 0 0 0 0 6 1 11 0 83 121 115 116 101 109 67 111 108 111 114 0 0 0 0 31 0 0 0 0 0 0 0 7 0 0 0 70 5 4 0 2 0 0 0 77 101 110 117 0 0 0 0 0 0 0 0 16 0 0 0 194 0 0 0 2 0 0 0 70 2 15 0 1 0 0 0 67 111 109 109 97 110 100 77 101 110 117 73 116 101 109 0 0 0 0 1 0 0 0 70 4 18 0 2 0 0 0 67 111 109 109 97 110 100 68 101 115 99 114 105 112 116 105 111 110 0 0 0 0 14 1 14 0 83 84 66 83 121 109 98 111 108 80 114 111 120 121 0 0 0 0 146 0 0 0 24 0 0 0 97 114 114 97 110 99 97 114 80 114 111 99 101 115 111 65 110 105 109 97 99 105 111 110 146 0 0 0 8 0 0 0 65 114 114 97 110 99 97 114 1 0 0 0 0 0 0 0 98 1 0 0 0 0 0 0 1 0 0 0 130 1 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 9 0 0 0 105 122 113 117 105 101 114 100 97 146 0 0 0 9 0 0 0 73 122 113 117 105 101 114 100 97 75 2 0 0 0 0 0 0 146 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 7 12 0 66 111 114 100 101 114 76 97 121 111 117 116 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 90 0 0 0 0 0 0 0 128 0 0 0 194 0 0 0 15 0 0 0 0 0 0 0 96 0 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 0 0 68 1 0 2 0 96 2 0 0 0 0 0 0 18 1 0 0 0 0 0 0 31 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 6 4 4 0 70 111 110 116 0 0 0 0 0 0 0 0 16 0 0 0 6 1 7 0 76 79 71 70 79 78 84 0 0 0 0 54 0 9 0 66 121 116 101 65 114 114 97 121 60 0 0 0 240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 2 3 2 1 2 87 105 110 103 100 105 110 103 115 0 15 1 0 0 196 53 15 1 2 0 144 39 0 0 0 0 191 0 253 85 63 1 6 2 5 0 80 111 105 110 116 0 0 0 0 193 0 0 0 193 0 0 0 0 0 0 0 0 0 0 0 6 4 10 0 71 114 105 100 76 97 121 111 117 116 0 0 0 0 3 0 0 0 9 0 0 0 21 0 0 0 21 0 0 0 14 2 26 0 83 84 66 73 100 101 110 116 105 116 121 68 105 99 116 105 111 110 97 114 121 80 114 111 120 121 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 18 0 0 0 73 100 101 110 116 105 116 121 68 105 99 116 105 111 110 97 114 121 194 0 0 0 0 0 0 0 0 0 0 0 6 1 15 0 77 101 115 115 97 103 101 83 101 113 117 101 110 99 101 0 0 0 0 14 2 18 0 83 84 66 67 111 108 108 101 99 116 105 111 110 80 114 111 120 121 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 17 0 0 0 79 114 100 101 114 101 100 67 111 108 108 101 99 116 105 111 110 194 0 0 0 1 0 0 0 6 3 11 0 77 101 115 115 97 103 101 83 101 110 100 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 16 0 0 0 99 114 101 97 116 101 65 116 58 101 120 116 101 110 116 58 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 1 0 0 0 205 1 0 0 18 3 0 0 0 0 0 0 245 1 0 0 41 0 0 0 96 2 0 0 6 1 15 0 87 73 78 68 79 87 80 76 65 67 69 77 69 78 84 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 230 0 0 0 250 0 0 0 250 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 4 0 0 0 90 0 0 0 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 10 0 0 0 80 117 115 104 66 117 116 116 111 110 194 0 0 0 17 0 0 0 0 0 0 0 96 2 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 208 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 130 1 0 0 0 0 0 0 0 2 0 0 146 0 0 0 1 0 0 0 231 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 3 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 1 0 0 0 1 0 0 0 18 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 208 4 0 0 18 4 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 10 0 0 0 105 115 69 110 97 98 108 101 100 58 194 0 0 0 1 0 0 0 32 0 0 0 208 4 0 0 18 4 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 5 0 0 0 116 101 120 116 58 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 231 208 4 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 55 0 0 0 20 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 144 3 0 0 18 3 0 0 0 0 0 0 193 0 0 0 193 0 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 224 4 0 0 194 0 0 0 17 0 0 0 0 0 0 0 96 2 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 160 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 130 1 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 5 0 0 0 97 98 97 106 111 146 0 0 0 1 0 0 0 234 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 3 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 131 0 0 0 1 0 0 0 18 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 160 6 0 0 18 4 0 0 0 0 0 0 224 5 0 0 194 0 0 0 1 0 0 0 32 0 0 0 160 6 0 0 18 4 0 0 0 0 0 0 32 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 234 160 6 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 0 0 0 0 120 0 0 0 20 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 144 3 0 0 144 6 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 224 4 0 0 194 0 0 0 17 0 0 0 0 0 0 0 96 2 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 32 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 178 2 0 0 0 0 0 0 0 0 0 0 16 0 0 0 210 2 0 0 0 0 0 0 242 2 0 0 60 0 0 0 240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 2 3 2 1 2 87 105 110 103 100 105 110 103 115 0 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1 32 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 130 1 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 5 0 0 0 114 111 116 97 114 146 0 0 0 1 0 0 0 200 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 3 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 5 1 0 0 1 0 0 0 18 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 32 8 0 0 18 4 0 0 0 0 0 0 224 5 0 0 194 0 0 0 1 0 0 0 32 0 0 0 32 8 0 0 18 4 0 0 0 0 0 0 32 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 200 32 8 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 130 0 0 0 0 0 0 0 185 0 0 0 20 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 144 3 0 0 144 6 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 224 4 0 0 194 0 0 0 17 0 0 0 0 0 0 0 96 2 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 208 9 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 130 1 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 7 0 0 0 100 101 114 101 99 104 97 146 0 0 0 1 0 0 0 232 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 3 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 135 1 0 0 1 0 0 0 18 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 208 9 0 0 18 4 0 0 0 0 0 0 224 5 0 0 194 0 0 0 1 0 0 0 32 0 0 0 208 9 0 0 18 4 0 0 0 0 0 0 32 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 232 208 9 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 250 0 0 0 20 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 144 3 0 0 144 6 0 0 0 0 0 0 19 0 0 0 144 6 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 90 0 0 0 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 13 0 0 0 82 101 102 101 114 101 110 99 101 86 105 101 119 194 0 0 0 14 0 0 0 0 0 0 0 96 0 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 0 0 68 1 0 2 0 80 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 2 18 0 82 101 115 111 117 114 99 101 73 100 101 110 116 105 102 105 101 114 0 0 0 0 122 0 0 0 0 0 0 0 146 0 0 0 14 0 0 0 71 114 97 112 104 105 99 79 98 106 101 99 116 115 146 0 0 0 23 0 0 0 71 114 97 112 104 105 99 68 114 97 119 105 110 103 80 114 101 115 101 110 116 101 114 146 0 0 0 12 0 0 0 68 101 102 97 117 108 116 32 118 105 101 119 0 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 1 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 1 0 0 0 1 0 0 0 18 3 0 0 0 0 0 0 245 1 0 0 205 1 0 0 80 11 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 250 0 0 0 230 0 0 0 194 0 0 0 0 0 0 0 144 6 0 0 0 0 0 0 19 0 0 0 90 3 0 0 0 0 0 0 112 3 0 0 194 0 0 0 2 0 0 0 80 11 0 0 146 0 0 0 7 0 0 0 100 114 97 119 105 110 103 0 0 0 0 162 3 0 0 0 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 2 0 0 0 18 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 18 3 0 0 0 0 0 0 11 0 0 0 11 0 0 0 18 3 0 0 0 0 0 0 245 1 0 0 245 1 0 0 96 0 0 0 18 4 0 0 0 0 0 0 170 1 0 0 0 0 0 0 146 0 0 0 12 0 0 0 99 111 110 116 101 120 116 77 101 110 117 58 194 0 0 0 1 0 0 0 64 1 0 0 96 0 0 0 130 4 0 0 0 0 0 0 242 2 0 0 44 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 255 0 0 0 255 0 0 0 202 3 0 0 0 0 0 0 224 3 0 0 194 0 0 0 2 0 0 0 80 11 0 0 96 2 0 0 144 6 0 0 0 0 0 0 19 0 0 0 70 5 4 0 3 0 0 0 73 99 111 110 0 0 0 0 0 0 0 0 16 0 0 0 14 2 17 0 83 84 66 83 105 110 103 108 101 116 111 110 80 114 111 120 121 0 0 0 0 78 2 13 0 1 0 0 0 83 84 66 67 108 97 115 115 80 114 111 120 121 0 0 0 0 54 0 6 0 83 116 114 105 110 103 7 0 0 0 68 111 108 112 104 105 110 18 1 0 0 24 0 0 0 73 109 97 103 101 82 101 108 97 116 105 118 101 70 105 108 101 76 111 99 97 116 111 114 14 1 14 0 83 84 66 83 121 109 98 111 108 80 114 111 120 121 0 0 0 0 18 1 0 0 7 0 0 0 99 117 114 114 101 110 116 18 1 0 0 17 0 0 0 67 111 110 116 97 105 110 101 114 86 105 101 119 46 105 99 111 14 2 31 0 83 84 66 69 120 116 101 114 110 97 108 82 101 115 111 117 114 99 101 76 105 98 114 97 114 121 80 114 111 120 121 0 0 0 0 18 1 0 0 16 0 0 0 100 111 108 112 104 105 110 100 114 48 48 52 46 100 108 108 0 0 0 0])!

(ResourceIdentifier class: TetrisPresenter name: 'Default view conBotones') assign: (Object fromBinaryStoreBytes:
#[33 83 84 66 32 48 32 70 2 12 0 1 0 0 0 86 105 101 119 82 101 115 111 117 114 99 101 0 0 0 0 14 1 36 0 83 84 66 82 101 115 111 117 114 99 101 83 84 66 66 121 116 101 65 114 114 97 121 65 99 99 101 115 115 111 114 80 114 111 120 121 0 0 0 0 54 0 9 0 66 121 116 101 65 114 114 97 121 243 13 0 0 33 83 84 66 32 48 32 78 8 12 0 10 0 0 0 83 84 66 86 105 101 119 80 114 111 120 121 0 0 0 0 78 2 13 0 1 0 0 0 83 84 66 67 108 97 115 115 80 114 111 120 121 0 0 0 0 54 0 6 0 83 116 114 105 110 103 7 0 0 0 68 111 108 112 104 105 110 146 0 0 0 13 0 0 0 67 111 110 116 97 105 110 101 114 86 105 101 119 38 0 5 0 65 114 114 97 121 15 0 0 0 0 0 0 0 0 0 0 0 194 0 0 0 2 0 0 0 54 0 12 0 76 97 114 103 101 73 110 116 101 103 101 114 4 0 0 0 0 0 0 68 1 0 2 0 96 0 0 0 0 0 0 0 6 1 11 0 83 121 115 116 101 109 67 111 108 111 114 0 0 0 0 31 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 7 12 0 66 111 114 100 101 114 76 97 121 111 117 116 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 90 0 0 0 0 0 0 0 128 0 0 0 194 0 0 0 15 0 0 0 0 0 0 0 96 0 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 0 0 68 1 0 2 0 80 1 0 0 0 0 0 0 18 1 0 0 0 0 0 0 31 0 0 0 0 0 0 0 7 0 0 0 70 5 4 0 2 0 0 0 77 101 110 117 0 0 0 0 0 0 0 0 16 0 0 0 194 0 0 0 3 0 0 0 70 2 15 0 1 0 0 0 67 111 109 109 97 110 100 77 101 110 117 73 116 101 109 0 0 0 0 1 0 0 0 70 4 18 0 2 0 0 0 67 111 109 109 97 110 100 68 101 115 99 114 105 112 116 105 111 110 0 0 0 0 14 1 14 0 83 84 66 83 121 109 98 111 108 80 114 111 120 121 0 0 0 0 146 0 0 0 9 0 0 0 105 122 113 117 105 101 114 100 97 146 0 0 0 0 0 0 0 75 2 0 0 0 0 0 0 210 1 0 0 0 0 0 0 1 0 0 0 242 1 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 24 0 0 0 97 114 114 97 110 99 97 114 80 114 111 99 101 115 111 65 110 105 109 97 99 105 111 110 146 0 0 0 5 0 0 0 83 116 97 114 116 1 0 0 0 0 0 0 0 210 1 0 0 0 0 0 0 1 0 0 0 242 1 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 7 0 0 0 100 101 114 101 99 104 97 146 0 0 0 0 0 0 0 79 2 0 0 0 0 0 0 146 0 0 0 0 0 0 0 0 0 0 0 6 4 4 0 70 111 110 116 0 0 0 0 0 0 0 0 16 0 0 0 6 1 7 0 76 79 71 70 79 78 84 0 0 0 0 54 0 9 0 66 121 116 101 65 114 114 97 121 60 0 0 0 240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 2 3 2 1 2 87 105 110 103 100 105 110 103 115 0 15 1 0 0 196 53 15 1 2 0 144 39 0 0 0 0 191 0 253 85 63 1 6 2 5 0 80 111 105 110 116 0 0 0 0 193 0 0 0 193 0 0 0 0 0 0 0 0 0 0 0 6 4 10 0 71 114 105 100 76 97 121 111 117 116 0 0 0 0 3 0 0 0 9 0 0 0 21 0 0 0 21 0 0 0 14 2 26 0 83 84 66 73 100 101 110 116 105 116 121 68 105 99 116 105 111 110 97 114 121 80 114 111 120 121 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 18 0 0 0 73 100 101 110 116 105 116 121 68 105 99 116 105 111 110 97 114 121 194 0 0 0 0 0 0 0 0 0 0 0 6 1 15 0 77 101 115 115 97 103 101 83 101 113 117 101 110 99 101 0 0 0 0 14 2 18 0 83 84 66 67 111 108 108 101 99 116 105 111 110 80 114 111 120 121 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 17 0 0 0 79 114 100 101 114 101 100 67 111 108 108 101 99 116 105 111 110 194 0 0 0 2 0 0 0 6 3 11 0 77 101 115 115 97 103 101 83 101 110 100 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 16 0 0 0 99 114 101 97 116 101 65 116 58 101 120 116 101 110 116 58 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 1 0 0 0 205 1 0 0 98 3 0 0 0 0 0 0 245 1 0 0 41 0 0 0 80 1 0 0 98 4 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 12 0 0 0 99 111 110 116 101 120 116 77 101 110 117 58 194 0 0 0 1 0 0 0 176 1 0 0 80 1 0 0 6 1 15 0 87 73 78 68 79 87 80 76 65 67 69 77 69 78 84 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 230 0 0 0 250 0 0 0 250 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 4 0 0 0 90 0 0 0 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 10 0 0 0 80 117 115 104 66 117 116 116 111 110 194 0 0 0 17 0 0 0 0 0 0 0 80 1 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 96 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 242 1 0 0 0 0 0 0 32 2 0 0 146 0 0 0 1 0 0 0 231 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 3 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 1 0 0 0 1 0 0 0 98 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 96 5 0 0 98 4 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 10 0 0 0 105 115 69 110 97 98 108 101 100 58 194 0 0 0 1 0 0 0 32 0 0 0 96 5 0 0 98 4 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 5 0 0 0 116 101 120 116 58 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 231 96 5 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 55 0 0 0 20 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 224 3 0 0 98 3 0 0 0 0 0 0 193 0 0 0 193 0 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 112 5 0 0 194 0 0 0 17 0 0 0 0 0 0 0 80 1 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 48 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 242 1 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 5 0 0 0 97 98 97 106 111 146 0 0 0 1 0 0 0 234 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 3 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 131 0 0 0 1 0 0 0 98 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 48 7 0 0 98 4 0 0 0 0 0 0 112 6 0 0 194 0 0 0 1 0 0 0 32 0 0 0 48 7 0 0 98 4 0 0 0 0 0 0 176 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 234 48 7 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 0 0 0 0 120 0 0 0 20 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 224 3 0 0 32 7 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 112 5 0 0 194 0 0 0 17 0 0 0 0 0 0 0 80 1 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 176 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 2 3 0 0 0 0 0 0 0 0 0 0 16 0 0 0 34 3 0 0 0 0 0 0 66 3 0 0 60 0 0 0 240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 2 3 2 1 2 87 105 110 103 100 105 110 103 115 0 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1 112 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 242 1 0 0 0 0 0 0 26 2 0 0 0 0 0 0 146 0 0 0 5 0 0 0 114 111 116 97 114 146 0 0 0 1 0 0 0 200 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 3 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 5 1 0 0 1 0 0 0 98 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 176 8 0 0 98 4 0 0 0 0 0 0 112 6 0 0 194 0 0 0 1 0 0 0 32 0 0 0 176 8 0 0 98 4 0 0 0 0 0 0 176 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 200 176 8 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 130 0 0 0 0 0 0 0 185 0 0 0 20 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 224 3 0 0 32 7 0 0 0 0 0 0 19 0 0 0 90 0 0 0 0 0 0 0 112 5 0 0 194 0 0 0 17 0 0 0 0 0 0 0 80 1 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 32 0 68 1 0 0 0 96 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 242 0 0 0 8 0 0 0 102 166 25 128 0 0 0 0 242 1 0 0 0 0 0 0 192 2 0 0 146 0 0 0 1 0 0 0 232 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 3 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 135 1 0 0 1 0 0 0 98 3 0 0 0 0 0 0 111 0 0 0 41 0 0 0 96 10 0 0 98 4 0 0 0 0 0 0 112 6 0 0 194 0 0 0 1 0 0 0 32 0 0 0 96 10 0 0 98 4 0 0 0 0 0 0 176 6 0 0 194 0 0 0 1 0 0 0 146 0 0 0 1 0 0 0 232 96 10 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 250 0 0 0 20 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 224 3 0 0 32 7 0 0 0 0 0 0 19 0 0 0 32 7 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 90 0 0 0 0 0 0 0 122 0 0 0 0 0 0 0 160 0 0 0 146 0 0 0 13 0 0 0 82 101 102 101 114 101 110 99 101 86 105 101 119 194 0 0 0 14 0 0 0 0 0 0 0 96 0 0 0 194 0 0 0 2 0 0 0 242 0 0 0 4 0 0 0 0 0 0 68 1 0 2 0 192 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 2 18 0 82 101 115 111 117 114 99 101 73 100 101 110 116 105 102 105 101 114 0 0 0 0 122 0 0 0 0 0 0 0 146 0 0 0 14 0 0 0 71 114 97 112 104 105 99 79 98 106 101 99 116 115 146 0 0 0 23 0 0 0 71 114 97 112 104 105 99 68 114 97 119 105 110 103 80 114 101 115 101 110 116 101 114 146 0 0 0 12 0 0 0 68 101 102 97 117 108 116 32 118 105 101 119 0 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 1 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 1 0 0 0 1 0 0 0 98 3 0 0 0 0 0 0 245 1 0 0 205 1 0 0 192 11 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 250 0 0 0 230 0 0 0 194 0 0 0 0 0 0 0 32 7 0 0 0 0 0 0 19 0 0 0 170 3 0 0 0 0 0 0 192 3 0 0 194 0 0 0 2 0 0 0 192 11 0 0 146 0 0 0 7 0 0 0 100 114 97 119 105 110 103 0 0 0 0 242 3 0 0 0 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 1 0 0 0 98 4 0 0 0 0 0 0 128 4 0 0 194 0 0 0 2 0 0 0 98 3 0 0 0 0 0 0 11 0 0 0 11 0 0 0 98 3 0 0 0 0 0 0 245 1 0 0 245 1 0 0 96 0 0 0 18 5 0 0 0 0 0 0 66 3 0 0 44 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 255 0 0 0 255 0 0 0 26 4 0 0 0 0 0 0 48 4 0 0 194 0 0 0 2 0 0 0 192 11 0 0 80 1 0 0 32 7 0 0 0 0 0 0 19 0 0 0 70 5 4 0 3 0 0 0 73 99 111 110 0 0 0 0 0 0 0 0 16 0 0 0 14 2 17 0 83 84 66 83 105 110 103 108 101 116 111 110 80 114 111 120 121 0 0 0 0 78 2 13 0 1 0 0 0 83 84 66 67 108 97 115 115 80 114 111 120 121 0 0 0 0 54 0 6 0 83 116 114 105 110 103 7 0 0 0 68 111 108 112 104 105 110 18 1 0 0 24 0 0 0 73 109 97 103 101 82 101 108 97 116 105 118 101 70 105 108 101 76 111 99 97 116 111 114 14 1 14 0 83 84 66 83 121 109 98 111 108 80 114 111 120 121 0 0 0 0 18 1 0 0 7 0 0 0 99 117 114 114 101 110 116 18 1 0 0 17 0 0 0 67 111 110 116 97 105 110 101 114 86 105 101 119 46 105 99 111 14 2 31 0 83 84 66 69 120 116 101 114 110 97 108 82 101 115 111 117 114 99 101 76 105 98 114 97 114 121 80 114 111 120 121 0 0 0 0 18 1 0 0 16 0 0 0 100 111 108 112 104 105 110 100 114 48 48 52 46 100 108 108 0 0 0 0])!

