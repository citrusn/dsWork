| package |
package := Package name: 'Lapiz'.
package paxVersion: 1;
	basicComment: 'Lapiz para Dolphin Smalltalk
Por Andres Otaduy
<aotaduy@frro.utn.edu.ar>

Lapiz es un package que proporciona una tortuga como la de Logo, o como el Pen de SmalltalkExpress pero solo con las funciones de tortuga.

Es muy interesante para dibujar ya que permite la realizacion de graficos especificando posiciones y angulos relativos.

Para ver algun ejemplo evaluar:
Lapiz example
'.


package classNames
	add: #Lapiz;
	add: #LapizPointRecorder;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #Lapiz
	instanceVariableNames: 'canvas location direction penDown'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Lapiz subclass: #LapizPointRecorder
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Lapiz guid: (GUID fromString: '{0ad9f620-9198-11d4-94ca-000021fb21b2}')!
Lapiz comment: ''!
!Lapiz categoriesForClass!Unclassified! !
!Lapiz methodsFor!

arrowTo: aPoint



	self direction:(    aPoint- self location ) degrees.
	self go: ( aPoint- self location ) r rounded- 6.
	self turn: -90 ; 
		go: -3 ;
		regularPolygon: 3 size: 6.
		!

canvas
	"Private - Answer the value of the receiver's ''canvas'' instance variable."

	^canvas!

canvas: anObject
	"Private - Set the value of the receiver's ''canvas'' instance variable to the argument, anObject."

	canvas := anObject!

center

	self goto: canvas extent / 2.!

direction
	"Private - Answer the value of the receiver's ''direction'' instance variable."

	^direction!

direction: anObject
	"Private - Set the value of the receiver's ''direction'' instance variable to the argument, anObject."

	direction :=  anObject \\ 360!

directionRadians: aFloat


	self direction: aFloat radiansToDegrees



!

go: unInteger
	"avanza un numero de puntos en la Direccion Actual" 

	^self goto: unInteger * (direction degreesToRadians cos @  direction degreesToRadians sin) + location




!

goto: aPoint

	"Move the receiver to position aPoint. If the pen is down, a line will be 
	drawn from the current position to the new one using the receiver's 
	direction does not change.
		From Squeak"
	| old |
	old :=  location.
	location := aPoint.
	penDown ifTrue: [canvas moveTo: old rounded.
				canvas lineTo: location rounded.]

!

initialize

	location := 0@0.
	direction := 0.
	penDown  := true.!

location
	"Private - Answer the value of the receiver's ''location'' instance variable."

	^location!

location: anObject
	"Private - Set the value of the receiver's ''location'' instance variable to the argument, anObject."

	location := anObject!

penDown


	 penDown := true!

penUp


	 penDown := false!

regularPolygon: sides size: anInteger


	|angle|
	angle := 360 / sides.
	sides timesRepeat:[
		self go: anInteger ; turn: angle
	].!

squareArrowHeadExtent: aPoint


	|hip|
	hip := (aPoint x / 4 @ (aPoint y / 2)) r. 
	self turn: -90; 
		go: aPoint x / 4 ;
		turn: 90;
		go: aPoint y / 2 ;
		turn: -90;
		go: aPoint x / 4 ;
		turn: 135;
		go: hip;
		turn: 90;
		go: hip;
		turn:135;
		go: aPoint x/4;
		turn: -90;
		go: aPoint y/2;
		turn: 90;
		go: aPoint x /4.


		
		


!

turn: degrees

	direction :=  (direction + degrees) \\ 360.

! !
!Lapiz categoriesFor: #arrowTo:!public! !
!Lapiz categoriesFor: #canvas!accessing!private! !
!Lapiz categoriesFor: #canvas:!accessing!private! !
!Lapiz categoriesFor: #center!public! !
!Lapiz categoriesFor: #direction!accessing!private! !
!Lapiz categoriesFor: #direction:!accessing!private! !
!Lapiz categoriesFor: #directionRadians:!public! !
!Lapiz categoriesFor: #go:!public! !
!Lapiz categoriesFor: #goto:!public! !
!Lapiz categoriesFor: #initialize!public! !
!Lapiz categoriesFor: #location!accessing!private! !
!Lapiz categoriesFor: #location:!accessing!private! !
!Lapiz categoriesFor: #penDown!public! !
!Lapiz categoriesFor: #penUp!public! !
!Lapiz categoriesFor: #regularPolygon:size:!public! !
!Lapiz categoriesFor: #squareArrowHeadExtent:!public! !
!Lapiz categoriesFor: #turn:!public! !

!Lapiz class methodsFor!

canvas: unCanvas
	^self new canvas: unCanvas!

example
	"Lapiz 
	self exampleLapiz
	"

	| lapiz |
	lapiz := self canvas: (Shell show view
						showMaximized;
						canvas).
	lapiz center.
	3 to: 20 do: [:i | i timesRepeat: 
					[lapiz go: 20.
					lapiz turn: 360 / i]]!

new
	^super new initialize!

test
	"Lapiz 
		self testa FlipperInspector
	"

	| lapiz |
	lapiz := self canvas: Shell show view canvas.
	lapiz center.
	3 to: 20 do: [:i | i timesRepeat: 
					[lapiz go: 10.
					lapiz turn: 360 / i]].
	^lapiz inspect! !
!Lapiz class categoriesFor: #canvas:!public! !
!Lapiz class categoriesFor: #example!public! !
!Lapiz class categoriesFor: #new!public! !
!Lapiz class categoriesFor: #test!public! !

LapizPointRecorder guid: (GUID fromString: '{ea5e7800-c28b-11d5-b8e3-000021fb21b2}')!
LapizPointRecorder comment: ''!
!LapizPointRecorder categoriesForClass!Unclassified! !
!LapizPointRecorder methodsFor!

goto: aPoint

	"Move the receiver to position aPoint. If the pen is down, a line will be 
	drawn from the current position to the new one using the receiver's 
	direction does not change.
		From Squeak"
	| old |
	location := aPoint.
	penDown ifTrue: [points add: aPoint]

!

initialize

	super initialize.
	points := OrderedCollection new.!

points
	"Private - Answer the value of the receiver's ''points'' instance variable."

	^points!

points: anObject
	"Private - Set the value of the receiver's ''points'' instance variable to the argument, anObject."

	points := anObject! !
!LapizPointRecorder categoriesFor: #goto:!public! !
!LapizPointRecorder categoriesFor: #initialize!public! !
!LapizPointRecorder categoriesFor: #points!accessing!private! !
!LapizPointRecorder categoriesFor: #points:!accessing!private! !

"Binary Globals"!

