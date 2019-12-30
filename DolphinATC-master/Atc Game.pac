| package |
package := Package name: 'Atc Game'.
package paxVersion: 1;
	basicComment: 'This is an example of an animated Air Traffic Simulation written in Dolphin Smaltalk. 

The code is intended to be studied in conjunction with a seres of detailed videos showing from start to finish how a complete project such as this can be undertaken in Dolphin Smalltalk. The videos are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

Alternatively, you can just enjoy playing the game. You can open a game window, either by finding and opening  the Atc Game icon in the Samples folder, or by evaluating the following exprerssion:

AtcGameShell show "Evaluate It"

(Version 1.3)'.

package basicPackageVersion: '1.3'.


package classNames
	add: #AtcCity;
	add: #AtcGameSessionManager;
	add: #AtcGameShell;
	add: #AtcModel;
	add: #AtcPlane;
	add: #AtcPresenter;
	add: #AtcView;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\..\Object Arts\Dolphin\System\Random\Dolphin Random Stream';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #AtcCity
	instanceVariableNames: 'name location radius'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #AtcPlane
	instanceVariableNames: 'flightId position velocity color departureCity destinationCity rateOfTurn'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Model subclass: #AtcModel
	instanceVariableNames: 'planes cities'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #AtcPresenter
	instanceVariableNames: 'updateProcess lastUpdateTime'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AtcGameShell
	instanceVariableNames: 'planesPresenter airspacePresenter scorePresenter statusPresenter timer random'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #AtcGameSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleBufferedView subclass: #AtcView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AtcCity guid: (GUID fromString: '{f521aa21-9600-4ff8-ae19-07b5f7a01523}')!
AtcCity comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcCity is a class representing a city location in the model. AtcPlanes fly between cities.

Instance Variables:
	name		<String>
	location		<Point>
	radius		<Integer>

'!
!AtcCity categoriesForClass!Unclassified! !
!AtcCity methodsFor!

box
	^Rectangle center: self location extent: self radius * 2!

color
	"
	ColorDialog showModal
	"
	^(RGB red: 196 green: 182 blue: 132)!

defaultRadius
	^50!

displayOn: aStream
	aStream display: self name!

drawOn: aCanvas 
	" ColorDialog showModal "

	| state |
	state := aCanvas save.
	self radius * 2 to: 500 by: 150 do: 
			[:n | 
			| ringBox |
			ringBox := Rectangle center: self location extent: n.
			ringBox := ringBox expandBy: (Time now asMilliseconds / 4 % 1500 //20).
			aCanvas brush: Brush transparent.
			aCanvas pen: (Pen 
						withStyle: PS_DOT
						width: 1
						color: (RGB red: 224 green: 224 blue: 224)). 
			aCanvas ellipse: ringBox].
	aCanvas
		brush: (Brush color: self color);
		pen: (Pen color: self color);
		ellipse: self box;
		"rectangle: self box;"
		font: (Font name: 'Arial' pixelSize: 10);
		setTextAlign: TA_CENTER;
		backgroundMode: TRANSPARENT;
		text: self name at: self location - (0 @ 6).
	aCanvas restore: state!

headingFrom: anAtcCity 
	| vector arcTan |
	vector := self location - anAtcCity location.
	vector x = 0 ifTrue: [^vector y > 0 ifTrue: [180] ifFalse: [0]].
	arcTan := (vector y / vector x) arcTan radiansToDegrees.
	(vector x > 0 and: [vector y < 0]) ifTrue: [^arcTan + 360 + 90].
	vector x < 0 ifTrue: [^arcTan + 180 + 90].
	^arcTan + 90!

location
	^location!

name
	^name!

radius
	radius ifNil: [radius := self defaultRadius].
	^radius!

setName: aStringName location: aPointLocation radius: anIntegerRadius 
	name := aStringName.
	location := aPointLocation.
	radius := anIntegerRadius! !
!AtcCity categoriesFor: #box!private! !
!AtcCity categoriesFor: #color!public! !
!AtcCity categoriesFor: #defaultRadius!private! !
!AtcCity categoriesFor: #displayOn:!private! !
!AtcCity categoriesFor: #drawOn:!public! !
!AtcCity categoriesFor: #headingFrom:!private! !
!AtcCity categoriesFor: #location!public! !
!AtcCity categoriesFor: #name!public! !
!AtcCity categoriesFor: #radius!private! !
!AtcCity categoriesFor: #setName:location:radius:!initializing!private! !

!AtcCity class methodsFor!

athens
	^self
		name: 'Athens'
		location: 480 @ 490
		radius: 40!

frankfurt
	^self
		name: 'Frankfurt'
		location: 650 @ 250
		radius: 25!

london
	^self
		name: 'London'
		location: 50 @ 50
		radius: 50!

madrid
	^self
		name: 'Madrid'
		location: 50 @ 480
		radius: 30!

name: aStringName location: aPointLocation radius: anIntegerRadius
	^super new
		setName: aStringName
		location: aPointLocation
		radius: anIntegerRadius!

paris
	^self
		name: 'Paris'
		location: 350 @ 210
		radius: 35! !
!AtcCity class categoriesFor: #athens!public! !
!AtcCity class categoriesFor: #frankfurt!public! !
!AtcCity class categoriesFor: #london!public! !
!AtcCity class categoriesFor: #madrid!public! !
!AtcCity class categoriesFor: #name:location:radius:!public! !
!AtcCity class categoriesFor: #paris!public! !

AtcPlane guid: (GUID fromString: '{6613e23f-6c97-44f1-bf97-19e2bd7300ac}')!
AtcPlane comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcPlane is a class representing a plane flying in the game model. AtcPlanes fly between AtcCitys.

Instance Variables:
	flightId		<String>
	position		<Point3D>
	velocity		<Point3D>
	color			<Color>
	departureCity	<AtcCity>
	destinationCity	<AtcCity>
	rateOfTurn		<Number>

'!
!AtcPlane categoriesForClass!Unclassified! !
!AtcPlane methodsFor!

arrivalDetectInModel: anAtcModel 
	(self position asPoint - self destinationCity location) r < self destinationCity radius 
		ifTrue: 
			[anAtcModel trigger: #arrivalOf:at: with: self with: self destinationCity.
			anAtcModel destroyPlane: self]!

box
	^Rectangle center: self position asPoint rounded extent: self boxExtent!

boxExtent
	^20@20!

collisionColor
	^Color red!

collisionDetectInModel: anAtcModel 
	| minDistance planesToTest closestPlane |
	closestPlane := nil.
	planesToTest := anAtcModel planes copyWithout: self.
	minDistance := planesToTest inject: Float fmax
				into: 
					[:currentMin :each | 
					| distance |
					distance := (self position - each position) r.
					distance < currentMin 
						ifTrue: 
							[closestPlane := each.
							distance]
						ifFalse: [currentMin]].
	minDistance > self collisionWarningDistance ifTrue: [^self color: self defaultColor].
	minDistance > self collisionDistance ifTrue: [^self color: self collisionColor].
	anAtcModel
		destroyPlane: self;
		destroyPlane: closestPlane.
	anAtcModel
		trigger: #planesDestroyed: with: (Array with: self with: closestPlane)!

collisionDistance
	^20!

collisionWarningDistance
	^100!

color
	color ifNil: [color := self defaultColor].
	^color!

color: anIndexedColor 
	color := anIndexedColor!

defaultColor
	^Color darkGreen!

departureCity
	^departureCity!

departureCity: anAtcCity 
	departureCity := anAtcCity!

destinationCity
	^destinationCity!

destinationCity: anAtcCity 
	destinationCity := anAtcCity!

drawOn: aCanvas 
	| flText textExtent vector velocity2D state |
	state := aCanvas save.
	aCanvas pen: Pen black.
	velocity2D := self velocity asPoint.
	velocity2D r > 0 
		ifTrue: 
			[vector := (velocity2D * self velocityVectorLength) rounded.
			aCanvas moveTo: self box center.
			aCanvas lineTo: self box center + vector].
	aCanvas setBkMode: TRANSPARENT.
	aCanvas font: (Font name: 'Arial' pointSize: 8).
	aCanvas fillRectangle: self box color: self color.
	aCanvas text: self flightId at: self box bottomRight.
	flText := 'FL' , self flightLevel displayString.
	textExtent := aCanvas textExtent: flText.
	aCanvas text: flText at: self box topLeft - textExtent.
	aCanvas restore: state!

flightId
	"Answer the value of the receiver's 'flightId' instance variable."

	^flightId!

flightId: aString 
	flightId := aString.
	rateOfTurn := 0!

flightLevel
	^self position asPoint3D z // 100!

heading
	| vector arcTan |
	vector := self velocity.
	vector x = 0 ifTrue: [^vector y > 0 ifTrue: [180] ifFalse: [0]].
	arcTan := (vector y / vector x) arcTan radiansToDegrees.
	(vector x > 0 and: [vector y < 0]) ifTrue: [^arcTan+360 + 90].
	(vector x < 0) ifTrue: [^arcTan+180 + 90].
	^arcTan + 90!

heading: headingInDegrees speed: speed 
	|  vector |
	vector := (headingInDegrees - 90) degreesToRadians cos 
				@ (headingInDegrees - 90) degreesToRadians sin.
	vector := vector * speed / vector r.
	self velocity: vector!

height: anInteger 
	self position: position asPoint @ anInteger!

isNearMiss
	^self color = self collisionColor!

position
	"Answer the value of the receiver's 'position' instance variable."

	^position!

position: anObject
	"Set the value of the receiver's 'position' instance variable to the argument."

	position := anObject!

printOn: aStream 
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self flightId;
		nextPut: $)!

rateOfTurn
	^rateOfTurn!

speed
	^self velocity r!

stop
	self velocity: Point3D zero!

turnAmount
	^10!

turnLeft
	rateOfTurn := rateOfTurn - self turnAmount!

turnRight
	rateOfTurn := rateOfTurn+self turnAmount!

updateForTimeInterval: timeInterval inModel: anAtcModel 
	| newPosition newHeading |
	newPosition := self position + (self velocity * timeInterval * 0.00003).
	self position: newPosition.
	newHeading := self heading+(self rateOfTurn * timeInterval * 0.0001).
	self heading: newHeading speed: self speed.
	self collisionDetectInModel: anAtcModel.
	self arrivalDetectInModel: anAtcModel!

velocity
	"Answer the value of the receiver's 'velocity' instance variable."

	^velocity!

velocity: anObject
	"Set the value of the receiver's 'velocity' instance variable to the argument."

	velocity := anObject!

velocityVectorLength
	^0.075! !
!AtcPlane categoriesFor: #arrivalDetectInModel:!private! !
!AtcPlane categoriesFor: #box!public! !
!AtcPlane categoriesFor: #boxExtent!private! !
!AtcPlane categoriesFor: #collisionColor!private! !
!AtcPlane categoriesFor: #collisionDetectInModel:!private! !
!AtcPlane categoriesFor: #collisionDistance!private! !
!AtcPlane categoriesFor: #collisionWarningDistance!private! !
!AtcPlane categoriesFor: #color!public! !
!AtcPlane categoriesFor: #color:!public! !
!AtcPlane categoriesFor: #defaultColor!private! !
!AtcPlane categoriesFor: #departureCity!public! !
!AtcPlane categoriesFor: #departureCity:!public! !
!AtcPlane categoriesFor: #destinationCity!public! !
!AtcPlane categoriesFor: #destinationCity:!public! !
!AtcPlane categoriesFor: #drawOn:!private! !
!AtcPlane categoriesFor: #flightId!accessing!public! !
!AtcPlane categoriesFor: #flightId:!private! !
!AtcPlane categoriesFor: #flightLevel!public! !
!AtcPlane categoriesFor: #heading!public! !
!AtcPlane categoriesFor: #heading:speed:!public! !
!AtcPlane categoriesFor: #height:!public! !
!AtcPlane categoriesFor: #isNearMiss!public! !
!AtcPlane categoriesFor: #position!accessing!public! !
!AtcPlane categoriesFor: #position:!accessing!public! !
!AtcPlane categoriesFor: #printOn:!private! !
!AtcPlane categoriesFor: #rateOfTurn!public! !
!AtcPlane categoriesFor: #speed!public! !
!AtcPlane categoriesFor: #stop!public! !
!AtcPlane categoriesFor: #turnAmount!private! !
!AtcPlane categoriesFor: #turnLeft!public! !
!AtcPlane categoriesFor: #turnRight!public! !
!AtcPlane categoriesFor: #updateForTimeInterval:inModel:!private! !
!AtcPlane categoriesFor: #velocity!accessing!public! !
!AtcPlane categoriesFor: #velocity:!accessing!public! !
!AtcPlane categoriesFor: #velocityVectorLength!private! !

!AtcPlane class methodsFor!

flightId: flightId from: departureCity to: destinationCity speed: speed height: height 
	| plane |
	plane := self 
				flightId: flightId
				position: departureCity location
				velocity: Point3D zero.
	plane height: height.
	plane heading: (destinationCity headingFrom: departureCity) speed: speed.
	plane
		departureCity: departureCity;
		destinationCity: destinationCity.
	^plane!

flightId: aFlightIdString position: aPoint3DPosition velocity: aPoint3DVelocity 
	^(self new)
		flightId: aFlightIdString;
		position: aPoint3DPosition;
		velocity: aPoint3DVelocity;
		yourself! !
!AtcPlane class categoriesFor: #flightId:from:to:speed:height:!public! !
!AtcPlane class categoriesFor: #flightId:position:velocity:!public! !

AtcModel guid: (GUID fromString: '{0de51e27-ed11-43a8-8d24-b78d1bb33c4d}')!
AtcModel comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcModel is a model class reresenting the airspace in which AtcPlanes fly between AtcCitys.

Instance Variables:
	planes		<ListModel of AtcPlanes>
	cities		<ListModel of AtcCities>

'!
!AtcModel categoriesForClass!Unclassified! !
!AtcModel methodsFor!

addPlane: anAtcPlane 
	planes add: anAtcPlane!

cities
	^cities!

destroyPlane: anAtcPlane 
	self planes remove: anAtcPlane ifAbsent: []!

drawOn: aCanvas 
	self cities do: [:each | each drawOn: aCanvas].
	self planes do: [:each | each drawOn: aCanvas].
!

initialize
	super initialize.
	planes := ListModel on: OrderedCollection new.
	cities := ListModel on: OrderedCollection new.
	self initializeCities!

initializeCities
	cities
		add: AtcCity london;
		add: AtcCity paris;
		add: AtcCity frankfurt;
		add: AtcCity madrid;
		add: AtcCity athens!

planes
	^planes!

setCities: aListModel 
	cities := aListModel! !
!AtcModel categoriesFor: #addPlane:!public! !
!AtcModel categoriesFor: #cities!public! !
!AtcModel categoriesFor: #destroyPlane:!public! !
!AtcModel categoriesFor: #drawOn:!private! !
!AtcModel categoriesFor: #initialize!initializing!private! !
!AtcModel categoriesFor: #initializeCities!private! !
!AtcModel categoriesFor: #planes!public! !
!AtcModel categoriesFor: #setCities:!public! !

!AtcModel class methodsFor!

stbConvertFrom: anSTBClassFormat
	"Private - Version 1 adds cities instance variable."

	^
	[:data |
	| newInstance |
	anSTBClassFormat version < 2
		ifTrue: 
			[newInstance := self basicNew.
			1 to: data size do: [:i | newInstance instVarAt: i put: (data at: i)].
			newInstance setCities: (ListModel on: OrderedCollection new)].
	newInstance]!

stbVersion
	"Version 1 adds cities instance variable"

	^2! !
!AtcModel class categoriesFor: #stbConvertFrom:!binary filing!private! !
!AtcModel class categoriesFor: #stbVersion!private! !

AtcPresenter guid: (GUID fromString: '{894f980f-1763-4e2e-9b2e-7531af83ea1c}')!
AtcPresenter comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcPresenter is a presenter that forms the heart of the animated airspace dislplay. AtcPresenter links with AtcModel and AtcView to form an MVP triad.

Instance Variables:
	updateProcess		<Process>
	lastUpdateTime		<Number>

'!
!AtcPresenter categoriesForClass!Unclassified! !
!AtcPresenter methodsFor!

onViewClosed
	self stopUpdateProcess.
	super onViewClosed!

onViewOpened
	super onViewOpened.
	self startUpdateProcess.!

startUpdateProcess
	self stopUpdateProcess.
	lastUpdateTime := Time now asMilliseconds.
	updateProcess := [
			[self updateGame.
			self view repaint.
			Processor sleep: 50] repeat]
				forkAt: Processor userBackgroundPriority!

stopUpdateProcess
	updateProcess ifNil: [^self].
	updateProcess terminate.
	updateProcess := nil!

updateGame
	| currentTime |
	currentTime := Time now asMilliseconds.
	self model planes copy do: [:each | each updateForTimeInterval: currentTime-lastUpdateTime inModel: self model].
	lastUpdateTime := currentTime.
	self trigger: #gameUpdated! !
!AtcPresenter categoriesFor: #onViewClosed!private! !
!AtcPresenter categoriesFor: #onViewOpened!private! !
!AtcPresenter categoriesFor: #startUpdateProcess!public! !
!AtcPresenter categoriesFor: #stopUpdateProcess!public! !
!AtcPresenter categoriesFor: #updateGame!private! !

!AtcPresenter class methodsFor!

defaultModel
	^AtcModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AtcView)  98 14 0 0 98 2 8 1409286144 1 416 525126 1 ##(Smalltalk.AtcModel)  0 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 530 202 208 98 5 459526 ##(Smalltalk.AtcCity)  8 'London' 328198 ##(Smalltalk.Point)  101 101 101 706 8 'Paris' 754 701 421 101 706 8 'Frankfurt' 754 1301 501 101 706 8 'Madrid' 754 101 961 101 706 8 'Athens' 754 961 981 101 0 608 786694 ##(Smalltalk.IndexedColor)  33554471 0 5 0 0 0 416 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 1 0 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 754 3047 21 754 751 591 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 243 5 0 0 10 0 0 0 106 7 0 0 49 1 0 0] 98 0 754 193 193 0 27 )! !
!AtcPresenter class categoriesFor: #defaultModel!public! !
!AtcPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

AtcGameShell guid: (GUID fromString: '{1f0addd3-3507-457f-a0d2-5fecbf48dba0}')!
AtcGameShell comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcGameShell is the top level shell window that controls the game. 

Instance Variables:
	planesPresenter	<ListPresenter of AtcPlanes>
	airspacePresenter	<AtcPresenter>
	scorePresenter		<NumberPresenter>
	statusPresenter		<TextPresenter>
	timer				<Integer>
	random			<Random>



'!
!AtcGameShell categoriesForClass!Unclassified! !
!AtcGameShell methodsFor!

about
	"Pop up a little helpful info. about this sample program."

	self class about!

createComponents
	"Private - Create the presenters contained by the receiver"

	super createComponents.
	planesPresenter := self add: ListPresenter new name: 'planes'.
	airspacePresenter := self add: AtcPresenter new name: 'airspace'.
	scorePresenter := self add: NumberPresenter new name: 'score'.
	statusPresenter := self add: TextPresenter new name: 'status'!

createSchematicWiring
	"Private - Create the event handlers observed by the receiver"

	self 
		when: #timerTick:
		send: #onTimerTick:
		to: self.
	airspacePresenter
		when: #planeClicked:
			send: #onPlaneClicked:
			to: self;
		when: #gameUpdated
			send: #onGameUpdated
			to: self!

isGameRunning
	^timer notNil!

londonToAthens
	self newRandomFlightFrom: AtcCity london to: AtcCity athens!

londonToFrankfurt
	self newRandomFlightFrom: AtcCity london to: AtcCity frankfurt!

londonToMadrid
	self newRandomFlightFrom: AtcCity london to: AtcCity madrid!

londonToParis
	self newRandomFlightFrom: AtcCity london to: AtcCity paris!

model: anAtcModel 
	super model: anAtcModel.
	planesPresenter model: anAtcModel planes.
	airspacePresenter model: anAtcModel.
	anAtcModel
		when: #arrivalOf:at:
			send: #onPlaneArrived
			to: self;
		when: #planesDestroyed:
			send: #onPlanesDestroyed
			to: self!

newRandomFlight
	| departure destination cities |
	cities := self model cities copy.
	departure := cities at: (self random next * cities size) ceiling.
	cities remove: departure.
	destination := cities at: (self random next * cities size) ceiling.
	^self newRandomFlightFrom: departure to: destination!

newRandomFlightFrom: departure to: destination 
	| airlineCodes airlinePrefix flightNo flightId speed plane |
	airlineCodes := #(AF AZ #BA EI FI #IB LH OS SN TK).
	airlinePrefix := airlineCodes at: (self random next * airlineCodes size) ceiling.
	flightNo := (self random next * 800) ceiling + 100.
	flightId := airlinePrefix asString , flightNo displayString.
	speed := (self random next  * 300) ceiling + 300.
	plane := AtcPlane 
				flightId: flightId
				from: departure
				to: destination
				speed: speed
				height: 15000.
	self model addPlane: plane!

onGameUpdated
	| nearMisses |
	self isGameRunning ifFalse: [^self].
	nearMisses := self model planes select: [:each | each isNearMiss].
	nearMisses notEmpty 
		ifTrue: [self status: 'WARNING' color: Color red]
		ifFalse: [self status: '' color: Color darkGreen]!

onPlaneArrived
	[self score: self score+self planeArrivedScore] postToInputQueue!

onPlaneClicked: aPlaneOrNil 
	planesPresenter selectionOrNil: aPlaneOrNil!

onPlanesDestroyed
	[self score: self score - self planeDestroyedScore] postToInputQueue!

onTimerTick: timerId 
	self newRandomFlight!

onViewOpened
	super onViewOpened.
	self resetGame!

planeArrivedScore
	^100!

planeDestroyedScore
	^200!

planeReleaseInterval
	^5000!

queryCommand: aCommandQuery 
	"Private - Enter details about a potential command for the receiver 
	into the <CommandQuery> argument."

	| selector |
	selector := aCommandQuery commandSymbol.
	(#(#turnLeft #turnRight) identityIncludes: selector) 
		ifTrue: 
			[aCommandQuery isEnabled: self selectionOrNil notNil.
			^true].
	^super queryCommand: aCommandQuery!

random
	random ifNil: [random := Random new].
	^random!

resetGame
	self stopGame.
	self model: self class defaultModel.
	self score: 0.
	self status: '' color: Color green.
!

score
	^scorePresenter value!

score: newScore 
	scorePresenter value: newScore.
	scorePresenter view forecolor: (newScore >= 0 ifTrue: [Color darkGreen] ifFalse: [Color darkRed]).
	newScore < 0 ifTrue: [^self youLose].
	newScore >= self winningScore ifTrue: [^self youWin]!

selectionOrNil
	^planesPresenter selectionOrNil!

startGame
	self resetGame.
	self view setTimer: (timer := 1) interval: self planeReleaseInterval.
	self newRandomFlight!

status: aString color: aColor 
	statusPresenter value: aString.
	aColor = statusPresenter view forecolor ifFalse: [statusPresenter view forecolor: aColor]!

stopGame
	self model planes do: [:each | each stop].
	timer 
		ifNotNil: 
			[:value | 
			self view killTimer: value.
			timer := nil]!

turnLeft
	self selectionOrNil turnLeft!

turnRight
	self selectionOrNil turnRight!

winningScore
	^2000!

youLose
	Sound errorBeep.
	self stopGame.
	self status: 'YOU LOSE'  color: Color darkRed
!

youWin
	Sound informationBeep.
	self stopGame.
	self status: 'YOU WIN'  color: Color darkGreen
! !
!AtcGameShell categoriesFor: #about!commands!public! !
!AtcGameShell categoriesFor: #createComponents!initializing!private! !
!AtcGameShell categoriesFor: #createSchematicWiring!initializing!private! !
!AtcGameShell categoriesFor: #isGameRunning!public! !
!AtcGameShell categoriesFor: #londonToAthens!commands!public! !
!AtcGameShell categoriesFor: #londonToFrankfurt!commands!public! !
!AtcGameShell categoriesFor: #londonToMadrid!commands!public! !
!AtcGameShell categoriesFor: #londonToParis!commands!public! !
!AtcGameShell categoriesFor: #model:!private! !
!AtcGameShell categoriesFor: #newRandomFlight!public! !
!AtcGameShell categoriesFor: #newRandomFlightFrom:to:!private! !
!AtcGameShell categoriesFor: #onGameUpdated!private! !
!AtcGameShell categoriesFor: #onPlaneArrived!event handling!private! !
!AtcGameShell categoriesFor: #onPlaneClicked:!event handling!private! !
!AtcGameShell categoriesFor: #onPlanesDestroyed!event handling!private! !
!AtcGameShell categoriesFor: #onTimerTick:!private! !
!AtcGameShell categoriesFor: #onViewOpened!private! !
!AtcGameShell categoriesFor: #planeArrivedScore!constants!private! !
!AtcGameShell categoriesFor: #planeDestroyedScore!constants!private! !
!AtcGameShell categoriesFor: #planeReleaseInterval!constants!private! !
!AtcGameShell categoriesFor: #queryCommand:!commands!private! !
!AtcGameShell categoriesFor: #random!helpers!private! !
!AtcGameShell categoriesFor: #resetGame!commands!public! !
!AtcGameShell categoriesFor: #score!public! !
!AtcGameShell categoriesFor: #score:!accessing!public! !
!AtcGameShell categoriesFor: #selectionOrNil!private!selection! !
!AtcGameShell categoriesFor: #startGame!commands!public! !
!AtcGameShell categoriesFor: #status:color:!private! !
!AtcGameShell categoriesFor: #stopGame!commands!public! !
!AtcGameShell categoriesFor: #turnLeft!commands!public! !
!AtcGameShell categoriesFor: #turnRight!commands!public! !
!AtcGameShell categoriesFor: #winningScore!constants!private! !
!AtcGameShell categoriesFor: #youLose!helpers!private! !
!AtcGameShell categoriesFor: #youWin!helpers!private! !

!AtcGameShell class methodsFor!

about
	"Private - Pop up a little helpful info. about this sample program."

	(MessageBox new)
		caption: ('About <1d>' expandMacrosWith: self);
		text: self aboutTemplate;
		open!

aboutTemplate
	"Private - Answer the text to be used in the receiver's about box"

	^'Dolphin Smalltalk Air Traffic Control Game

Steer the planes to avoid collisions. Using the mouse, select a plane in danger and
type Ctrl+L or Ctrl+R to initiate turns left or right. Reach 2000pts to win or below 
zero to lose.

DISCLAIMER: This software is freely provided purely as an educational tool and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'!

defaultModel
	^AtcModel new!

displayOn: aStream
	"Append, to aStream, a String whose characters are a representation of the receiver as a user
	would want to see it."

	aStream nextPutAll: 'Atc Game'!

initialize
	"Private - Initialize the receiver's class variables
		self initialize
	"

	Smalltalk developmentSystem addSamplesFolderIconFor: self description: self displayString!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1801 1401 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 592 0 196934 1 ##(Smalltalk.RGB)  30780331 0 7 0 0 0 592 562 1 1 0 0 410 8 ##(Smalltalk.StaticText)  98 16 0 592 98 2 8 1140850944 1 720 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #equality 0 674 30780331 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 962 1 994 8 #bePlain 8 '&Plain' 1 1 0 0 0 962 1 994 8 #toggleBold 8 '&Bold' 1 1 0 0 0 962 1 994 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 962 1 994 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 1058 4097 914 0 16 98 3 962 1025 994 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 962 1025 994 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 962 1025 994 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 1058 4097 962 1 994 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 720 786694 ##(Smalltalk.IndexedColor)  33554437 8 4294902439 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1159 11 530 601 91 720 1938 8 #contextMenu: 98 1 928 720 1938 8 #text: 98 1 8 'Steer the planes to avoid collisions. Select a plane in danger and type Ctrl+L or Ctrl+R to initiate turns left or right. Reach 2000pts to win or below zero to lose.' 720 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 2 0 0 5 0 0 0 111 3 0 0 50 0 0 0] 98 0 530 193 193 0 27 410 736 98 16 0 592 98 2 8 1140850945 1 2224 0 0 0 7 0 1698 0 16 1730 8 #[219 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 2224 674 303 8 4294902439 1842 0 0 0 1874 202 208 98 2 1938 1968 98 2 530 11 11 530 601 91 2224 1938 2096 98 1 8 'WARNING' 2224 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 49 1 0 0 50 0 0 0] 98 0 2208 0 27 410 736 98 16 0 592 98 2 8 1140850945 1 2608 0 0 0 7 0 1698 0 16 1730 8 #[221 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 49 67 111 117 114 105 101 114 32 78 101 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 2608 674 7927807 8 4294902439 852742 ##(Smalltalk.IntegerToText)  0 8 '' 0 0 1874 202 208 98 1 1938 1968 98 2 530 611 11 530 549 91 2608 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 5 0 0 0 67 2 0 0 50 0 0 0] 98 0 2208 0 27 234 256 98 4 2224 8 'status' 2608 8 'score' 590342 ##(Smalltalk.Rectangle)  530 11 11 530 11 11 1874 202 208 98 2 1938 1968 98 2 530 1 1 530 1769 111 592 1938 2096 98 1 8 'Info and Score' 592 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 55 0 0 0] 98 3 2224 720 2608 2208 0 27 0 0 0 410 608 98 15 0 416 98 2 8 1140850688 131073 3312 0 0 0 7 0 0 0 3312 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 3312 98 2 8 1140920397 1025 3440 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 842 864 8 #identity 1794 33554433 0 7 0 0 0 3440 1794 33554471 8 4294903271 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 842 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Flight Id' 131 8 #left 3666 3696 3568 3666 8 #<= 3568 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:plane | plane flightId]' 8 #[30 105 226 0 106] 8 #flightId 3936 7 257 0 0 3440 0 1 0 0 3810 8 'Departure' 161 3856 3666 3696 3568 3666 3904 3568 3922 0 0 3954 2 1 3984 8 'doIt' 8 '[:plane | plane departureCity]' 8 #[30 105 226 0 106] 8 #departureCity 4128 7 257 0 0 3440 0 1 0 0 3810 8 'Destination' 157 3856 3666 3696 3568 3666 3904 3568 3922 0 0 3954 2 1 3984 8 'doIt' 8 '[:plane | plane destinationCity]' 8 #[30 105 226 0 106] 8 #destinationCity 4288 7 257 0 0 3440 0 3 0 0 8 #report 3568 0 2145 0 0 1874 202 208 98 2 1938 1968 98 2 530 1 1 530 455 1175 3440 1938 2096 98 1 8 'Flight Id' 3440 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 227 0 0 0 75 2 0 0] 98 0 2208 0 27 327734 ##(Smalltalk.Float)  8 102 102 102 102 102 102 214 63 32 234 256 98 4 410 8 ##(Smalltalk.AtcView)  98 14 0 3312 98 2 8 1149239296 1 4672 525126 2 ##(Smalltalk.AtcModel)  0 3522 202 208 3568 0 842 864 3600 3522 202 208 3568 0 4816 674 14236159 0 7 0 0 0 4672 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 1 530 1303 1175 32 1874 202 208 98 1 1938 1968 98 2 530 467 1 530 1303 1175 4672 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 233 0 0 0 0 0 0 0 116 3 0 0 75 2 0 0] 98 0 2208 0 27 8 'airspace' 3440 8 'planes' 0 1874 202 208 98 2 1938 1968 98 2 530 1 111 530 1769 1175 3312 1938 2096 98 1 8 'Main Game Area' 3312 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 116 3 0 0 130 2 0 0] 98 3 3440 410 8 ##(Smalltalk.Splitter)  98 12 0 3312 98 2 8 1140850688 1 5328 0 482 8 4278190080 0 7 0 0 0 5328 1874 202 208 98 1 1938 1968 98 2 530 455 1 530 13 1175 5328 2146 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 227 0 0 0 0 0 0 0 233 0 0 0 75 2 0 0] 98 0 2208 0 27 4672 2208 0 27 234 256 3568 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 914 0 16 98 5 962 1 994 8 #startGame 8 '&Start Game' 1 1 0 0 0 1058 4097 962 1 994 8 #resetGame 8 '&Reset Game' 1 1 0 0 0 1058 4097 962 1 994 8 #exit 8 '&Quit' 17639 1 0 0 0 8 '&Game' 0 134217729 0 0 28727 0 0 914 0 16 98 6 962 1 994 8 #newRandomFlight 8 '&New Random Flight' 9373 1 0 0 0 1058 4097 962 1 994 8 #londonToParis 8 'London->&Paris' 1 1 0 0 0 962 1 994 8 #londonToFrankfurt 8 'London->&Frankfurt' 1 1 0 0 0 962 1 994 8 #londonToAthens 8 'London->&Athens' 1 1 0 0 0 962 1 994 8 #londonToMadrid 8 'London->&Madrid' 1 1 0 0 0 8 '&Flight' 0 134217729 0 0 28739 0 0 914 0 16 98 2 962 1 994 8 #turnLeft 8 'Turn Left' 9369 1 0 0 0 962 1 994 8 #turnRight 8 'Turn Right' 9381 1 0 0 0 8 '&Course' 0 134217729 0 0 28745 0 0 962 1 994 8 #about 8 '&About' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1874 202 208 98 3 1938 1968 98 2 530 2923 21 530 1801 1401 416 1938 2096 98 1 8 'Don''t Panic!! - An Air Traffic Simulation for Dolphin Smalltalk' 416 1938 8 #updateMenuBar 3568 416 2146 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 181 5 0 0 10 0 0 0 57 9 0 0 198 2 0 0] 98 2 592 3312 2208 0 27 )!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system.
		self uninitialize
	"

	Smalltalk developmentSystem
		removeSystemFolderIconNamed: self displayString! !
!AtcGameShell class categoriesFor: #about!enquiries!private! !
!AtcGameShell class categoriesFor: #aboutTemplate!enquiries!private! !
!AtcGameShell class categoriesFor: #defaultModel!public! !
!AtcGameShell class categoriesFor: #displayOn:!displaying!public! !
!AtcGameShell class categoriesFor: #initialize!development!initializing!private! !
!AtcGameShell class categoriesFor: #resource_Default_view!public!resources-views! !
!AtcGameShell class categoriesFor: #uninitialize!development!initializing!private! !

AtcGameSessionManager guid: (GUID fromString: '{93751ee7-f4e8-4d06-8837-43aab87de624}')!
AtcGameSessionManager comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcSessionManager is a RuntimeSessionManager that allows the Atc Game package to be exported into a Windows executable. It is responsible for creating the AtcShellPresenter when the application starts. Note that Dolphin Professional is required to create Windows executables.

'!
!AtcGameSessionManager categoriesForClass!Unclassified! !
!AtcGameSessionManager methodsFor!

main
	"Start up the Dolphin Calculator sample application"

	self mainShellClass show! !
!AtcGameSessionManager categoriesFor: #main!operations-startup!public! !

!AtcGameSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^AtcGameShell! !
!AtcGameSessionManager class categoriesFor: #mainShellClass!constants!public! !

AtcView guid: (GUID fromString: '{c6a8563b-3d2c-4ba3-bd11-60d97103b24e}')!
AtcView comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcView is a view that displays the contents of the animated airspace held in an AtcModel. AtcView links with AtcModel and AtcPresenter to form an MVP triad.

'!
!AtcView categoriesForClass!Unclassified! !
!AtcView methodsFor!

onLeftButtonPressed: aMouseEvent
	| selection |
	selection := self model planes detect: [:each | each box containsPoint: aMouseEvent position]
				ifNone: [].
	self presenter trigger: #planeClicked: with: selection!

render
	| canvas |
	canvas := self canvas.
	canvas
		backcolor: self backcolor;
		erase.
	self model drawOn: canvas.
	super render!

visualObjectAtPoint: aPoint
	^self model planes , self model cities detect: [:each | each box containsPoint: aPoint]
		ifNone: [super visualObjectAtPoint: aPoint]! !
!AtcView categoriesFor: #onLeftButtonPressed:!private! !
!AtcView categoriesFor: #render!public! !
!AtcView categoriesFor: #visualObjectAtPoint:!public! !

!AtcView class methodsFor!

defaultModel
	^AtcModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AtcView)  98 14 0 0 98 2 8 1409286144 1 416 525126 1 ##(Smalltalk.AtcModel)  0 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 530 202 208 98 5 459526 ##(Smalltalk.AtcCity)  8 'London' 328198 ##(Smalltalk.Point)  101 101 101 706 8 'Paris' 754 701 421 101 706 8 'Frankfurt' 754 1301 501 101 706 8 'Madrid' 754 101 961 101 706 8 'Athens' 754 961 981 101 0 608 786694 ##(Smalltalk.IndexedColor)  33554471 0 5 0 0 0 416 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 1 0 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 754 3047 21 754 751 591 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 243 5 0 0 10 0 0 0 106 7 0 0 49 1 0 0] 98 0 754 193 193 0 27 )! !
!AtcView class categoriesFor: #defaultModel!public! !
!AtcView class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

