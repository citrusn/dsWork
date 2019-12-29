"Filed out from Dolphin Smalltalk 7"!

GraphicObject subclass: #GraphicPath
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicPath guid: (GUID fromString: '{a6c9d003-b10e-11d5-b8e3-000021fb21b2}')!
GraphicPath comment: '(Shell show:''testGraph'') view subViews first addGraphicObject: self test


DrawingShell show'!
!GraphicPath categoriesForClass!Unclassified! !
!GraphicPath methodsFor!

add: aPoint


	self addPoint: aPoint!

addPoint: aPoint

	self invalidate.
	self points add: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

addPoint: aPoint afterIndex: anIndex

	self invalidate.
	self points add: aPoint afterIndex: anIndex.
	self trigger: #sizeChanged.
	self invalidate.!

addPointHandleFor: aGraphicView index: anIndex

	
	|prev next| 
	prev := ValueKeyedAdaptor subject: self key: anIndex .
	next := ValueKeyedAdaptor subject: self key: anIndex  +1.

	^(GraphicLeftButtonHandle for: self  
					arena: aGraphicView 
					positionAction: [ (Rectangle vertex: prev value vertex: next value )  center]) action:[:aMouseEvent| self addPoint: aMouseEvent position afterIndex: anIndex ].


!

at: anIndex


	^points at: anIndex!

at: index put: aPoint

	"Deberia invalidar solo el rectangulo desde el punto anterior y el siguiiente"
	self invalidate.
	 points at: index put: aPoint.
	self trigger: #sizeChanged.
	self invalidate



!

basicTranslateBy: delta

	self points: (points collect:[:aPoint | aPoint + delta]).!

calculateBounds

	"Ugly but fast!!"
		|newOrigin newCorner|

	self points isEmpty ifTrue:[^0@0 corner: 0@0].
	newOrigin:= newCorner := self points first.
	1 to: self points size do:[:i | 

		newOrigin := (self points at:i) min: newOrigin.
		newCorner := (self points at:i) max: newCorner.
			].
	^newOrigin corner: newCorner.

!

containsPoint: aPoint


	|prev tmp|
	(super containsPoint: aPoint )ifFalse:[^false].
	prev := self points first.
	self points detect:[:point | tmp := point = prev ifTrue:[ false] 
							 ifFalse:[
		(aPoint dist: (aPoint nearestPointAlongLineFrom: prev to: point) )<= 3 ].
	prev:= point. 
	tmp] ifNone:[^false].
	^true!

defaultHandleClasses


	^Array "with:GraphicConnectHandle "with: GraphicMoveHandle with: GraphicDeleteHandle with: GraphicCloneHandle with: GraphicColorHandle !

defaultHandlesFor: aGraphicView


	^((1 to: points size) collect:[:i | self pointHandleFor: aGraphicView index: i]) asOrderedCollection addAll: (super defaultHandlesFor: aGraphicView);
		addAll: ((1 to: points size -1) collect:[:i | self addPointHandleFor: aGraphicView index: i]) ; yourself.
	
	!

initialize


	super initialize.
	points := OrderedCollection new.
!

pointHandleFor: aGraphicView index: anIndex



	^GraphicAspectHandle for: self  
					arena: aGraphicView 
					adaptor: ( ValueKeyedAdaptor subject: self key: anIndex)!

points
	"Private - Answer the value of the receiver's ''points'' instance variable."

	^points!

points: anObject
	"Private - Set the value of the receiver's ''points'' instance variable to the argument, anObject."

	points := anObject!

translateBy: delta

	self invalidate.
	self basicTranslateBy: delta.
	self trigger: #positionChanged.
	self invalidate.! !
!GraphicPath categoriesFor: #add:!public! !
!GraphicPath categoriesFor: #addPoint:!public! !
!GraphicPath categoriesFor: #addPoint:afterIndex:!public! !
!GraphicPath categoriesFor: #addPointHandleFor:index:!public! !
!GraphicPath categoriesFor: #at:!public! !
!GraphicPath categoriesFor: #at:put:!public! !
!GraphicPath categoriesFor: #basicTranslateBy:!public! !
!GraphicPath categoriesFor: #calculateBounds!public! !
!GraphicPath categoriesFor: #containsPoint:!public! !
!GraphicPath categoriesFor: #defaultHandleClasses!public! !
!GraphicPath categoriesFor: #defaultHandlesFor:!public! !
!GraphicPath categoriesFor: #initialize!public! !
!GraphicPath categoriesFor: #pointHandleFor:index:!public! !
!GraphicPath categoriesFor: #points!public! !
!GraphicPath categoriesFor: #points:!public! !
!GraphicPath categoriesFor: #translateBy:!public! !

!GraphicPath class methodsFor!

boundingRectForPoints: points


	|newOrigin newCorner|

	points isEmpty ifTrue:[^0@0 corner: 0@0].
	newOrigin:= newCorner := points first.
	1 to:  points size do:[:i | 

		newOrigin := (points at:i) min: newOrigin.
		newCorner := (points at:i) max: newCorner.
			].
	^newOrigin corner: newCorner.
!

test

	^self new points: ((1 to: 10 ) collect:[:i |  i  @ i * 10] )! !
!GraphicPath class categoriesFor: #boundingRectForPoints:!public! !
!GraphicPath class categoriesFor: #test!public! !

