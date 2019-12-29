"Filed out from Dolphin Smalltalk 7"!

Model subclass: #GraphicObject
	instanceVariableNames: 'parentGraphic metaObject graphicAttributes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObject guid: (GUID fromString: '{7e3e11a1-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicObject comment: ''!
!GraphicObject categoriesForClass!Unclassified! !
!GraphicObject methodsFor!

attach: aSelfLocation to: aGraphicObject location: aGraphicObjectLocation

	"Attachs the receiver to aGraphicObject"

	aGraphicObject 	when: #positionChanged 
				send: #move:nextTo:location: 
				to: self 
				withArguments: (Array with: (aSelfLocation , ':')  asSymbol with: aGraphicObject with: aGraphicObjectLocation).!

attachTo: aGraphicObject location: aSymbol

	"Attachs the receiver to aGraphicObject"

	aGraphicObject 	when: #positionChanged 
				send: #moveNextTo:location: 
				to: self 
				withArguments: (Array with: aGraphicObject with:aSymbol).!

borderColor

	^graphicAttributes borderColor!

borderColor:arg1 

	^graphicAttributes borderColor:arg1 !

borderStyle:arg1 

	^graphicAttributes borderStyle:arg1 !

borderWidth

	^graphicAttributes borderWidth!

borderWidth:arg1 

	^graphicAttributes borderWidth:arg1 !

bottom

	^self bounds bottom!

bottomCenter

	^self bounds bottomCenter!

bottomLeft

	^self bounds bottomLeft!

bottomRight

	^self bounds bottomRight!

bounds
	"Private - Answer the value of the receiver's ''bounds'' instance variable."

	^self calculateBounds!

bringToFront

	self parentGraphic bringToFront: self. !

center

	^self bounds center!

clone


	^self deepCopy!

containsPoint: aPoint


	^self bounds containsPoint: aPoint!

corner

	^self bounds corner!

defaultHandleClasses


	^OrderedCollection new addAll: (Array with:GraphicConnectHandle with: GraphicMoveHandle with: GraphicDeleteHandle with: GraphicCloneHandle )   ; yourself!

defaultHandlesFor: aGraphicView



	^self defaultHandleClasses collect:[:aClass | aClass for: self arena: aGraphicView].!

delete

	^self parentGraphic remove: self!

drawOn: aCanvas


		^graphicAttributes installOn: aCanvas.

!

extent

	^self bounds extent!

graphicAttributes
	"Private - Answer the value of the receiver's ''graphicAttributes'' instance variable."

	^graphicAttributes!

graphicAttributes: anObject
	"Private - Set the value of the receiver's ''graphicAttributes'' instance variable to the argument, anObject."

	graphicAttributes := anObject!

height

	^self bounds height!

initialize


	graphicAttributes := self class defaultGraphicAttributes.
	!

invalidate


	^self invalidateRectangle: self bounds!

invalidateRectangle: aRectangleOrNil


	^self parentGraphic isNil ifFalse:[ self trigger: #invalidateRectangle: with: aRectangleOrNil.
							self parentGraphic invalidateRectangle: aRectangleOrNil].!

left

	^self bounds left!

leftCenter

	^self bounds leftCenter!

metaObject
	"Private - Answer the value of the receiver's ''metaObject'' instance variable."

	^metaObject!

metaObject: anObject
	"Private - Set the value of the receiver's ''metaObject'' instance variable to the argument, anObject."

	metaObject := anObject!

move: aSelfLocation nextTo: aGraphicObject location: aSymbol

	^self perform: aSelfLocation with: (aGraphicObject perform: aSymbol)!

move: aSelfLocation NextTo: aGraphicObject location: aSymbol

	^self perform: aSelfLocation with: (aGraphicObject perform: aSymbol)!

moveNextTo: aGraphicObject location: aSymbol

	^self perform: (aSymbol, ':') asSymbol with: (aGraphicObject perform: aSymbol)!

origin

	^self bounds origin!

parentGraphic
	"Private - Answer the value of the receiver's ''parentGraphic'' instance variable."

	^parentGraphic!

parentGraphic: anObject
	"Private - Set the value of the receiver's ''parentGraphic'' instance variable to the argument, anObject."

	parentGraphic := anObject!

pen
	"Private - Answer the value of the receiver's ''pen'' instance variable."

	^Pen withStyle: (self class penStyleForStyleName: self borderStyle)
		width: self borderWidth 
		color: self borderColor !

position


	^self bounds origin!

publishedAspects
	"Answer a Set of AspectDescriptors that describe the aspects published
	by instances of this class. Add in a instance based aspect for the arrangement
	based on the layout manager of the parent"

	| aspects attributesAspects|
	aspects := super publishedAspects.
	attributesAspects := self graphicAttributes publishedAspects.
	aspects addAll: attributesAspects.
	^aspects.!

right

	^self bounds right!

rightCenter

	^self bounds rightCenter!

sendToBack

	self parentGraphic sendToBack: self. !

top

	^self bounds top!

topCenter

	^self bounds topCenter!

topLeft

	^self bounds topLeft!

topRight

	^self bounds topRight!

width

	^self bounds width! !
!GraphicObject categoriesFor: #attach:to:location:!public! !
!GraphicObject categoriesFor: #attachTo:location:!public! !
!GraphicObject categoriesFor: #borderColor!public! !
!GraphicObject categoriesFor: #borderColor:!public! !
!GraphicObject categoriesFor: #borderStyle:!public! !
!GraphicObject categoriesFor: #borderWidth!public! !
!GraphicObject categoriesFor: #borderWidth:!public! !
!GraphicObject categoriesFor: #bottom!public! !
!GraphicObject categoriesFor: #bottomCenter!public! !
!GraphicObject categoriesFor: #bottomLeft!public! !
!GraphicObject categoriesFor: #bottomRight!public! !
!GraphicObject categoriesFor: #bounds!accessing!private! !
!GraphicObject categoriesFor: #bringToFront!public! !
!GraphicObject categoriesFor: #center!public! !
!GraphicObject categoriesFor: #clone!copying!public! !
!GraphicObject categoriesFor: #containsPoint:!public! !
!GraphicObject categoriesFor: #corner!public! !
!GraphicObject categoriesFor: #defaultHandleClasses!public! !
!GraphicObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicObject categoriesFor: #delete!public! !
!GraphicObject categoriesFor: #drawOn:!public! !
!GraphicObject categoriesFor: #extent!public! !
!GraphicObject categoriesFor: #graphicAttributes!accessing!private! !
!GraphicObject categoriesFor: #graphicAttributes:!accessing!private! !
!GraphicObject categoriesFor: #height!public! !
!GraphicObject categoriesFor: #initialize!public! !
!GraphicObject categoriesFor: #invalidate!public! !
!GraphicObject categoriesFor: #invalidateRectangle:!public! !
!GraphicObject categoriesFor: #left!public! !
!GraphicObject categoriesFor: #leftCenter!public! !
!GraphicObject categoriesFor: #metaObject!accessing!private! !
!GraphicObject categoriesFor: #metaObject:!accessing!private! !
!GraphicObject categoriesFor: #move:nextTo:location:!public! !
!GraphicObject categoriesFor: #move:NextTo:location:!public! !
!GraphicObject categoriesFor: #moveNextTo:location:!public! !
!GraphicObject categoriesFor: #origin!public! !
!GraphicObject categoriesFor: #parentGraphic!accessing!private! !
!GraphicObject categoriesFor: #parentGraphic:!accessing!private! !
!GraphicObject categoriesFor: #pen!accessing!private! !
!GraphicObject categoriesFor: #position!public! !
!GraphicObject categoriesFor: #publishedAspects!public! !
!GraphicObject categoriesFor: #right!public! !
!GraphicObject categoriesFor: #rightCenter!public! !
!GraphicObject categoriesFor: #sendToBack!public! !
!GraphicObject categoriesFor: #top!public! !
!GraphicObject categoriesFor: #topCenter!public! !
!GraphicObject categoriesFor: #topLeft!public! !
!GraphicObject categoriesFor: #topRight!public! !
!GraphicObject categoriesFor: #width!public! !

!GraphicObject class methodsFor!

defaultGraphicAttributes


	^GraphicObjectPenAttribute default!

new


	^super new initialize!

penStyleForStyleName: aSymbol


	^(#(solid dot dash dashDot  ) indexOf: aSymbol ) -1


!

test


	^ (self  new bounds: ( 10@10 extent: 30) )! !
!GraphicObject class categoriesFor: #defaultGraphicAttributes!public! !
!GraphicObject class categoriesFor: #new!public! !
!GraphicObject class categoriesFor: #penStyleForStyleName:!public! !
!GraphicObject class categoriesFor: #test!public! !

