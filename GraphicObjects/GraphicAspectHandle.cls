"Filed out from Dolphin Smalltalk 7"!

GraphicTrackerHandle subclass: #GraphicAspectHandle
	instanceVariableNames: 'adaptor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicAspectHandle guid: (GUID fromString: '{aea81b00-b209-11d5-b8e3-000021fb21b2}')!
GraphicAspectHandle comment: ''!
!GraphicAspectHandle categoriesForClass!Unclassified! !
!GraphicAspectHandle methodsFor!

adaptor
	"Private - Answer the value of the receiver's ''adaptor'' instance variable."

	^adaptor!

adaptor: anObject
	"Private - Set the value of the receiver's ''adaptor'' instance variable to the argument, anObject."

	adaptor := anObject!

calculateBounds

	"Ignore the alignmentSelector"

	^(self adaptor value  ) + self offset - 2 extent: 5!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"

	self adaptor value: aPoint.
	self arena view invalidateRectangle: self bounds.
	
	^aPoint

!

drawOn: aCanvas

	aCanvas setDefaultAttributes.
	^aCanvas fillRectangle: self bounds brush: Brush black

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	"arena drawFocusRect: (self bounds  copy moveTo:  aPoint ) asParameter."
"	arena invalidateRect: (tracker origin corner: aPoint).
	self target drawOn: arena canvas."!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	| clone|
	self adaptor value: aPoint.
	super endTrackingAt: aPoint.


	^aPoint! !
!GraphicAspectHandle categoriesFor: #adaptor!public! !
!GraphicAspectHandle categoriesFor: #adaptor:!public! !
!GraphicAspectHandle categoriesFor: #calculateBounds!public! !
!GraphicAspectHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicAspectHandle categoriesFor: #drawOn:!public! !
!GraphicAspectHandle categoriesFor: #drawTrackingAt:!public! !
!GraphicAspectHandle categoriesFor: #endTrackingAt:!public! !

!GraphicAspectHandle class methodsFor!

for: aGraphicObject arena:aGraphicView adaptor: aValueAdaptor



	^self for: aGraphicObject arena:aGraphicView  adaptor: aValueAdaptor  positionAction: aValueAdaptor


!

for: aGraphicObject arena:aGraphicView adaptor: aValueAdaptor positionAction: anAction



	^(self for: aGraphicObject arena:aGraphicView ) adaptor: aValueAdaptor ; positionAction: anAction


!

resizeFor: aGraphicObject arena: aGraphicView 



	^(self for: aGraphicObject arena:aGraphicView ) adaptor: (aGraphicObject aspectValue:#corner).


! !
!GraphicAspectHandle class categoriesFor: #for:arena:adaptor:!public! !
!GraphicAspectHandle class categoriesFor: #for:arena:adaptor:positionAction:!public! !
!GraphicAspectHandle class categoriesFor: #resizeFor:arena:!public! !

