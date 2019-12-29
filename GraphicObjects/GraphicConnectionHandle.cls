"Filed out from Dolphin Smalltalk 7"!

GraphicTrackerHandle subclass: #GraphicConnectionHandle
	instanceVariableNames: 'connectionEndSelector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicConnectionHandle guid: (GUID fromString: '{a8bb9280-b2fe-11d5-b8e3-000021fb21b2}')!
GraphicConnectionHandle comment: ''!
!GraphicConnectionHandle categoriesForClass!Unclassified! !
!GraphicConnectionHandle methodsFor!

calculateBounds


	^(self target perform: (connectionEndSelector copyFrom: 1 to: connectionEndSelector size -1)asSymbol )  center extent: 5!

connectionEndSelector
	"Private - Answer the value of the receiver's ''connectionEndSelector'' instance variable."

	^connectionEndSelector!

connectionEndSelector: anObject
	"Private - Set the value of the receiver's ''connectionEndSelector'' instance variable to the argument, anObject."

	connectionEndSelector := anObject!

drawOn: aCanvas


	^aCanvas ellipse: self bounds

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (self bounds moveTo:  aPoint ) asParameter.!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|dest|
	super endTrackingAt: aPoint.
	dest := self arena model graphicObjectAt: aPoint.
	dest isNil ifTrue:[^aPoint].
	self target perform: connectionEndSelector with: dest.


	^aPoint!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	self drawTrackingAt: aPoint.
	^aPoint! !
!GraphicConnectionHandle categoriesFor: #calculateBounds!public! !
!GraphicConnectionHandle categoriesFor: #connectionEndSelector!accessing!private! !
!GraphicConnectionHandle categoriesFor: #connectionEndSelector:!accessing!private! !
!GraphicConnectionHandle categoriesFor: #drawOn:!public! !
!GraphicConnectionHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicConnectionHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicConnectionHandle categoriesFor: #startTrackingAt:!private!tracking! !

