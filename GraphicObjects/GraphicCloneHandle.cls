"Filed out from Dolphin Smalltalk 7"!

GraphicTrackerHandle subclass: #GraphicCloneHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCloneHandle guid: (GUID fromString: '{6536a8e4-b09e-11d5-b8e3-000021fb21b2}')!
GraphicCloneHandle comment: ''!
!GraphicCloneHandle categoriesForClass!Unclassified! !
!GraphicCloneHandle methodsFor!

drawOn: aCanvas


	^aCanvas polygon: (Array with: self position with: 0@4 + self position with: 4@0 + self position)

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (target  bounds copy moveBy:  aPoint - tracker origin) asParameter.!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	| clone|
	super endTrackingAt: aPoint.
	self arena model  add: (clone := self target clone translateBy:  aPoint - tracker origin ).
	self arena removeAllHandles.
	self arena addHandleTo: clone.

	^aPoint!

offset



	^##(20@-10)! !
!GraphicCloneHandle categoriesFor: #drawOn:!public! !
!GraphicCloneHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicCloneHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicCloneHandle categoriesFor: #offset!public! !

