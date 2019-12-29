"Filed out from Dolphin Smalltalk 7"!

GraphicTrackerHandle subclass: #GraphicMoveHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicMoveHandle guid: (GUID fromString: '{7e3e11a6-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicMoveHandle comment: ''!
!GraphicMoveHandle categoriesForClass!Unclassified! !
!GraphicMoveHandle methodsFor!

calculateBounds


	^target bounds!

containsPoint: aPoint

	^self target containsPoint: aPoint!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	self eraseTrackingAt: aPreviousPoint.
	self drawTrackingAt: aPoint.
		self arena selection do:[:aGraphicObject |  	aGraphicObject translateBy: aPoint - aPreviousPoint].
	^aPoint

!

drawOn: ignore!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."


	super endTrackingAt: aPoint.


	^aPoint!

offset



	^##(-10@ -10)!

target: anObject
	"Private - Set the value of the receiver's ''target'' instance variable to the argument, anObject."
	
"	anObject  when: #invalidateRectangle: send: #invalidate to: self."
"	anObject  when: #positionChanged send: #invalidate to: self.
	anObject  when: #sizeChanged send: #invalidate to: self."
	target := anObject.
	!

targetOriginOffset



	^##(-10@-10)! !
!GraphicMoveHandle categoriesFor: #calculateBounds!public! !
!GraphicMoveHandle categoriesFor: #containsPoint:!public! !
!GraphicMoveHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicMoveHandle categoriesFor: #drawOn:!public! !
!GraphicMoveHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicMoveHandle categoriesFor: #offset!public! !
!GraphicMoveHandle categoriesFor: #target:!accessing!private! !
!GraphicMoveHandle categoriesFor: #targetOriginOffset!public! !

