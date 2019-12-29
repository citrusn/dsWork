"Filed out from Dolphin Smalltalk 7"!

GraphicHandle subclass: #GraphicTrackerHandle
	instanceVariableNames: 'tracker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle guid: (GUID fromString: '{6536a8e3-b09e-11d5-b8e3-000021fb21b2}')!
GraphicTrackerHandle comment: ''!
!GraphicTrackerHandle categoriesForClass!Unclassified! !
!GraphicTrackerHandle methodsFor!

cancelTrackingAt: arg1
	"This is an auto-generated target implementation for the protocol <mouseTrackerTarget>
	and remains to be correctly implemented."

	^Error notYetImplemented!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	self eraseTrackingAt: aPreviousPoint.
	self drawTrackingAt: aPoint.

	^aPoint

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	
	self eraseTrackingAt: aPoint.
	self invalidate.
	^aPoint!

eraseTrackingAt: aPoint


	self drawTrackingAt: aPoint.

	^aPoint

!

onLeftButtonDrag: aMouseEvent


		(tracker := MouseTracker at: aMouseEvent screenPosition)
		origin: aMouseEvent position;
		startTracking: self.
!

onLeftButtonPressed: aMouseEvent


	!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	self drawTrackingAt: aPoint.
	^aPoint!

tracker
	"Private - Answer the value of the receiver's ''tracker'' instance variable."

	^tracker!

tracker: anObject
	"Private - Set the value of the receiver's ''tracker'' instance variable to the argument, anObject."

	tracker := anObject! !
!GraphicTrackerHandle categoriesFor: #cancelTrackingAt:!public! !
!GraphicTrackerHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #eraseTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #onLeftButtonDrag:!public! !
!GraphicTrackerHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicTrackerHandle categoriesFor: #startTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #tracker!accessing!private! !
!GraphicTrackerHandle categoriesFor: #tracker:!accessing!private! !

GraphicTrackerHandle methodProtocol: #mouseTrackerTarget attributes: #(#readOnly) selectors: #(#~~ #~= #= #== #cancelTrackingAt: #class #continueTrackingAt:from: #copy #doesNotUnderstand: #endTrackingAt: #error: #hash #identityHash #isKindOf: #isMemberOf: #isNil #notNil #perform: #perform:with: #perform:with:with: #perform:with:with:with: #perform:withArguments: #printOn: #printString #respondsTo: #startTrackingAt: #yourself)!

