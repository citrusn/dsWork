"Filed out from Dolphin Smalltalk 7"!

GraphicObjectManipulationTool subclass: #GraphicObjectTrackingTool
	instanceVariableNames: 'tracker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectTrackingTool guid: (GUID fromString: '{30721920-efe9-11d5-b8e3-d4bacc772835}')!
GraphicObjectTrackingTool comment: ''!
!GraphicObjectTrackingTool categoriesForClass!Unclassified! !
!GraphicObjectTrackingTool methodsFor!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	arena view drawFocusRect: (tracker origin corner: aPreviousPoint) normalize asParameter.
	arena  view drawFocusRect: (tracker origin corner: aPoint) normalize asParameter.

	^aPoint

!

onLeftButtonPressedBack: aMouseEvent
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	(tracker := MouseTracker at: aMouseEvent screenPosition)
		origin: aMouseEvent position;
		startTracking: self.
!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (tracker origin corner: aPoint) normalize.

	^aPoint! !
!GraphicObjectTrackingTool categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicObjectTrackingTool categoriesFor: #onLeftButtonPressedBack:!event handling!public! !
!GraphicObjectTrackingTool categoriesFor: #startTrackingAt:!private!tracking! !

