"Filed out from Dolphin Smalltalk 7"!

GraphicTrackerHandle subclass: #GraphicConnectHandle
	instanceVariableNames: 'connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicConnectHandle guid: (GUID fromString: '{547ea8a0-aeb8-11d5-b8e3-000021fb21b2}')!
GraphicConnectHandle comment: '(Shell show:''testGraph'' ) view subViews first addGraphicObject: GraphicEllipse test ; addGraphicObject: GraphicObject test'!
!GraphicConnectHandle categoriesForClass!Unclassified! !
!GraphicConnectHandle methodsFor!

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
	self arena model remove: connection. 
	dest := self arena model graphicObjectAt: aPoint.
	dest isNil ifTrue:[
				^aPoint. ].
	connection   to: dest; from: self target .
	self arena model add: connection.
	connection bringToFront.


	^aPoint!

initialize


	 self alignmentSelector:#center.
	!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

"	super startTrackingAt: aPoint."

	connection := 	self arena model add: (GraphicConnection2 new from: self target ; to: self target ).
	^aPoint! !
!GraphicConnectHandle categoriesFor: #drawOn:!public! !
!GraphicConnectHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicConnectHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicConnectHandle categoriesFor: #initialize!public! !
!GraphicConnectHandle categoriesFor: #startTrackingAt:!private!tracking! !

