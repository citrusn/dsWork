"Filed out from Dolphin Smalltalk 7"!

GraphicObject subclass: #GraphicConnection2
	instanceVariableNames: 'fromAction endAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicConnection2 guid: (GUID fromString: '{93cc2360-f040-11d5-b8e3-a73e87cebb00}')!
GraphicConnection2 comment: 'DrawingShell show'!
!GraphicConnection2 categoriesForClass!Unclassified! !
!GraphicConnection2 methodsFor!

basicTranslateBy: anOffset

!

bounds


	^(Rectangle vertex: fromAction value vertex: endAction value) expandedBy: 4!

containsPoint: aPoint



	(super containsPoint: aPoint )ifFalse:[^false].

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: fromAction value to: endAction value) )<= 3!

defaultHandlesFor: aGraphicView

	|col|
	col := OrderedCollection new.
"	col addAll: (super defaultHandlesFor: aGraphicView)."
	col
	add: (GraphicDeleteHandle for: self arena: aGraphicView ).
	^col!

drawOn: aCanvas

	super drawOn: aCanvas.	
	^(Lapiz canvas: aCanvas ) penUp; 
					      goto: fromAction value; 
					      penDown;
					      arrowTo: endAction value.



!

endAction
	"Private - Answer the value of the receiver's ''endAction'' instance variable."

	^endAction!

endAction: anObject
	"Private - Set the value of the receiver's ''endAction'' instance variable to the argument, anObject."

	endAction := anObject!

from: anotherGraphicObject 

	self fromAction isNil ifFalse:[ fromAction receiver removeEventsTriggeredFor: self].
	self fromAction: (MessageSend receiver: anotherGraphicObject selector: #center).
	anotherGraphicObject when: #invalidateRectangle: send: #invalidate to: self .


!

fromAction
	"Private - Answer the value of the receiver's ''fromAction'' instance variable."

	^fromAction!

fromAction: anObject
	"Private - Set the value of the receiver's ''fromAction'' instance variable to the argument, anObject."

	fromAction := anObject!

to: anotherGraphicObject 

	self endAction isNil ifFalse:[ endAction receiver removeEventsTriggeredFor: self].
	self endAction: (MessageSend receiver: anotherGraphicObject selector: #center).
	anotherGraphicObject when: #invalidateRectangle: send: #invalidate to: self .
! !
!GraphicConnection2 categoriesFor: #basicTranslateBy:!public! !
!GraphicConnection2 categoriesFor: #bounds!public! !
!GraphicConnection2 categoriesFor: #containsPoint:!public! !
!GraphicConnection2 categoriesFor: #defaultHandlesFor:!public! !
!GraphicConnection2 categoriesFor: #drawOn:!public! !
!GraphicConnection2 categoriesFor: #endAction!accessing!private! !
!GraphicConnection2 categoriesFor: #endAction:!accessing!private! !
!GraphicConnection2 categoriesFor: #from:!public! !
!GraphicConnection2 categoriesFor: #fromAction!accessing!private! !
!GraphicConnection2 categoriesFor: #fromAction:!accessing!private! !
!GraphicConnection2 categoriesFor: #to:!public! !

!GraphicConnection2 class methodsFor!

from: aGraphicObject
to: anotherGraphicObject

^self new from: aGraphicObject ;
			to:  anotherGraphicObject .
	
! !
!GraphicConnection2 class categoriesFor: #from:to:!public! !

