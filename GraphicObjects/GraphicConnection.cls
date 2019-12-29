"Filed out from Dolphin Smalltalk 7"!

GraphicLine subclass: #GraphicConnection
	instanceVariableNames: 'from to'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicConnection guid: (GUID fromString: '{547ea8a1-aeb8-11d5-b8e3-000021fb21b2}')!
GraphicConnection comment: '(Shell show:''testGraph'' ) view subViews first addGraphicObject: GraphicEllipse test ; addGraphicObject: GraphicObject test


self allInstances'!
!GraphicConnection categoriesForClass!Unclassified! !
!GraphicConnection methodsFor!

bounds


	^Rectangle vertex: from center vertex: to center!

containsPoint: aPoint



	"(super containsPoint: aPoint )ifFalse:[^false]."

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: from bounds center  to: to bounds center) )<= 3!

defaultHandlesFor: aGraphicView

	|col|
	col := OrderedCollection new.
"	col addAll: (super defaultHandlesFor: aGraphicView)."
	col add: ((GraphicConnectionHandle for: self arena: aGraphicView ) connectionEndSelector: #to:); 
	add: (GraphicDeleteHandle for: self arena: aGraphicView ).
	^col!

drawOn: aCanvas

	
	^(Lapiz canvas: aCanvas ) penUp; 
					      goto: from center; 
					      penDown;
					      arrowTo: to center.



!

from
	"Private - Answer the value of the receiver's ''from'' instance variable."

	^from!

from: anObject
	"Private - Set the value of the receiver's ''from'' instance variable to the argument, anObject."

	anObject when: #invalidateRectangle:  send: #invalidate to: self.
	from := anObject.
"	self invalidate."
	self trigger: #fromChanged.!

recalculateCorner


	bounds corner: to center.
!

recalculateOrigin


	bounds origin: from center.
!

to
	"Private - Answer the value of the receiver's ''to'' instance variable."

	^to!

to: anObject
	"Private - Set the value of the receiver's ''to'' instance variable to the argument, anObject."

	anObject when: #invalidateRectangle:  send: #invalidate  to: self.
	to := anObject.
	self invalidate.
	self trigger: #toChanged.! !
!GraphicConnection categoriesFor: #bounds!public! !
!GraphicConnection categoriesFor: #containsPoint:!public! !
!GraphicConnection categoriesFor: #defaultHandlesFor:!public! !
!GraphicConnection categoriesFor: #drawOn:!public! !
!GraphicConnection categoriesFor: #from!accessing!private! !
!GraphicConnection categoriesFor: #from:!accessing!private! !
!GraphicConnection categoriesFor: #recalculateCorner!public! !
!GraphicConnection categoriesFor: #recalculateOrigin!public! !
!GraphicConnection categoriesFor: #to!accessing!private! !
!GraphicConnection categoriesFor: #to:!accessing!private! !

!GraphicConnection class methodsFor!

from: aGraphicObject
to: anotherGraphicObject


	^self new from: aGraphicObject;
			to: anotherGraphicObject;
			bounds: (Rectangle vertex:aGraphicObject center vertex: anotherGraphicObject center)
! !
!GraphicConnection class categoriesFor: #from:to:!public! !

