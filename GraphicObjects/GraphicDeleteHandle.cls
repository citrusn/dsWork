"Filed out from Dolphin Smalltalk 7"!

GraphicHandle subclass: #GraphicDeleteHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicDeleteHandle guid: (GUID fromString: '{6536a8e2-b09e-11d5-b8e3-000021fb21b2}')!
GraphicDeleteHandle comment: ''!
!GraphicDeleteHandle categoriesForClass!Unclassified! !
!GraphicDeleteHandle methodsFor!

drawOn: aCanvas


	^aCanvas moveTo: self bounds origin ; 
			lineTo: self bounds corner; 
			moveTo: self bounds bottomLeft; 
			lineTo: self bounds topRight.

!

offset



	^##(-10@ 20)!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	self target delete.
	self target invalidate.
	arena removeAllHandles.

	
!

targetOriginOffset



	^##(-10@ 20)! !
!GraphicDeleteHandle categoriesFor: #drawOn:!public! !
!GraphicDeleteHandle categoriesFor: #offset!public! !
!GraphicDeleteHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicDeleteHandle categoriesFor: #targetOriginOffset!public! !

