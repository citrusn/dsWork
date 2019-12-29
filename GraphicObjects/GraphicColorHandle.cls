"Filed out from Dolphin Smalltalk 7"!

GraphicHandle subclass: #GraphicColorHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicColorHandle guid: (GUID fromString: '{e6d9dc00-b35f-11d5-b8e3-000021fb21b2}')!
GraphicColorHandle comment: ''!
!GraphicColorHandle categoriesForClass!Unclassified! !
!GraphicColorHandle methodsFor!

drawOn: aCanvas


	aCanvas brush: Brush gray ; rectangle: self bounds
!

initialize

	super initialize.
	 self alignmentSelector:#topRight.
	!

offset



	^##(10@-10)!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	ColorDialog showModalOn: (self target aspectValue: #borderColor). 
	self target invalidate.


	
! !
!GraphicColorHandle categoriesFor: #drawOn:!public! !
!GraphicColorHandle categoriesFor: #initialize!public! !
!GraphicColorHandle categoriesFor: #offset!public! !
!GraphicColorHandle categoriesFor: #onLeftButtonPressed:!public! !

