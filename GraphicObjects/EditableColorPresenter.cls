"Filed out from Dolphin Smalltalk 7"!

ColorPresenter subclass: #EditableColorPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EditableColorPresenter guid: (GUID fromString: '{446c68c3-c11f-11d5-b8e3-000021fb21b2}')!
EditableColorPresenter comment: ''!
!EditableColorPresenter categoriesForClass!Unclassified! !
!EditableColorPresenter methodsFor!

onLeftButtonDoubleClicked: aMouseEvent

	
	super onLeftButtonDoubleClicked: aMouseEvent.
	ColorDialog showModalOn: self model.! !
!EditableColorPresenter categoriesFor: #onLeftButtonDoubleClicked:!public! !

