"Filed out from Dolphin Smalltalk 7"!

Presenter subclass: #GraphicObjectPenAttributePresenter
	instanceVariableNames: 'borderColor borderWidth borderStyle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectPenAttributePresenter guid: (GUID fromString: '{446c68c1-c11f-11d5-b8e3-000021fb21b2}')!
GraphicObjectPenAttributePresenter comment: ''!
!GraphicObjectPenAttributePresenter categoriesForClass!Unclassified! !
!GraphicObjectPenAttributePresenter methodsFor!

createComponents

	 super createComponents.
	borderColor := self add: EditableColorPresenter new name:'borderColor' .
	borderWidth := self add: NumberPresenter new name:'borderWidth' .
	borderStyle := self add: ChoicePresenter new name:'borderStyle' .
!

model: anObject 

	 super model: anObject .
	borderColor model: (self model aspectValue: #borderColor) .
	borderWidth model: (self model aspectValue: #borderWidth) .
	borderStyle model: (self model aspectValue: #borderStyle) .
! !
!GraphicObjectPenAttributePresenter categoriesFor: #createComponents!public! !
!GraphicObjectPenAttributePresenter categoriesFor: #model:!public! !

