"Filed out from Dolphin Smalltalk 7"!

GraphicObjectPenAttributePresenter subclass: #GraphicObjectBrushAttributePresenter
	instanceVariableNames: 'fillStyle fillColor fillHatch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectBrushAttributePresenter guid: (GUID fromString: '{446c68c2-c11f-11d5-b8e3-000021fb21b2}')!
GraphicObjectBrushAttributePresenter comment: ''!
!GraphicObjectBrushAttributePresenter categoriesForClass!Unclassified! !
!GraphicObjectBrushAttributePresenter methodsFor!

createComponents

	 super createComponents.
	fillStyle := self add: ChoicePresenter new name:'fillStyle' .
	fillColor := self add: EditableColorPresenter new name:'fillColor' .
	fillHatch := self add: ChoicePresenter new name:'fillHatch' .
!

model: anObject 

	 super model: anObject .
	fillStyle model: (self model aspectValue: #fillStyle) .
	fillColor model: (self model aspectValue: #fillColor) .
	fillHatch model: (self model aspectValue: #fillHatch) .
! !
!GraphicObjectBrushAttributePresenter categoriesFor: #createComponents!public! !
!GraphicObjectBrushAttributePresenter categoriesFor: #model:!public! !

