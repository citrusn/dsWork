"Filed out from Dolphin Smalltalk 7"!

InstVarGraphic subclass: #IndexedInstVarGraphic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IndexedInstVarGraphic guid: (GUID fromString: '{9fc61f82-e727-11d5-b8e3-9162307a4e65}')!
IndexedInstVarGraphic comment: ''!
!IndexedInstVarGraphic categoriesForClass!Unclassified! !
!IndexedInstVarGraphic methodsFor!

representedObject


	^metaObject basicAt: index!

text 


	^(index displayString), ' :', (metaObject basicAt: index) printString! !
!IndexedInstVarGraphic categoriesFor: #representedObject!public! !
!IndexedInstVarGraphic categoriesFor: #text!public! !

