"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObject subclass: #GraphicDrawing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicDrawing guid: (GUID fromString: '{a6c1aac0-b5d1-11d5-b8e3-000021fb21b2}')!
GraphicDrawing comment: ''!
!GraphicDrawing categoriesForClass!Unclassified! !
!GraphicDrawing methodsFor!

invalidateRectangle: aRectangleOrNil


	^self trigger: #invalidateRectangle: with: aRectangleOrNil!

metaObject: aCollection


	self createWith: aCollection! !
!GraphicDrawing categoriesFor: #invalidateRectangle:!public! !
!GraphicDrawing categoriesFor: #metaObject:!public! !

