"Filed out from Dolphin Smalltalk 7"!

BorderLayoutGraphicObject subclass: #GraphicSimpleClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicSimpleClass guid: (GUID fromString: '{c16bc922-e789-11d5-b8e3-9d717074350f}')!
GraphicSimpleClass comment: ''!
!GraphicSimpleClass categoriesForClass!Unclassified! !
!GraphicSimpleClass methodsFor!

metaObject: anObject

	self west: (GraphicImage image: anObject icon).
	self centerGraphic: (GraphicText text: anObject printString).
! !
!GraphicSimpleClass categoriesFor: #metaObject:!public! !

