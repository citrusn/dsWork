"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObject subclass: #GraphicCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCollection guid: (GUID fromString: '{a3c67583-c1ed-11d5-b8e3-000021fb21b2}')!
GraphicCollection comment: ''!
!GraphicCollection categoriesForClass!Unclassified! !
!GraphicCollection methodsFor!

metaObject: aCollection

	|col grap|
	super metaObject: aCollection.
	col := self add: (GraphicSimpleObject new metaObject: aCollection).
	aCollection asArray keysAndValuesDo:[:index :object | 

					grap := (subObjects add: (GraphicSimpleObject new metaObject: object) )position: index * 10 @ 40.
					subObjects add: (GraphicConnection from: col to:grap)].
self calculateBounds.
	! !
!GraphicCollection categoriesFor: #metaObject:!public! !

