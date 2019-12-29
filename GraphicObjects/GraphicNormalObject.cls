"Filed out from Dolphin Smalltalk 7"!

BorderLayoutGraphicObject subclass: #GraphicNormalObject
	instanceVariableNames: 'instVars'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicNormalObject guid: (GUID fromString: '{d4ace4a1-c2b4-11d5-b8e3-000021fb21b2}')!
GraphicNormalObject comment: 'DrawingShell show model add: (self new metaObject: Object)'!
!GraphicNormalObject categoriesForClass!Unclassified! !
!GraphicNormalObject methodsFor!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	instVars subObjects do:[:i|  col addAll: ( i defaultHandlesFor: arena) ].
	col addAll: (super defaultHandlesFor: arena).
	^col!

metaObject: anObject


	self north: (GraphicText text: anObject basicPrintString).
	instVars := AlignmentGraphicObject  vertical .
	1 to: anObject class instSize do:[:i | 
		instVars add: (InstVarGraphic object: anObject index: i).
		].
	anObject isIndexable  ifTrue:[ 1 to: (anObject basicSize min:7)  do:[:i | 
		instVars add: (IndexedInstVarGraphic object: anObject index: i).
		].].
	self extent: instVars calculateBounds extent + self north extent.
	self centerGraphic: (instVars).


	self reorganize.
! !
!GraphicNormalObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicNormalObject categoriesFor: #metaObject:!public! !

