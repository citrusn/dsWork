"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObjectAbstract subclass: #ManagedLayoutGraphicComposite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ManagedLayoutGraphicComposite guid: (GUID fromString: '{9fc61f81-e727-11d5-b8e3-9162307a4e65}')!
ManagedLayoutGraphicComposite comment: ''!
!ManagedLayoutGraphicComposite categoriesForClass!Unclassified! !
!ManagedLayoutGraphicComposite methodsFor!

add: aGraphicObject

	super add: aGraphicObject.
	self reorganize.

	^aGraphicObject!

addAll: aCollection

	super addAll: aCollection.
	self reorganize.

	^aCollection!

basicBounds: aRectangle

	super bounds: aRectangle.
	self reorganize!

bottomRight: aPoint

	super bottomRight: aPoint.
	self reorganize.
!

bounds: aRect

	super bounds: aRect.
	self reorganize.

	!

remove: aGraphicObject ifAbsent: aBlock



	super remove: aGraphicObject ifAbsent: aBlock.
	self reorganize.

	^aGraphicObject!

reorganize! !
!ManagedLayoutGraphicComposite categoriesFor: #add:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #addAll:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #basicBounds:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #bottomRight:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #bounds:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #remove:ifAbsent:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #reorganize!public! !

