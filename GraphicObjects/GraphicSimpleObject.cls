"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObject subclass: #GraphicSimpleObject
	instanceVariableNames: 'ellipse text'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicSimpleObject guid: (GUID fromString: '{a3c67581-c1ed-11d5-b8e3-000021fb21b2}')!
GraphicSimpleObject comment: ''!
!GraphicSimpleObject categoriesForClass!Unclassified! !
!GraphicSimpleObject methodsFor!

defaultHandlesFor: aPresenter

	
	^ellipse defaultHandlesFor: aPresenter!

graphicAttributes


	^ellipse graphicAttributes!

metaObject: anObject


	text := GraphicText new text: anObject printString.
	ellipse := GraphicRectangle new extent: 40@40.
	ellipse when: #sizeChanged send: #onEllipseSizeChanged to: self.
	self add: text ; add: ellipse.


	!

onEllipseSizeChanged 

	self invalidate.
	text extent: ellipse extent.
	text center: ellipse center.
	self calculateBounds.
	self invalidate.

	! !
!GraphicSimpleObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicSimpleObject categoriesFor: #graphicAttributes!public! !
!GraphicSimpleObject categoriesFor: #metaObject:!public! !
!GraphicSimpleObject categoriesFor: #onEllipseSizeChanged!public! !

