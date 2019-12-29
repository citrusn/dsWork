"Filed out from Dolphin Smalltalk 7"!

GraphicFixedBoundsObject subclass: #GraphicRectangle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicRectangle guid: (GUID fromString: '{6e46bf3d-b61b-11d5-b8e3-000021fb21b2}')!
GraphicRectangle comment: ''!
!GraphicRectangle categoriesForClass!Unclassified! !
!GraphicRectangle methodsFor!

drawOn: aCanvas


	graphicAttributes installOn: aCanvas.
	aCanvas rectangle: (self bounds insetBy: graphicAttributes borderWidth -1).

! !
!GraphicRectangle categoriesFor: #drawOn:!public! !

!GraphicRectangle class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicRectangle class categoriesFor: #defaultGraphicAttributes!public! !

