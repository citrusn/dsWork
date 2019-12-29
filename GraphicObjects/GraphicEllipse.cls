"Filed out from Dolphin Smalltalk 7"!

GraphicFixedBoundsObject subclass: #GraphicEllipse
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicEllipse guid: (GUID fromString: '{7e3e11a2-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicEllipse comment: ''!
!GraphicEllipse categoriesForClass!Unclassified! !
!GraphicEllipse methodsFor!

containsPoint: aPoint

	"Robado de Squeak>>EllipseMorph>>containsPoint: "

	| radius other delta xOverY |
	(bounds containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	(bounds width = 1 or: [bounds height = 1])
		ifTrue: [^ true].  "Degenerate case -- code below fails by a bit"

	radius :=  bounds height asFloat / 2.
	other :=  bounds width asFloat / 2.
	delta :=  aPoint - bounds topLeft - (other@radius).
	xOverY :=  bounds width asFloat / bounds height asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared!

drawOn: aCanvas


	super drawOn: aCanvas.
	aCanvas ellipse: self bounds.

!

initialize

	super initialize.
	! !
!GraphicEllipse categoriesFor: #containsPoint:!public! !
!GraphicEllipse categoriesFor: #drawOn:!public! !
!GraphicEllipse categoriesFor: #initialize!public! !

!GraphicEllipse class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicEllipse class categoriesFor: #defaultGraphicAttributes!public! !

