"Filed out from Dolphin Smalltalk 7"!

GraphicFixedBoundsObject subclass: #GraphicLine
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicLine guid: (GUID fromString: '{7e3e11a3-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicLine comment: ''!
!GraphicLine categoriesForClass!Unclassified! !
!GraphicLine methodsFor!

containsPoint: aPoint



	(super containsPoint: aPoint )ifFalse:[^false].

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: self bounds origin to: self bounds corner) )<= 3!

drawOn: aCanvas

		graphicAttributes installOn: aCanvas.
	^aCanvas moveTo: self bounds origin; 
			lineTo: self bounds corner


! !
!GraphicLine categoriesFor: #containsPoint:!public! !
!GraphicLine categoriesFor: #drawOn:!public! !

!GraphicLine class methodsFor!

from: origin
to: corner

	^self new bounds:  (origin corner: corner)! !
!GraphicLine class categoriesFor: #from:to:!public! !

