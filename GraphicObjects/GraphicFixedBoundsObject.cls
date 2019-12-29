"Filed out from Dolphin Smalltalk 7"!

GraphicObject subclass: #GraphicFixedBoundsObject
	instanceVariableNames: 'bounds'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject guid: (GUID fromString: '{aacb6500-b164-11d5-b8e3-000021fb21b2}')!
GraphicFixedBoundsObject comment: ''!
!GraphicFixedBoundsObject categoriesForClass!Unclassified! !
!GraphicFixedBoundsObject methodsFor!

basicBounds: anObject


	bounds:= anObject!

basicCenter: aPoint


	self basicTranslateBy: aPoint - self center!

basicPosition: aPoint

	

	bounds moveTo: aPoint.
!

basicTranslateBy: delta


		bounds moveBy: delta.!

bottomLeft: aPoint


	self invalidate.
	self bounds bottomLeft: aPoint.
	self trigger: #sizeChanged.
"	self invalidate."!

bottomRight: aPoint


	self invalidate.
	self bounds bottomRight: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

bounds
	"Private - Answer the value of the receiver's ''bounds'' instance variable."

	^bounds!

bounds: anObject
	"Private - Set the value of the receiver's ''bounds'' instance variable to the argument, anObject."

	self invalidate.
	bounds := anObject!

calculateBounds


	^bounds!

center: aPoint


	self invalidate.
	self basicCenter: aPoint.
	self trigger: #positionChanged.
	self invalidate.!

corner: aPoint


	self invalidate.
	self basicTranslateBy: aPoint - self corner.
	self trigger: #positionChanged.
	self invalidate.!

defaultHandlesFor: aGraphicView



	^(super defaultHandlesFor: aGraphicView) addAll:(self resizeHandlesFor: aGraphicView) ; yourself!

extent: aPoint


	self invalidate.
	self bounds extent: aPoint.
	self bounds: self bounds normalize.
	self trigger: #sizeChanged.
	self invalidate.!

fillColor

	^graphicAttributes fillColor!

fillColor:arg1 

	^graphicAttributes fillColor:arg1 !

fillHatch

	^graphicAttributes fillHatch!

fillHatch:arg1 

	^graphicAttributes fillHatch:arg1 !

fillStyle

	^graphicAttributes fillStyle!

fillStyle:arg1 

	^graphicAttributes fillStyle:arg1 !

initialize


	super initialize.
	bounds := Rectangle origin: 10@10 extent: 20.!

origin: aPoint


	self invalidate.
	self bounds origin: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

position: aPoint

	
"	self invalidate."
	bounds moveTo: aPoint.
	self trigger: #positionChanged.
	self invalidate.!

resizeHandlesFor: aGraphicView


	^#(bottomRight topLeft topRight bottomLeft) collect:[:aSymbol | 
		(GraphicAspectHandle for: self arena:aGraphicView ) adaptor: (self  aspectValue:aSymbol).].


!

topLeft: aPoint


	self invalidate.
	self bounds topLeft: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

topRight: aPoint


	self invalidate.
	self bounds topRight: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

translateBy: delta

	self invalidate.
	self basicTranslateBy: delta.
	self trigger: #positionChanged.
	self invalidate.! !
!GraphicFixedBoundsObject categoriesFor: #basicBounds:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicCenter:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicPosition:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicTranslateBy:!public! !
!GraphicFixedBoundsObject categoriesFor: #bottomLeft:!public! !
!GraphicFixedBoundsObject categoriesFor: #bottomRight:!public! !
!GraphicFixedBoundsObject categoriesFor: #bounds!accessing!private! !
!GraphicFixedBoundsObject categoriesFor: #bounds:!accessing!private! !
!GraphicFixedBoundsObject categoriesFor: #calculateBounds!public! !
!GraphicFixedBoundsObject categoriesFor: #center:!public! !
!GraphicFixedBoundsObject categoriesFor: #corner:!public! !
!GraphicFixedBoundsObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicFixedBoundsObject categoriesFor: #extent:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillColor!public! !
!GraphicFixedBoundsObject categoriesFor: #fillColor:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillHatch!public! !
!GraphicFixedBoundsObject categoriesFor: #fillHatch:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillStyle!public! !
!GraphicFixedBoundsObject categoriesFor: #fillStyle:!public! !
!GraphicFixedBoundsObject categoriesFor: #initialize!public! !
!GraphicFixedBoundsObject categoriesFor: #origin:!public! !
!GraphicFixedBoundsObject categoriesFor: #position:!public! !
!GraphicFixedBoundsObject categoriesFor: #resizeHandlesFor:!public! !
!GraphicFixedBoundsObject categoriesFor: #topLeft:!public! !
!GraphicFixedBoundsObject categoriesFor: #topRight:!public! !
!GraphicFixedBoundsObject categoriesFor: #translateBy:!public! !

!GraphicFixedBoundsObject class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicFixedBoundsObject class categoriesFor: #defaultGraphicAttributes!public! !

