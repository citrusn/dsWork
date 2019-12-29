"Filed out from Dolphin Smalltalk 7"!

ManagedLayoutGraphicComposite subclass: #AlignmentGraphicObject
	instanceVariableNames: 'orientation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AlignmentGraphicObject guid: (GUID fromString: '{c68d3440-e6a6-11d5-b8e3-e0f30574409d}')!
AlignmentGraphicObject comment: 'DrawingShell show'!
!AlignmentGraphicObject categoriesForClass!Unclassified! !
!AlignmentGraphicObject methodsFor!

beHorizontal

	orientation := #horizontal.
	self reorganize
	!

beVertical

	orientation := #vertical.
	self reorganize
	!

bottomRight: aPoint

	^super bottomRight: (aPoint max: self calculateBounds corner) .

!

bounds


	^bounds!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	col addAll: (super defaultHandlesFor: arena).
	"self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)]."
	col add: ((GraphicLeftButtonHandle for: self arena: arena selector: #switchOrientation) 
			 alignmentSelector: #topCenter).
	^col!

initialize


super initialize.
	orientation := #horizontal.
	self when:#sizeChanged send: #reorganize  to: self.!

orientation
	"Private - Answer the value of the receiver's ''orientation'' instance variable."

	^orientation!

orientation: anObject
	"Private - Set the value of the receiver's ''orientation'' instance variable to the argument, anObject."

	orientation := anObject!

reorganize
	
	subObjects isEmpty ifTrue:[^self].
	orientation == #vertical ifTrue:[^self reorganizeVertical].
	self reorganizeHorizontal
	!

reorganizeHorizontal

	|delta margen centerY|
	margen :=   ( self extent x- self totalExtent x  / subObjects size ) floor .
	centerY := self center y.
	delta := self origin x + (self subObjects first extent x /2) rounded.
	self subObjects do:[:aGraphicObject | aGraphicObject basicCenter:   delta @ centerY.
								delta := delta + ( aGraphicObject extent x ) + margen].
	self invalidate.
	!

reorganizeVertical

	|delta margen centerX|
	margen :=   ( self extent y - self totalExtent y  / subObjects size ) floor .
	centerX := self center x.
	delta := self origin y + (self subObjects first extent y /2) rounded.

	self subObjects do:[:aGraphicObject | aGraphicObject basicCenter:    centerX @ delta.
								delta := delta + ( aGraphicObject extent y ) + margen].
	self invalidate.
	!

reorganizeVertical1

	|delta|
	delta := (subObjects inject: 0@0 into:[:suma : aGraphicObject | suma + aGraphicObject extent]) / subObjects size.
	self subObjects do:[:aGraphicObject | aGraphicObject basicPosition: self origin + delta.
								delta := delta + (0@aGraphicObject extent  y )].
	self calculateBounds.
	self invalidate.
	!

reorganizeVertical2

	|delta margen|
	margen :=  0@ ( self extent y - self totalExtent y / (subObjects size )  ) floor .
	
	delta := 0@0.
	self subObjects do:[:aGraphicObject | aGraphicObject basicPosition: self origin + delta.
								delta := delta + (0@ aGraphicObject extent y) + margen].
	self invalidate.
	!

switchOrientation

	orientation == #horizontal ifTrue:[ ^self beVertical].
	self beHorizontal.
	!

totalExtent


	^subObjects inject: 0@0 into:[:suma : aGraphicObject | suma + aGraphicObject extent].! !
!AlignmentGraphicObject categoriesFor: #beHorizontal!public! !
!AlignmentGraphicObject categoriesFor: #beVertical!public! !
!AlignmentGraphicObject categoriesFor: #bottomRight:!public! !
!AlignmentGraphicObject categoriesFor: #bounds!public! !
!AlignmentGraphicObject categoriesFor: #defaultHandlesFor:!public! !
!AlignmentGraphicObject categoriesFor: #initialize!public! !
!AlignmentGraphicObject categoriesFor: #orientation!accessing!private! !
!AlignmentGraphicObject categoriesFor: #orientation:!accessing!private! !
!AlignmentGraphicObject categoriesFor: #reorganize!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeHorizontal!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical1!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical2!public! !
!AlignmentGraphicObject categoriesFor: #switchOrientation!public! !
!AlignmentGraphicObject categoriesFor: #totalExtent!public! !

!AlignmentGraphicObject class methodsFor!

horizontal

	^self new orientation: #horizontal!

vertical

	^self new orientation: #vertical! !
!AlignmentGraphicObject class categoriesFor: #horizontal!public! !
!AlignmentGraphicObject class categoriesFor: #vertical!public! !

