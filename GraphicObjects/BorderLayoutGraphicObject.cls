"Filed out from Dolphin Smalltalk 7"!

ManagedLayoutGraphicComposite subclass: #BorderLayoutGraphicObject
	instanceVariableNames: 'north south centerGraphic east west'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BorderLayoutGraphicObject guid: (GUID fromString: '{9fc61f80-e727-11d5-b8e3-9162307a4e65}')!
BorderLayoutGraphicObject comment: ''!
!BorderLayoutGraphicObject categoriesForClass!Unclassified! !
!BorderLayoutGraphicObject methodsFor!

centerGraphic
	"Private - Answer the value of the receiver's ''centerGraphic'' instance variable."

	^centerGraphic!

centerGraphic: anObject
	"Private - Set the value of the receiver's ''centerGraphic'' instance variable to the argument, anObject."

	centerGraphic notNil ifTrue:[ self remove: centerGraphic ifAbsent:[]].
	centerGraphic := anObject.
	self add: anObject!

east
	"Private - Answer the value of the receiver's ''east'' instance variable."

	^east!

east: anObject
	"Private - Set the value of the receiver's ''east'' instance variable to the argument, anObject."

	east notNil ifTrue:[ self remove: east ifAbsent:[] ].
	east := anObject.
	self add: anObject!

hasCenter

	^self centerGraphic notNil !

hasEast

	^self east notNil !

hasNorth

	^self north notNil !

hasSouth

	^self south notNil !

hasWest

	^self west notNil !

north
	"Private - Answer the value of the receiver's ''north'' instance variable."

	^north!

north: anObject
	"Private - Set the value of the receiver's ''north'' instance variable to the argument, anObject."

	north notNil ifTrue:[ self remove: north ifAbsent:[] ].
	north := anObject.
	self add: anObject!

reorganize
	"Performs a layout operation on the contents in aContainerView"

	| top bottom left right d insets |
	insets := 0@0 corner: 0@0.
	top := self top .
	bottom := self bottom.
	left := self left.
	right := self  right.

	self hasNorth ifTrue: [
		self north extent: (right - left) @ self north height.
		d := self north extent.
		self north bounds: (left@top extent: right - left @ d y).
		top := top + d y ].
	self hasSouth ifTrue: [
		self south extent: right - left @ self south height.
		d := self south extent.
		self south bounds: (left @ (bottom - d y) extent: right - left @ d y).
		bottom := bottom - d y ].
	self hasEast ifTrue: [
		self east extent: self east width @ (bottom - top).
		d := self east extent.
		self east bounds: ((right - d x)@top extent: d x @ (bottom - top)).
		right := right - d x ].
	self hasWest ifTrue: [
		self west extent: self west width @ (bottom - top).
		d := self west extent.
		self west bounds: (left@top extent: d x @ (bottom - top)).
		left := left + d x ].
	self hasCenter ifTrue: [
		self centerGraphic bounds: (left @ top extent: right - left @ (bottom - top))].!

south
	"Private - Answer the value of the receiver's ''south'' instance variable."

	^south!

south: anObject
	"Private - Set the value of the receiver's ''south'' instance variable to the argument, anObject."

	south notNil ifTrue:[ self remove: south ifAbsent:[] ].
	south := anObject.
	self add: anObject!

west
	"Private - Answer the value of the receiver's ''west'' instance variable."

	^west!

west: anObject
	"Private - Set the value of the receiver's ''west'' instance variable to the argument, anObject."

	west notNil ifTrue:[ self remove: west ifAbsent:[] ].
	west := anObject.
	self add: anObject! !
!BorderLayoutGraphicObject categoriesFor: #centerGraphic!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #centerGraphic:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #east!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #east:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #hasCenter!public! !
!BorderLayoutGraphicObject categoriesFor: #hasEast!public! !
!BorderLayoutGraphicObject categoriesFor: #hasNorth!public! !
!BorderLayoutGraphicObject categoriesFor: #hasSouth!public! !
!BorderLayoutGraphicObject categoriesFor: #hasWest!public! !
!BorderLayoutGraphicObject categoriesFor: #north!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #north:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #reorganize!geometry!public! !
!BorderLayoutGraphicObject categoriesFor: #south!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #south:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #west!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #west:!accessing!private! !

!BorderLayoutGraphicObject class methodsFor!

example1
	
	"DrawingShell show model add: self example1"
	^self new centerGraphic: (GraphicEllipse new);	
			north: (GraphicRectangle new);
			east: (GraphicRectangle new);
			 west: (GraphicRectangle new);
			south: (GraphicEllipse new).

!

example2
	
	"DrawingShell show model add: self example1"
	^self new centerGraphic: (GraphicEllipse new);	
			 west: (GraphicRectangle new).


!

example3
	
	"DrawingShell show model add: self example3"
	^self new centerGraphic: self example1;	
			 north: self example2;
			bottomRight: 200@200.


! !
!BorderLayoutGraphicObject class categoriesFor: #example1!public! !
!BorderLayoutGraphicObject class categoriesFor: #example2!public! !
!BorderLayoutGraphicObject class categoriesFor: #example3!public! !

