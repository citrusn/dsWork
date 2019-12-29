"Filed out from Dolphin Smalltalk 7"!

GraphicFixedBoundsObject subclass: #GraphicCompositeObjectAbstract
	instanceVariableNames: 'subObjects openEditing'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObjectAbstract guid: (GUID fromString: '{c68d3441-e6a6-11d5-b8e3-e0f30574409d}')!
GraphicCompositeObjectAbstract comment: ''!
!GraphicCompositeObjectAbstract categoriesForClass!Unclassified! !
!GraphicCompositeObjectAbstract methodsFor!

add: aGraphicObject



	self subObjects add: aGraphicObject.
	aGraphicObject parentGraphic: self.
	self trigger: #graphicAdded: with: aGraphicObject.
	bounds := self calculateBounds.

	^aGraphicObject!

addAll: aCollection



	self subObjects addAll: aCollection.
	aCollection do:[:aGraphicObject | 		aGraphicObject parentGraphic: self.
								self trigger: #graphicAdded: with: aGraphicObject.].
	bounds := self calculateBounds.

	^aCollection!

basicPosition: aPoint

	|delta|
	delta :=  aPoint - self position .
	self subObjects do:[:aGraphObj | aGraphObj basicPosition: aGraphObj position + delta].
	"self calculateBounds." "podria ser "super basicPosition:aPoint.
!

basicTranslateBy: aPoint



	super basicTranslateBy: aPoint.
	self subObjects do:[:aGraphObj | aGraphObj basicTranslateBy: aPoint].
!

bringToFront: aGraphicObject


	aGraphicObject == self subObjects first ifTrue:[^self].
	subObjects addFirst: (subObjects remove: aGraphicObject).
	self invalidate.
^aGraphicObject	!

calculateBounds


	subObjects isEmpty ifTrue:[^bounds].
	^self subObjects inject: self subObjects  first bounds into:[:max :aGraphicObject | 
		max merge: aGraphicObject bounds]!

decompose

	^self parentGraphic addAll: self subObjects ; remove: self!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	openEditing ifTrue: [self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)].].
	col addAll: (super defaultHandlesFor: arena).


	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent | arena model addAll: self subObjects; remove: self] ; 	alignmentSelector: #bottomCenter).
	
	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent | self switchEditing] ; alignmentSelector: #rightCenter).
	^col!

drawOn: aCanvas


"	aCanvas  selectClipRegion: (Region rectangle: (self bounds))."
	self subObjects  reverseDo:[:aGraphicObject |aGraphicObject drawOn: aCanvas].
"	aCanvas  selectClipRegion: nil"!

graphicObjectAt: aPoint


	^self subObjects detect:[:aGraphicObject | aGraphicObject containsPoint: aPoint] ifNone:[nil].!

initialize

	super initialize.
	subObjects := OrderedCollection new.
	openEditing := false.!

position: aPoint

	|delta|
	self invalidate.
	delta :=  aPoint - self position .
	
	self subObjects do:[:aGraphObj | aGraphObj basicPosition: aGraphObj position + delta].
	"self calculateBounds." "podria ser "super position:aPoint.
	self invalidate.!

remove: oldElement


		^self remove: oldElement ifAbsent: [self errorNotFound: oldElement]
!

remove: aGraphicObject ifAbsent: aBlock


	self invalidate.
	self subObjects remove: aGraphicObject ifAbsent: aBlock.
	aGraphicObject parentGraphic: nil.
	self trigger: #graphicRemoved: with: aGraphicObject.
	self calculateBounds.

	^aGraphicObject!

removeAll


	|ans|
	ans := subObjects.
	subObjects := OrderedCollection new.
	^ans!

sendToBack: aGraphicObject


	aGraphicObject == self subObjects last ifTrue:[^self].
	(subObjects addLast: (subObjects remove: aGraphicObject) ) invalidate.
	self invalidate.

	^aGraphicObject	!

subObjects
	"Private - Answer the value of the receiver's ''subObjects'' instance variable."

	^subObjects!

subObjects: anObject
	"Private - Set the value of the receiver's ''subObjects'' instance variable to the argument, anObject."

	subObjects := anObject!

switchEditing

	openEditing := openEditing not! !
!GraphicCompositeObjectAbstract categoriesFor: #add:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #addAll:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #basicPosition:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #basicTranslateBy:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #bringToFront:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #calculateBounds!public! !
!GraphicCompositeObjectAbstract categoriesFor: #decompose!public! !
!GraphicCompositeObjectAbstract categoriesFor: #defaultHandlesFor:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #drawOn:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #graphicObjectAt:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #initialize!public! !
!GraphicCompositeObjectAbstract categoriesFor: #position:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #remove:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #remove:ifAbsent:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #removeAll!public! !
!GraphicCompositeObjectAbstract categoriesFor: #sendToBack:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #subObjects!accessing!private! !
!GraphicCompositeObjectAbstract categoriesFor: #subObjects:!accessing!private! !
!GraphicCompositeObjectAbstract categoriesFor: #switchEditing!public! !

