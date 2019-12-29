"Filed out from Dolphin Smalltalk 7"!

ManagedLayoutGraphicComposite subclass: #ProportionalLayoutGraphicObject
	instanceVariableNames: 'proportions horizontal'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProportionalLayoutGraphicObject guid: (GUID fromString: '{c16bc921-e789-11d5-b8e3-9d717074350f}')!
ProportionalLayoutGraphicObject comment: 'DrawingShell show'!
!ProportionalLayoutGraphicObject categoriesForClass!Unclassified! !
!ProportionalLayoutGraphicObject methodsFor!

add: anObject 


	^self   add: anObject proportion: 1
!

add: anObject proportion: aProportion

	self proportions at: anObject put: aProportion.
	super  add: anObject .

^anObject!

addAll: aCollection


	aCollection do:[:aGraphicObject | self add: aGraphicObject].
	^aCollection!

bounds: aRectangle

	super bounds: aRectangle.
	self reorganize!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	col addAll: (super defaultHandlesFor: arena).
	"self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)]."
	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent |self switchOrientation] ; alignmentSelector: #topCenter).
	^col!

initialize

	super initialize.
	proportions := LookupTable new.
	horizontal := false.!

majorDimensionPoint: aNumber


	horizontal  ifTrue:[ ^aNumber @ 1].
	^1@ aNumber !

nextOriginSelector

	horizontal  ifTrue:[ ^#topRight].
	^#bottomLeft!

proportionFor: aGraphicObject

	^self majorDimensionPoint: (proportions at: aGraphicObject) !

proportions
	"Private - Answer the value of the receiver's ''proportions'' instance variable."

	^proportions!

proportions: anObject
	"Private - Set the value of the receiver's ''proportions'' instance variable to the argument, anObject."

	proportions := anObject!

remove: aGraphicObject ifAbsent: aBlock


	super remove: aGraphicObject ifAbsent: aBlock.
	proportions removeKey: aGraphicObject.

	^aGraphicObject!

reorganize
	|origin total|
	total := self totalProportions.
	origin := self origin.	
	self subObjects do:[:aGraphicObject |
		aGraphicObject 
			basicBounds: (origin extent:( (self extent * (self proportionFor: aGraphicObject)) /(self majorDimensionPoint: total) ) floor).
		origin := aGraphicObject perform: self nextOriginSelector.
		 ].
	self invalidate.!

switchOrientation


	horizontal := horizontal not.
	self reorganize!

totalProportions


	^proportions inject:0 into:[:sum :aNumber | sum + aNumber].
! !
!ProportionalLayoutGraphicObject categoriesFor: #add:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #add:proportion:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #addAll:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #bounds:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #defaultHandlesFor:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #initialize!public! !
!ProportionalLayoutGraphicObject categoriesFor: #majorDimensionPoint:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #nextOriginSelector!public! !
!ProportionalLayoutGraphicObject categoriesFor: #proportionFor:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #proportions!accessing!private! !
!ProportionalLayoutGraphicObject categoriesFor: #proportions:!accessing!private! !
!ProportionalLayoutGraphicObject categoriesFor: #remove:ifAbsent:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #reorganize!public! !
!ProportionalLayoutGraphicObject categoriesFor: #switchOrientation!public! !
!ProportionalLayoutGraphicObject categoriesFor: #totalProportions!public! !

!ProportionalLayoutGraphicObject class methodsFor!

example1
	
	"DrawingShell show model add: self example1"
	^self new add: BorderLayoutGraphicObject example1 proportion: 2 ; 
			add: BorderLayoutGraphicObject example1 proportion: 1 ;
			extent: 200@200;
			yourself


! !
!ProportionalLayoutGraphicObject class categoriesFor: #example1!public! !

