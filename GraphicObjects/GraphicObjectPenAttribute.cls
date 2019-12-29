"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GraphicObjectPenAttribute
	instanceVariableNames: 'borderColor borderWidth borderStyle cachedPen oldCanvasState'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectPenAttribute guid: (GUID fromString: '{6e46bf3e-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectPenAttribute comment: ''!
!GraphicObjectPenAttribute categoriesForClass!Unclassified! !
!GraphicObjectPenAttribute methodsFor!

borderColor
	"Private - Answer the value of the receiver's ''borderColor'' instance variable."

	^borderColor!

borderColor: anObject
	"Private - Set the value of the receiver's ''borderColor'' instance variable to the argument, anObject."

	self invalidatePen.
	borderColor := anObject!

borderStyle
	"Private - Answer the value of the receiver's ''borderStyle'' instance variable."

	^borderStyle!

borderStyle: anObject
	"Private - Set the value of the receiver's ''borderStyle'' instance variable to the argument, anObject."
	self invalidatePen.
	borderStyle := anObject!

borderWidth
	"Private - Answer the value of the receiver's ''borderWidth'' instance variable."

	^borderWidth!

borderWidth: anObject
	"Private - Set the value of the receiver's ''borderWidth'' instance variable to the argument, anObject."

	self invalidatePen.
	borderWidth := anObject!

cachedPen
	"Private - Answer the value of the receiver's ''cachedPen'' instance variable."

	^cachedPen!

cachedPen: anObject
	"Private - Set the value of the receiver's ''cachedPen'' instance variable to the argument, anObject."

	cachedPen := anObject!

installOn: aCanvas


	aCanvas pen: self pen!

invalidatePen


	self cachedPen: nil!

oldCanvasState
	"Private - Answer the value of the receiver's ''oldCanvasState'' instance variable."

	^oldCanvasState!

oldCanvasState: anObject
	"Private - Set the value of the receiver's ''oldCanvasState'' instance variable to the argument, anObject."

	oldCanvasState := anObject!

pen
	""

	cachedPen isNil ifTrue:[ self cachedPen: (Pen withStyle: self borderStyle width: self borderWidth color: self borderColor )].

	^cachedPen!

presenterClass


	^GraphicObjectPenAttributePresenter!

presentIn: aCompositePresenter

	|presenter|
	presenter := self presenterClass createIn: aCompositePresenter on: self.
	aCompositePresenter view extent: 0@20 + presenter view extent .
	presenter view position: 0@0.
! !
!GraphicObjectPenAttribute categoriesFor: #borderColor!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderColor:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderStyle!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderStyle:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderWidth!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderWidth:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #cachedPen!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #cachedPen:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #installOn:!public! !
!GraphicObjectPenAttribute categoriesFor: #invalidatePen!public! !
!GraphicObjectPenAttribute categoriesFor: #oldCanvasState!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #oldCanvasState:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #pen!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #presenterClass!public! !
!GraphicObjectPenAttribute categoriesFor: #presentIn:!public! !

!GraphicObjectPenAttribute class methodsFor!

default


	^self new borderColor: Color black; borderWidth: 1 ; borderStyle: (Win32Constants at: 'PS_SOLID').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect color: #borderColor);
		add: (Aspect integer: #borderWidth).
	^aspects.! !
!GraphicObjectPenAttribute class categoriesFor: #default!public! !
!GraphicObjectPenAttribute class categoriesFor: #publishedAspectsOfInstances!public! !

