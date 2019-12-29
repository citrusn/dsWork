"Filed out from Dolphin Smalltalk 7"!

GraphicObjectPenAttribute subclass: #GraphicObjectBrushAttribute
	instanceVariableNames: 'fillStyle fillColor fillHatch cachedBrush'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectBrushAttribute guid: (GUID fromString: '{6e46bf3f-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectBrushAttribute comment: ''!
!GraphicObjectBrushAttribute categoriesForClass!Unclassified! !
!GraphicObjectBrushAttribute methodsFor!

brush
	""

	cachedBrush isNil ifTrue:[ self cachedBrush: (Brush  color: fillColor )].

	^cachedBrush!

cachedBrush
	"Private - Answer the value of the receiver's ''cachedBrush'' instance variable."

	^cachedBrush!

cachedBrush: anObject
	"Private - Set the value of the receiver's ''cachedBrush'' instance variable to the argument, anObject."


	cachedBrush := anObject!

fillColor
	"Private - Answer the value of the receiver's ''fillColor'' instance variable."

	^fillColor!

fillColor: anObject
	"Private - Set the value of the receiver's ''fillColor'' instance variable to the argument, anObject."
	self invalidateBrush.
	fillColor := anObject!

fillHatch
	"Private - Answer the value of the receiver's ''fillHatch'' instance variable."

	^fillHatch!

fillHatch: anObject
	"Private - Set the value of the receiver's ''fillHatch'' instance variable to the argument, anObject."
	self invalidateBrush.

	fillHatch := anObject!

fillStyle
	"Private - Answer the value of the receiver's ''fillStyle'' instance variable."

	^fillStyle!

fillStyle: anObject
	"Private - Set the value of the receiver's ''fillStyle'' instance variable to the argument, anObject."
	self invalidateBrush.

	fillStyle := anObject!

installOn: aCanvas


	super installOn: aCanvas.
	aCanvas brush: self brush!

invalidateBrush


	self cachedBrush: nil!

presenterClass


	^GraphicObjectBrushAttributePresenter! !
!GraphicObjectBrushAttribute categoriesFor: #brush!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #cachedBrush!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #cachedBrush:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillColor!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillColor:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillHatch!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillHatch:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillStyle!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillStyle:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #installOn:!public! !
!GraphicObjectBrushAttribute categoriesFor: #invalidateBrush!public! !
!GraphicObjectBrushAttribute categoriesFor: #presenterClass!public! !

!GraphicObjectBrushAttribute class methodsFor!

default


	^super  default fillColor: Color none ; fillHatch: 1 ; fillStyle: (Win32Constants at: 'BS_SOLID').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect color: #fillColor).
	
	^aspects.! !
!GraphicObjectBrushAttribute class categoriesFor: #default!public! !
!GraphicObjectBrushAttribute class categoriesFor: #publishedAspectsOfInstances!public! !

