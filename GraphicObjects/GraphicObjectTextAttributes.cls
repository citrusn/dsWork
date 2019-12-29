"Filed out from Dolphin Smalltalk 7"!

GraphicObjectBrushAttribute subclass: #GraphicObjectTextAttributes
	instanceVariableNames: 'font textAlign textColor angle fontName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectTextAttributes guid: (GUID fromString: '{6e46bf40-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectTextAttributes comment: ''!
!GraphicObjectTextAttributes categoriesForClass!Unclassified! !
!GraphicObjectTextAttributes methodsFor!

angle

	^font  angle!

angle:arg1 

	^font  angle:arg1 !

font
	"Private - Answer the value of the receiver's ''font'' instance variable."

	^font!

font: anObject
	"Private - Set the value of the receiver's ''font'' instance variable to the argument, anObject."

	font := anObject!

installOn: aCanvas


	super installOn: aCanvas.
	aCanvas setTextColor: self textColor ;
			font: self font.!

isBold

	^font  isBold!

isBold:arg1 

	^font  isBold:arg1 !

isItalic

	^font  isItalic!

isItalic:arg1 

	^font  isItalic:arg1 !

isStruckThrough

	^font  isStruckThrough!

isStruckThrough:arg1 

	^font  isStruckThrough:arg1 !

isUnderlined

	^font  isUnderlined!

isUnderlined:arg1 

	^font  isUnderlined:arg1 !

name

	^font  name!

name:arg1 

	^font  name:arg1 !

pixelSize

	^font  pixelSize!

pointSize

	^font  pointSize!

pointSize:arg1 

	^font  pointSize:arg1 !

textAlign
	"Private - Answer the value of the receiver's ''textAlign'' instance variable."

	^textAlign!

textAlign: anObject
	"Private - Set the value of the receiver's ''textAlign'' instance variable to the argument, anObject."

	textAlign := anObject!

textColor
	"Private - Answer the value of the receiver's ''textColor'' instance variable."

	^textColor!

textColor: anObject
	"Private - Set the value of the receiver's ''textColor'' instance variable to the argument, anObject."

	textColor := anObject! !
!GraphicObjectTextAttributes categoriesFor: #angle!public! !
!GraphicObjectTextAttributes categoriesFor: #angle:!public! !
!GraphicObjectTextAttributes categoriesFor: #font!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #font:!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #installOn:!public! !
!GraphicObjectTextAttributes categoriesFor: #isBold!public! !
!GraphicObjectTextAttributes categoriesFor: #isBold:!public! !
!GraphicObjectTextAttributes categoriesFor: #isItalic!public! !
!GraphicObjectTextAttributes categoriesFor: #isItalic:!public! !
!GraphicObjectTextAttributes categoriesFor: #isStruckThrough!public! !
!GraphicObjectTextAttributes categoriesFor: #isStruckThrough:!public! !
!GraphicObjectTextAttributes categoriesFor: #isUnderlined!public! !
!GraphicObjectTextAttributes categoriesFor: #isUnderlined:!public! !
!GraphicObjectTextAttributes categoriesFor: #name!public! !
!GraphicObjectTextAttributes categoriesFor: #name:!public! !
!GraphicObjectTextAttributes categoriesFor: #pixelSize!public! !
!GraphicObjectTextAttributes categoriesFor: #pointSize!public! !
!GraphicObjectTextAttributes categoriesFor: #pointSize:!public! !
!GraphicObjectTextAttributes categoriesFor: #textAlign!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textAlign:!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textColor!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textColor:!accessing!private! !

!GraphicObjectTextAttributes class methodsFor!

default


	^super  default textColor: Color black ; font: (Font name: 'Arial' pointSize: 8 )  ; textAlign: (Win32Constants at: 'DT_CENTER').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
	
		add: (Aspect font: #font);
		add: (Aspect color: #textColor);
		add: (Aspect integer: #angle).

	^aspects.! !
!GraphicObjectTextAttributes class categoriesFor: #default!public! !
!GraphicObjectTextAttributes class categoriesFor: #publishedAspectsOfInstances!public! !

