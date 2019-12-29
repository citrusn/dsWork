"Filed out from Dolphin Smalltalk 7"!

GraphicValueObject subclass: #GraphicText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicText guid: (GUID fromString: '{09070681-af1f-11d5-b8e3-000021fb21b2}')!
GraphicText comment: ''!
!GraphicText categoriesForClass!Unclassified! !
!GraphicText methodsFor!

angle

	^0!

drawOn: aCanvas

	graphicAttributes installOn: aCanvas.
	aCanvas setBkMode: 1.
	^aCanvas formatText: self text in: self bounds align: #center 


!

font

	^graphicAttributes font!

font:arg1 

	^graphicAttributes font:arg1 !

text
	"Private - Answer the value of the receiver's ''text'' instance variable."

	^valueModel value!

text: anObject
	"Private - Set the value of the receiver's ''text'' instance variable to the argument, anObject."

	valueModel value: anObject!

textColor

	^graphicAttributes textColor!

textColor:arg1 

	^graphicAttributes textColor:arg1 ! !
!GraphicText categoriesFor: #angle!public! !
!GraphicText categoriesFor: #drawOn:!public! !
!GraphicText categoriesFor: #font!public! !
!GraphicText categoriesFor: #font:!public! !
!GraphicText categoriesFor: #text!accessing!private! !
!GraphicText categoriesFor: #text:!accessing!private! !
!GraphicText categoriesFor: #textColor!public! !
!GraphicText categoriesFor: #textColor:!public! !

!GraphicText class methodsFor!

defaultGraphicAttributes


	^GraphicObjectTextAttributes default!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect string: #text).
		

	^aspects.!

test


	^self text: 'Hola que tal'!

text: aString


	^self new text: aString! !
!GraphicText class categoriesFor: #defaultGraphicAttributes!public! !
!GraphicText class categoriesFor: #publishedAspectsOfInstances!constants!development!public! !
!GraphicText class categoriesFor: #test!public! !
!GraphicText class categoriesFor: #text:!public! !

