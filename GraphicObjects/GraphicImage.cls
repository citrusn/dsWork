"Filed out from Dolphin Smalltalk 7"!

GraphicValueObject subclass: #GraphicImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicImage guid: (GUID fromString: '{6536a8e1-b09e-11d5-b8e3-000021fb21b2}')!
GraphicImage comment: ''!
!GraphicImage categoriesForClass!Unclassified! !
!GraphicImage methodsFor!

drawOn: aCanvas


	^self image drawOn: aCanvas at: self bounds origin extent: self bounds extent


!

image
	"Private - Answer the value of the receiver's ''image'' instance variable."

	^valueModel value!

image: anObject
	"Private - Set the value of the receiver's ''image'' instance variable to the argument, anObject."

	valueModel value: anObject! !
!GraphicImage categoriesFor: #drawOn:!public! !
!GraphicImage categoriesFor: #image!accessing!private! !
!GraphicImage categoriesFor: #image:!accessing!private! !

!GraphicImage class methodsFor!

image: anImage

	
	^self new image: anImage!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect image: #image).

	^aspects.!

test


	^self image: Object icon.! !
!GraphicImage class categoriesFor: #image:!public! !
!GraphicImage class categoriesFor: #publishedAspectsOfInstances!constants!development!public! !
!GraphicImage class categoriesFor: #test!public! !

