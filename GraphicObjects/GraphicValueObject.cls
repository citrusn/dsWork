"Filed out from Dolphin Smalltalk 7"!

GraphicFixedBoundsObject subclass: #GraphicValueObject
	instanceVariableNames: 'valueModel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicValueObject guid: (GUID fromString: '{aacb6501-b164-11d5-b8e3-000021fb21b2}')!
GraphicValueObject comment: ''!
!GraphicValueObject categoriesForClass!Unclassified! !
!GraphicValueObject methodsFor!

initialize

	super initialize.
	valueModel := nil asValue!

valueModel
	"Private - Answer the value of the receiver's ''valueModel'' instance variable."

	^valueModel!

valueModel: anObject
	"Private - Set the value of the receiver's ''valueModel'' instance variable to the argument, anObject."

	valueModel when:#valueChanged send: #invalidate to: self.

	valueModel := anObject! !
!GraphicValueObject categoriesFor: #initialize!public! !
!GraphicValueObject categoriesFor: #valueModel!accessing!private! !
!GraphicValueObject categoriesFor: #valueModel:!accessing!private! !

