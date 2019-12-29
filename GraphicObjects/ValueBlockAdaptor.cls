"Filed out from Dolphin Smalltalk 7"!

ValueAdaptor subclass: #ValueBlockAdaptor
	instanceVariableNames: 'getValueBlock setValueBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValueBlockAdaptor guid: (GUID fromString: '{6e46bf3b-b61b-11d5-b8e3-000021fb21b2}')!
ValueBlockAdaptor comment: ''!
!ValueBlockAdaptor categoriesForClass!Unclassified! !
!ValueBlockAdaptor methodsFor!

setValue: newValue
	"Set the value of the receiver to be the <Object> newValue
	without notifying dependents of the change."

	^setValueBlock value: subject value: newValue!

value
	"Answer the <Object> value of the receiver."

	^getValueBlock value: subject! !
!ValueBlockAdaptor categoriesFor: #setValue:!accessing!public! !
!ValueBlockAdaptor categoriesFor: #value!accessing!public! !

!ValueBlockAdaptor class methodsFor!

subject: anObject getter: getBlock setter: setBlock


	^(self subject: anObject )getValueBlock: getBlock;
					   setValueBlock: setBlock ;
						yourself.! !
!ValueBlockAdaptor class categoriesFor: #subject:getter:setter:!public! !

