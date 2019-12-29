"Filed out from Dolphin Smalltalk 7"!

Object subclass: #DelegatorMethodBuilder
	instanceVariableNames: 'targetClass targetVar selectors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DelegatorMethodBuilder guid: (GUID fromString: '{6e46bf41-b61b-11d5-b8e3-000021fb21b2}')!
DelegatorMethodBuilder comment: ''!
!DelegatorMethodBuilder categoriesForClass!Unclassified! !
!DelegatorMethodBuilder methodsFor!

generateMethods

	|stream|
	(self selectors "reject:[:aSymbol | 	targetClass respondsTo: aSymbol ]")
				do:[:aSymbol | 
	stream := String writeStream.
	self writeHeaderOn: stream symbol: aSymbol.
	stream cr;
		   cr;
		   tab;
			nextPut: $^; 
		   nextPutAll: targetVar displayString;
		space.
			self writeHeaderOn:stream symbol: aSymbol.
	targetClass compile: stream contents.
	]
		!

selectors
	"Private - Answer the value of the receiver's ''selectors'' instance variable."

	^selectors!

selectors: anObject
	"Private - Set the value of the receiver's ''selectors'' instance variable to the argument, anObject."

	selectors := anObject!

targetClass
	"Private - Answer the value of the receiver's ''targetClass'' instance variable."

	^targetClass!

targetClass: anObject
	"Private - Set the value of the receiver's ''targetClass'' instance variable to the argument, anObject."

	targetClass := anObject!

targetVar
	"Private - Answer the value of the receiver's ''targetVar'' instance variable."

	^targetVar!

targetVar: anObject
	"Private - Set the value of the receiver's ''targetVar'' instance variable to the argument, anObject."

	targetVar := anObject!

writeHeaderOn:stream symbol: aSymbol


	|readStream count|
	readStream := aSymbol asString readStream.
	count := 0.
	[readStream atEnd] 
		whileFalse:[
			readStream peek = $: ifTrue:[ stream nextPut: readStream next;
									         nextPutAll: 'arg';
										display: (count := count +1);
								    space.]
							 ifFalse:[stream nextPut: readStream next.].
				].
		
	! !
!DelegatorMethodBuilder categoriesFor: #generateMethods!public! !
!DelegatorMethodBuilder categoriesFor: #selectors!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #selectors:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetClass!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetClass:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetVar!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetVar:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #writeHeaderOn:symbol:!public! !

!DelegatorMethodBuilder class methodsFor!

class: aClass target: aString


	!

class: aClass target: aString selectors: aCollection


	^self new targetClass: aClass;
			targetVar: aString;
			selectors: aCollection;
			generateMethods.!

class: aClass target: aString targetClass: aTarget

	|aCollection|
	aCollection := ChoicePrompter multipleChoices: aTarget selectors asArray.
	aCollection isNil ifTrue:[^self].

	^self new targetClass: aClass;
			targetVar: aString;
			selectors: aCollection;
			generateMethods.! !
!DelegatorMethodBuilder class categoriesFor: #class:target:!public! !
!DelegatorMethodBuilder class categoriesFor: #class:target:selectors:!public! !
!DelegatorMethodBuilder class categoriesFor: #class:target:targetClass:!public! !

