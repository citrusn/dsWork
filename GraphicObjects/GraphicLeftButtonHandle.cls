"Filed out from Dolphin Smalltalk 7"!

GraphicHandle subclass: #GraphicLeftButtonHandle
	instanceVariableNames: 'action'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicLeftButtonHandle guid: (GUID fromString: '{07047ec0-b67d-11d5-b8e3-000021fb21b2}')!
GraphicLeftButtonHandle comment: ''!
!GraphicLeftButtonHandle categoriesForClass!Unclassified! !
!GraphicLeftButtonHandle methodsFor!

action
	"Private - Answer the value of the receiver's ''action'' instance variable."

	^action!

action: anObject
	"Private - Set the value of the receiver's ''action'' instance variable to the argument, anObject."

	action := anObject!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	^action value: aMouseEvent 

	
! !
!GraphicLeftButtonHandle categoriesFor: #action!accessing!private! !
!GraphicLeftButtonHandle categoriesFor: #action:!accessing!private! !
!GraphicLeftButtonHandle categoriesFor: #onLeftButtonPressed:!public! !

!GraphicLeftButtonHandle class methodsFor!

for: aGraphicObject arena:aGraphicView



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			 initialize


!

for: aGraphicObject arena:aGraphicView action: aMonadicValuable



	^(self for: aGraphicObject arena:aGraphicView ) action: aMonadicValuable


!

for: aGraphicObject arena:aGraphicView selector: aSymbol



	^(self for: aGraphicObject arena:aGraphicView ) action: (MessageSend receiver: aGraphicObject selector: aSymbol )


! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:!public! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:action:!public! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:selector:!public! !

