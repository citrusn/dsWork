"Filed out from Dolphin Smalltalk 7"!

GraphicObject subclass: #GraphicHandle
	instanceVariableNames: 'target arena positionAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicHandle guid: (GUID fromString: '{7e3e11a5-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicHandle comment: ''!
!GraphicHandle categoriesForClass!Unclassified! !
!GraphicHandle methodsFor!

alignmentSelector: anObject
	"Private - Set the value of the receiver's ''alignmentSelector'' instance variable to the argument, anObject."



	positionAction := MessageSend receiver: self target selector: anObject!

arena
	"Private - Answer the value of the receiver's ''arena'' instance variable."

	^arena!

arena: anObject
	"Private - Set the value of the receiver's ''arena'' instance variable to the argument, anObject."

	arena := anObject!

calculateBounds


	^(self positionAction value) + self offset -3 extent: 6!

drawOn: aCanvas

	aCanvas setDefaultAttributes.
	^aCanvas rectangle: self bounds

!

initialize


	self alignmentSelector: #topLeft	!

invalidate

"	super invalidate."
	self arena view invalidateRect: (self bounds expandedBy:1) erase: false!

offset


	^0!

onLeftButtonDoubleClicked: aMouseEvent 
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonDrag: aMouseEvent


	
!

onLeftButtonPressed: aMouseEvent

	"Some visual feedBack"
	arena view  canvas fillRectangle: self bounds   brush: Brush white.
	 (Delay forMilliseconds:100) wait. 
	self drawOn: arena view  canvas 

	!

positionAction
	"Private - Answer the value of the receiver's ''positionAction'' instance variable."

	^positionAction!

positionAction: anObject
	"Private - Set the value of the receiver's ''positionAction'' instance variable to the argument, anObject."

	positionAction := anObject!

target
	"Private - Answer the value of the receiver's ''target'' instance variable."

	^target!

target: anObject
	"Private - Set the value of the receiver's ''target'' instance variable to the argument, anObject."
	
	anObject  when: #invalidateRectangle: send: #invalidate to: self.
"	anObject  when: #positionChanged send: #invalidate to: self.
	anObject  when: #sizeChanged send: #invalidate to: self."
	target := anObject.
	! !
!GraphicHandle categoriesFor: #alignmentSelector:!accessing!public! !
!GraphicHandle categoriesFor: #arena!accessing!private! !
!GraphicHandle categoriesFor: #arena:!accessing!private! !
!GraphicHandle categoriesFor: #calculateBounds!public! !
!GraphicHandle categoriesFor: #drawOn:!public! !
!GraphicHandle categoriesFor: #initialize!public! !
!GraphicHandle categoriesFor: #invalidate!public! !
!GraphicHandle categoriesFor: #offset!public! !
!GraphicHandle categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!GraphicHandle categoriesFor: #onLeftButtonDrag:!public! !
!GraphicHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicHandle categoriesFor: #positionAction!public! !
!GraphicHandle categoriesFor: #positionAction:!public! !
!GraphicHandle categoriesFor: #target!accessing!private! !
!GraphicHandle categoriesFor: #target:!accessing!private! !

!GraphicHandle class methodsFor!

for: aGraphicObject arena:aGraphicView



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			 initialize


!

for: aGraphicObject arena:aGraphicView positionAction: anAction



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			positionAction: anAction
			 


! !
!GraphicHandle class categoriesFor: #for:arena:!public! !
!GraphicHandle class categoriesFor: #for:arena:positionAction:!public! !

