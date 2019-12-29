"Filed out from Dolphin Smalltalk 7"!

GraphicObjectTrackingTool subclass: #GraphicObjectCreationTool
	instanceVariableNames: 'newObjectPrototype'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectCreationTool guid: (GUID fromString: '{a6c9d002-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectCreationTool comment: ''!
!GraphicObjectCreationTool categoriesForClass!Unclassified! !
!GraphicObjectCreationTool methodsFor!

cursor



	^Cursor arrow!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|selectionRect|

	Cursor wait showWhile:[
		(self arena model add: 
				(self newObjectPrototype clone  
					bounds: (Rectangle vertex: tracker origin vertex: aPoint))
		) invalidate ; bringToFront.
 ].
	arena  view drawFocusRect: (tracker origin corner: aPoint )normalize.
!

newObjectPrototype
	"Private - Answer the value of the receiver's ''newObjectPrototype'' instance variable."

	^newObjectPrototype!

newObjectPrototype: anObject
	"Private - Set the value of the receiver's ''newObjectPrototype'' instance variable to the argument, anObject."

	newObjectPrototype := anObject!

onButtonPressed: aMouseEvent


	Cursor wait showWhile:[(self arena model add: self newObjectPrototype clone ) position: aMouseEvent position ]!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

self onLeftButtonPressedBack: aMouseEvent.! !
!GraphicObjectCreationTool categoriesFor: #cursor!public! !
!GraphicObjectCreationTool categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicObjectCreationTool categoriesFor: #newObjectPrototype!public! !
!GraphicObjectCreationTool categoriesFor: #newObjectPrototype:!public! !
!GraphicObjectCreationTool categoriesFor: #onButtonPressed:!public! !
!GraphicObjectCreationTool categoriesFor: #onLeftButtonPressed:hit:!public! !

!GraphicObjectCreationTool class methodsFor!

ellipse


	^self new newObjectPrototype: GraphicEllipse new!

line


	^self new newObjectPrototype: GraphicLine new!

text


	^self new newObjectPrototype: GraphicText test! !
!GraphicObjectCreationTool class categoriesFor: #ellipse!public! !
!GraphicObjectCreationTool class categoriesFor: #line!public! !
!GraphicObjectCreationTool class categoriesFor: #text!public! !

