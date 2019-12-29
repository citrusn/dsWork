"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GraphicObjectManipulationTool
	instanceVariableNames: 'arena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectManipulationTool guid: (GUID fromString: '{a6c9d000-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectManipulationTool comment: ''!
!GraphicObjectManipulationTool categoriesForClass!Unclassified! !
!GraphicObjectManipulationTool methodsFor!

arena
	"Private - Answer the value of the receiver's ''arena'' instance variable."

	^arena!

arena: anObject
	"Private - Set the value of the receiver's ''arena'' instance variable to the argument, anObject."

	arena := anObject!

cursor



	^Cursor arrow!

onKeyPressed: aKeyEvent
	"Default handler for a key press event.
	Accept the default window processing."

!

onKeyReleased: aKeyEvent
	"Default handler for a key up event.
	Accept the default window processing."
!

onKeyTyped: aKeyEvent
	"Default handler for a keyboard event.
	Accept the default window processing."
!

onLeftButtonDoubleClicked: aMouseEvent hit: aGraphicObject
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	!

onLeftButtonPressedBack: aMouseEvent
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	!

onLeftButtonReleased: aMouseEvent
	"Default handler for a Left button up mouse event.
	Accept the default window processing."

!

onMiddleButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse middle button double-click event.
	Accept the default window processing."

!

onMiddleButtonPressed: aMouseEvent
	"Default handler for a middle button down mouse event.
	Accept the default window processing."

	!

onMiddleButtonReleased: aMouseEvent
	"Default handler for a Middle button up mouse event.
	Accept the default window processing."
!

onMouseMoved: aMouseEvent
	"Default handler for a mouse move event.
	Accept the default processing.
	Note: We don't trigger an event here to save churning."

	!

onMouseWheel: aMouseEvent
	"Default handler for a ouseWheel mouse event.
	Accept the default window processing."

!

onRightButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse Right button double-click event.
	Accept the default window processing."

!

onRightButtonPressed: aMouseEvent
	"Default handler for a Right button down mouse event.
	Accept the default window processing."

	!

onRightButtonReleased: aMouseEvent
	"Default handler for a Right button up mouse event.
	Accept the default window processing."

! !
!GraphicObjectManipulationTool categoriesFor: #arena!accessing!private! !
!GraphicObjectManipulationTool categoriesFor: #arena:!accessing!private! !
!GraphicObjectManipulationTool categoriesFor: #cursor!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyReleased:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyTyped:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonDoubleClicked:hit:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonPressed:hit:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonPressedBack:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonReleased:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonDoubleClicked:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonPressed:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonReleased:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMouseMoved:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onMouseWheel:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonDoubleClicked:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonPressed:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonReleased:!event handling!public! !

!GraphicObjectManipulationTool class methodsFor!

for: aGraphicView



	^self new arena: aGraphicView! !
!GraphicObjectManipulationTool class categoriesFor: #for:!public! !

