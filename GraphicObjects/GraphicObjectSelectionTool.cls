"Filed out from Dolphin Smalltalk 7"!

GraphicObjectTrackingTool subclass: #GraphicObjectSelectionTool
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectSelectionTool guid: (GUID fromString: '{a6c9d001-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectSelectionTool comment: ''!
!GraphicObjectSelectionTool categoriesForClass!Unclassified! !
!GraphicObjectSelectionTool methodsFor!

cursor



	^Cursor cross!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|selectionRect|
	arena  view drawFocusRect: (Rectangle vertex: tracker origin vertex: aPoint) asParameter.
	selectionRect := (tracker origin corner: aPoint )normalize.
	arena basicSelection: ( arena model subObjects select:[:aGraphObject| selectionRect contains: aGraphObject bounds]).!

onKeyPressed: aKeyEvent
	"Default handler for a key press event.
	Accept the default window processing."

	
	aKeyEvent code = 46 "VK_DELETE" ifTrue:[ self arena removeSelection].!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	^self arena selectAll!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	aMouseEvent isShiftDown ifTrue:[ ^(self arena isSelected: aGraphicObject ) ifTrue:[ self arena removeSelection: aGraphicObject] 
												 ifFalse:[self arena addSelection: aGraphicObject] ].
	arena basicSelection: (OrderedCollection  with: aGraphicObject)! !
!GraphicObjectSelectionTool categoriesFor: #cursor!public! !
!GraphicObjectSelectionTool categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicObjectSelectionTool categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicObjectSelectionTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectSelectionTool categoriesFor: #onLeftButtonPressed:hit:!event handling!public! !

