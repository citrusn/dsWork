"Filed out from Dolphin Smalltalk 7"!

GraphicObjectManipulationTool subclass: #GraphicObjectPathCreationTool
	instanceVariableNames: 'path'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectPathCreationTool guid: (GUID fromString: '{a8bb9281-b2fe-11d5-b8e3-000021fb21b2}')!
GraphicObjectPathCreationTool comment: 'DrawingShell show'!
!GraphicObjectPathCreationTool categoriesForClass!Unclassified! !
!GraphicObjectPathCreationTool methodsFor!

cursor



	^Cursor cross!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	 self arena setSelectionTool.
!

onLeftButtonPressedBack: aMouseEvent


	self path isNil ifTrue:[ path:= GraphicClosedPolygon new.
					self arena model add: path.
					self arena addSelection: path.].

	self path addPoint: aMouseEvent position.!

path
	"Private - Answer the value of the receiver's ''path'' instance variable."

	^path!

path: anObject
	"Private - Set the value of the receiver's ''path'' instance variable to the argument, anObject."

	path := anObject! !
!GraphicObjectPathCreationTool categoriesFor: #cursor!public! !
!GraphicObjectPathCreationTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectPathCreationTool categoriesFor: #onLeftButtonPressedBack:!public! !
!GraphicObjectPathCreationTool categoriesFor: #path!accessing!private! !
!GraphicObjectPathCreationTool categoriesFor: #path:!accessing!private! !

