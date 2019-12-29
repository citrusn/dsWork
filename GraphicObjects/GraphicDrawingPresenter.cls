"Filed out from Dolphin Smalltalk 7"!

Presenter subclass: #GraphicDrawingPresenter
	instanceVariableNames: 'selection handles tool'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicDrawingPresenter guid: (GUID fromString: '{a6c1aac1-b5d1-11d5-b8e3-000021fb21b2}')!
GraphicDrawingPresenter comment: 'DrawingShell show'!
!GraphicDrawingPresenter categoriesForClass!Unclassified! !
!GraphicDrawingPresenter methodsFor!

addHandles: aCollection


	aCollection do:[:aGraphicObject | self addHandleTo: aGraphicObject ].!

addHandleTo: aGraphicObject



	(self handles addAll: (aGraphicObject defaultHandlesFor: self)) do:[:aHandle | aHandle invalidate].
	
	self view invalidateRect: aGraphicObject bounds!

addSelection: aGraphic


	selection add: aGraphic.
	self addHandleTo: aGraphic.
	self trigger: #selectionChanged!

alignSelection

	|group|
	self hasSelection ifFalse:[^self].
	group := ProportionalLayoutGraphicObject new.
	group addAll: (self removeSelection).
	self model add: group.
	


	!

basicSelection: aCollection

	self removeAllHandles.
	self addHandles: aCollection.
	selection := aCollection.
	self trigger: #selectionChanged.!

bringToFrontSelection


	self selection do:[:aGraphic| self model bringToFront: aGraphic].!

connectSelection

	( self selection copyFrom: 1 to: selection size -1 )do:[:aGraphicObject| 
			(self model add: 
				(GraphicConnection2 
					from: aGraphicObject 
					to:(self selection after: aGraphicObject ))) bringToFront].
	(self model add: 
		(GraphicConnection2 
			from: 	self selection last
			to:(self selection first))) bringToFront.!

cursor


	^self tool cursor


	!

groupSelection

	|group|
	self hasSelection ifFalse:[^self].
	group := GraphicCompositeObject new.
	group addAll: (self removeSelection).
	self model add: group.
	


	!

handleAtPoint: aPoint


^self handles detect:[:graphObject | graphObject containsPoint: aPoint] ifNone:[nil].!

handles
	"Private - Answer the value of the receiver's ''handles'' instance variable."

	^handles!

handles: anObject
	"Private - Set the value of the receiver's ''handles'' instance variable to the argument, anObject."

	handles := anObject!

hasSelection


	^selection notEmpty!

initialize

	super initialize.
	handles := OrderedCollection new.
	tool := GraphicObjectSelectionTool for: self.
	selection := OrderedCollection new.!

isSelected: aGraphic


	^selection includes: aGraphic!

onKeyPressed: aKeyEvent

	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	
	tool onKeyPressed: aKeyEvent.
	^super onKeyPressed: aKeyEvent.
!

onKeyTyped: aKeyEvent

	"Default handler for a mouse left button double-click event.
	Accept the default window processing."


	tool onKeyPressed: aKeyEvent.
	^super onKeyPressed: aKeyEvent.
!

onLeftButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	|hit hitGraph hitHandle|
	super onLeftButtonDoubleClicked: aMouseEvent.
	hit := aMouseEvent position.
	
	hitHandle := self handleAtPoint: hit.
	hitHandle == nil  ifFalse:[  ^hitHandle onLeftButtonDoubleClicked: aMouseEvent ].

	hitGraph := self model graphicObjectAt: hit.
	hitGraph == nil  ifFalse:[^self  tool onLeftButtonDoubleClicked: aMouseEvent hit: hitGraph ].
	tool onLeftButtonDoubleClickedBack: aMouseEvent.
!

onLeftButtonPressed: aMouseEvent

	|hit hitGraph hitHandle|

	hit := aMouseEvent position.
	
	hitHandle := self handleAtPoint: hit.
	hitHandle == nil  ifFalse:[ (self view dragDetect: aMouseEvent) ifTrue:[ ^hitHandle onLeftButtonDrag: aMouseEvent]
										ifFalse:[ ^hitHandle onLeftButtonPressed: aMouseEvent] ].

	hitGraph := self model graphicObjectAt: hit.
	hitGraph == nil  ifFalse:[^self  tool onLeftButtonPressed: aMouseEvent hit: hitGraph ].
	tool onLeftButtonPressedBack: aMouseEvent .
	^super  onLeftButtonPressed: aMouseEvent.!

removeAllHandles

	handles do:[:i | self view  invalidateRect: i bounds. 
				i setEvents: nil].
	handles := OrderedCollection new.!

removeHandle: aHandle



	self handles remove: aHandle ifAbsent:[].
	aHandle invalidate.!

removeHandleFrom: aGraphicObject



	(self handles select:[:aHandle | aHandle target == aGraphicObject ]) do:[:aHandle | self removeHandle: aHandle ].
	
	self view invalidateRect: aGraphicObject bounds!

removeSelection

	|answer|
	selection isNil ifTrue:[^self].
	answer := self selection copy.
	selection do:[:aGraphicObject |  aGraphicObject delete].
	self removeAllHandles.
	^answer.!

removeSelection: aGraphic


	selection remove: aGraphic.
	self removeHandleFrom: aGraphic.
	self trigger: #selectionChanged.!

selectAll


	^self selection: model subObjects copy!

selection


^selection!

selection: aCollection


	self basicSelection: aCollection!

sendToBackSelection


	self selection do:[:aGraphic| self model sendToBack: aGraphic].!

setClosedPathTool


	^self tool: GraphicObjectPathCreationTool new!

setEllipseTool


	^self tool: GraphicObjectCreationTool ellipse!

setLineTool


	^self tool: GraphicObjectCreationTool line!

setOpenPathTool


	^self tool: GraphicObjectPathCreationTool new!

setPathTool


	^self tool: GraphicObjectPathCreationTool new!

setRectangleTool


	^self tool: (GraphicObjectCreationTool new newObjectPrototype: GraphicRectangle new).!

setSelectionTool


	^self tool: GraphicObjectSelectionTool new!

setTextTool


	^self tool: GraphicObjectCreationTool text!

showTree


	TreePresenter showOn: (GraphicObjectTree withRoots: self selection).!

tool
	"Private - Answer the value of the receiver's ''tool'' instance variable."

	^tool!

tool: anObject
	"Private - Set the value of the receiver's ''tool'' instance variable to the argument, anObject."

	tool := anObject.
	tool arena: self.
	self view setCursor: tool cursor.! !
!GraphicDrawingPresenter categoriesFor: #addHandles:!public! !
!GraphicDrawingPresenter categoriesFor: #addHandleTo:!public! !
!GraphicDrawingPresenter categoriesFor: #addSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #alignSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #basicSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #bringToFrontSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #connectSelection!public! !
!GraphicDrawingPresenter categoriesFor: #cursor!public! !
!GraphicDrawingPresenter categoriesFor: #groupSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #handleAtPoint:!public! !
!GraphicDrawingPresenter categoriesFor: #handles!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #handles:!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #hasSelection!public! !
!GraphicDrawingPresenter categoriesFor: #initialize!initializing!public! !
!GraphicDrawingPresenter categoriesFor: #isSelected:!public! !
!GraphicDrawingPresenter categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onKeyTyped:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onLeftButtonPressed:!public! !
!GraphicDrawingPresenter categoriesFor: #removeAllHandles!public! !
!GraphicDrawingPresenter categoriesFor: #removeHandle:!public! !
!GraphicDrawingPresenter categoriesFor: #removeHandleFrom:!public! !
!GraphicDrawingPresenter categoriesFor: #removeSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #removeSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #selectAll!commands!public! !
!GraphicDrawingPresenter categoriesFor: #selection!public! !
!GraphicDrawingPresenter categoriesFor: #selection:!public! !
!GraphicDrawingPresenter categoriesFor: #sendToBackSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setClosedPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setEllipseTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setLineTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setOpenPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setRectangleTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setSelectionTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setTextTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #showTree!public! !
!GraphicDrawingPresenter categoriesFor: #tool!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #tool:!accessing!private! !

!GraphicDrawingPresenter class methodsFor!

defaultModel


	^GraphicDrawing new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.GraphicObjectsView) 34 12 nil nil 34 2 8 1141047296 1 416 nil 196934 1 ##(Smalltalk.RGB) 8519169 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 8 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #setSelectionTool 8 'Selection' 1 1 nil nil nil 562 1 594 #groupSelection 8 'Group' 1 1 nil nil nil 562 1 594 #alignSelection 8 'Group with Alignment' 1 1 nil nil nil 514 nil true 34 5 562 1 594 #setRectangleTool 8 'Rectangle' 1 1 nil nil nil 562 1 594 #setClosedPathTool 8 'Closed Path' 1 1 nil nil nil 562 1 594 #setOpenPathTool 8 'Open Path' 1 1 nil nil nil 562 1 594 #setTextTool 8 'Text' 1 1 nil nil nil 562 1 594 #setEllipseTool 8 'Ellipse' 1 1 nil nil nil 8 'New' nil 1 nil nil nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 562 1 594 #showTree 8 'Show Tree' 1 1 nil nil nil 1026 4097 562 1 594 #connectSelection 8 'Connect' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 416 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 11 11 1298 225 201 416 1250 #contextMenu: 34 1 528 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 117 0 0 0 105 0 0 0] 8 #() 1298 193 193 nil 27 )! !
!GraphicDrawingPresenter class categoriesFor: #defaultModel!public! !
!GraphicDrawingPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

