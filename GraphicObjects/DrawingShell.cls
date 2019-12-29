"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #DrawingShell
	instanceVariableNames: 'drawingPresenter attributesWindow'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DrawingShell guid: (GUID fromString: '{a6c1aac2-b5d1-11d5-b8e3-000021fb21b2}')!
DrawingShell comment: ''!
!DrawingShell categoriesForClass!Unclassified! !
!DrawingShell methodsFor!

attributesWindow


	"(attributesWindow isNil or:[	attributesWindow view isOpen not]) ifTrue:[attributesWindow := (Shell showAsToolboxFor: self ).
												attributesWindow caption: 'Propiedades'].
	
	^attributesWindow"
	!

createComponents


	super createComponents.
	drawingPresenter := self add: GraphicDrawingPresenter new name:'drawing'.!

createSchematicWiring


	super createSchematicWiring.
	drawingPresenter when: #selectionChanged send: #onDrawingSelectionChanged to: self!

model: aDrawing


	super model: aDrawing.
	drawingPresenter  model: self model.

!

onDrawingSelectionChanged


"	drawingPresenter selection isEmpty ifTrue:[^self].
	self attributesWindow subPresenters isEmpty ifFalse:[ self attributesWindow remove: self attributesWindow subPresenters first].
	drawingPresenter selection first graphicAttributes presentIn: self attributesWindow ."!

showProperties

	drawingPresenter hasSelection ifFalse:[^self].

	^PublishedAspectInspector shellOn: drawingPresenter selection first.! !
!DrawingShell categoriesFor: #attributesWindow!public! !
!DrawingShell categoriesFor: #createComponents!public! !
!DrawingShell categoriesFor: #createSchematicWiring!public! !
!DrawingShell categoriesFor: #model:!public! !
!DrawingShell categoriesFor: #onDrawingSelectionChanged!public! !
!DrawingShell categoriesFor: #showProperties!public! !

!DrawingShell class methodsFor!

defaultModel


	^GraphicDrawing new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140853548 131137 544 nil 466 496 nil 517 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 32 198 66 49 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1] 328198 ##(Smalltalk.Point) 193 193 nil 544 466 496 8 4294904939 170 192 8 #() 170 192 784 138 144 784 170 176 784 nil 1 nil 706 33 33 706 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 706 1 1 706 707 51 544 978 #updateSizePosted 784 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 97 1 0 0 25 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 544 34 2 8 1140853548 131137 1136 nil 466 496 nil 517 nil 626 nil true 658 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 32 198 66 49 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1] 706 193 193 nil 1136 466 496 8 4294904939 170 192 784 170 192 34 6 59371 853766 ##(Smalltalk.ToolbarButton) 59371 nil 1136 1 1180998 4 ##(Smalltalk.CommandDescription) #bringToFrontSelection 8 'Move First' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 706 1857 33 35 59373 1362 59373 nil 1136 1 1394 #sendToBackSelection 8 'Move Last' 1 1 nil 1456 37 59375 1246982 ##(Smalltalk.ToolbarSystemButton) 59375 nil 1136 1 1394 #removeSelection 8 'Delete Item' 1 1 nil 1 11 34 3 1376 1584 1648 170 176 34 4 1 117 1456 1 nil 1 nil 706 33 33 706 45 45 nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 706 1 1 706 139 51 1136 978 #updateSizePosted 784 1136 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 69 0 0 0 25 0 0 0] 8 #() 706 193 193 nil 27 1952 nil 27 nil nil nil 410 ##(Smalltalk.ScrollingDecorator) 34 18 nil 416 34 2 8 1144061952 131073 1968 nil 466 496 nil 5 nil nil nil 1968 1573190 1 ##(Smalltalk.ScrollingDecoratorLayout) true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 1968 34 2 8 1140916224 131073 2112 nil nil nil 5 nil nil nil 2112 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.GraphicDrawingPresenter) #resource_Default_view nil 914 138 144 34 1 978 #createAt:extent: 34 2 706 1 1 706 907 351 2112 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 197 1 0 0 175 0 0 0] 784 1952 nil 27 8 'drawing' nil 706 1 1 true 706 17 17 914 138 144 34 1 978 #createAt:extent: 34 2 706 1 51 706 707 351 1968 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 97 1 0 0 200 0 0 0] 34 1 2112 1952 nil 27 170 192 784 nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1394 #showProperties 8 'properties' 1 1 nil nil nil 8 'Object' nil 1 nil nil 59369 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 706 101 101 706 2881 1513 416 978 #updateMenuBar 784 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 50 0 0 0 50 0 0 0 210 5 0 0 38 3 0 0] 34 2 544 1968 1952 nil 27 )! !
!DrawingShell class categoriesFor: #defaultModel!public! !
!DrawingShell class categoriesFor: #resource_Default_view!public!resources-views! !

