"Filed out from Dolphin Smalltalk 7"!

View subclass: #GraphicObjectsView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectsView guid: (GUID fromString: '{7e3e11a4-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicObjectsView comment: '	|obj|

gr := (Shell show:''testGraph'' ) view subViews first .

	obj := Dictionary allInstances first.
	gr  addGraphicObject:  (GraphicText text: obj printString).
	obj do:[:i | gr  addGraphicObject:  (GraphicText text: i printString)].11'!
!GraphicObjectsView categoriesForClass!Unclassified! !
!GraphicObjectsView methodsFor!

connectModel


"	self model when:#invalidate send: #invalidateGraphicObjectRect:  to: self with: self model;
			when:#graphicAdded:  send: #onGraphicAdded:   to: self;
			when:#graphicRemoved:  send: #onGraphicRemoved:   to: self.
			when:#invalidateRectangle:  send: #invalidateRectangle:  to: self .
"

!

cursor


	^self tool cursor


	!

dragDetect: aMouseEvent
	"Answer whether the <MouseEvent> parameter (from a button down event) is the first event
	in a drag action."

	^aMouseEvent button == #left
		ifTrue: [UserLibrary default dragDetect: self asParameter pt: aMouseEvent screenPosition asParameter]
		ifFalse: [
			#todo "Implement right drag detection, oddly DragDetect() only works for the left button.".
			false]!

invalidateGraphicObjectRect: aGraphicObject


"	self invalidateRect: aGraphicObject bounds normalize."!

invalidateRectangle: aRectangle


	^self invalidateRect: aRectangle normalize erase: false!

layoutExtent


	self model isNil ifTrue:[ ^super layoutExtent].
	^self model extent.!

onGraphicAdded: aGraphicObject
	"	aGraphicObject when:#invalidateRectangle: send: #invalidateRectangle: to: self"

	!

onGraphicRemoved: aGraphicObject
	"	self invalidateGraphicObjectRect: aGraphicObject .
	aGraphicObject removeEventsTriggeredFor: self."

	!

onPaintRequired: aPaintEvent


	|canvas dib dibCanvas|
	canvas := aPaintEvent canvas.
	dib := Bitmap compatible: canvas extent: canvas extent.
	dibCanvas := dib canvas.
	dibCanvas backcolor: self backcolor.
	dibCanvas erase.
	self model isNil ifFalse:[self model drawOn: dibCanvas.].
	presenter == self ifFalse:[presenter handles reverseDo:[ :aHandle | aHandle drawOn: dibCanvas].].
	dib drawOn: canvas at: 0@0 extent: canvas extent .
	dib free.
	dibCanvas free.


!

scale

	^1.1!

selection: aCollection


	self basicSelection: aCollection!

tool


	presenter == self ifTrue:[^nil].
	^presenter tool! !
!GraphicObjectsView categoriesFor: #connectModel!public! !
!GraphicObjectsView categoriesFor: #cursor!public! !
!GraphicObjectsView categoriesFor: #dragDetect:!helpers!private! !
!GraphicObjectsView categoriesFor: #invalidateGraphicObjectRect:!public! !
!GraphicObjectsView categoriesFor: #invalidateRectangle:!public! !
!GraphicObjectsView categoriesFor: #layoutExtent!public! !
!GraphicObjectsView categoriesFor: #onGraphicAdded:!public! !
!GraphicObjectsView categoriesFor: #onGraphicRemoved:!public! !
!GraphicObjectsView categoriesFor: #onPaintRequired:!public! !
!GraphicObjectsView categoriesFor: #scale!public! !
!GraphicObjectsView categoriesFor: #selection:!public! !
!GraphicObjectsView categoriesFor: #tool!public! !

!GraphicObjectsView class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.GraphicObjectsView) 34 12 nil nil 34 2 8 1140850688 1 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 416 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 642 225 201 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 112 0 0 0 100 0 0 0] 8 #() 642 193 193 nil 27 )! !
!GraphicObjectsView class categoriesFor: #resource_Default_view!public!resources-views! !

