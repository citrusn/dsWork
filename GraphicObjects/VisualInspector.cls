"Filed out from Dolphin Smalltalk 7"!

Presenter subclass: #VisualInspector
	instanceVariableNames: 'graphics objects'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VisualInspector guid: (GUID fromString: '{aea81b01-b209-11d5-b8e3-000021fb21b2}')!
VisualInspector comment: 'No funciona.

De hecho esta clase es el proposito original por el que existe GraphicObjects, para hacer un VisualInspector, pero el proyecto paso a segundo plano, por ahora me concentro mas en hacer de GraphicObjects algo digno de usar, '!
!VisualInspector categoriesForClass!Unclassified! !
!VisualInspector methodsFor!

addObject


	|str|
	str := Prompter prompt:'Expresion'.
	self addObject: (Compiler evaluate: str logged: false).!

addObject: anObject


	^self graphicFor: anObject.!

createGraphicFor:  anObject


	|gr|

	gr := GraphicCompositeObject new.
	gr add: ( GraphicImage new valueModel: (anObject aspectValue: #icon) ; 
						bounds: (0@0 extent: anObject icon extent//2) ).
	gr add: ( GraphicText new valueModel: (anObject aspectValue: #displayString) ; 
						center: gr center + (0@ (anObject icon extent y//2)); extent: 100@30 ).
	gr metaObject:anObject.
	
	^gr!

expand: aGraphicObject

	|anObject|
	
	anObject := aGraphicObject metaObject.
	1 to: anObject class instSize + anObject basicSize 
	   do:[:i | graphics addGraphicObject: 
			(GraphicConnection from: aGraphicObject to:
				(((self addObject:(anObject instVarAt: i)) position: aGraphicObject position + 30 +
					( i *20  @ (i \\ 4 * 20) 
					) ))) 

			].
		
!

expandSelection


	graphics selection isNil ifTrue:[^self].
	graphics selection isEmpty ifTrue:[^self].
	graphics selection do:[:i | self expand: i].
!

graphicFor: anObject


	|graph|
	^objects at: anObject ifAbsentPut:[graphics addGraphicObject: (self createGraphicFor: anObject)].

!

initialize

	super initialize.

	objects := IdentityDictionary new.
!

onViewOpened


	graphics := (self view viewNamed: 'graphics') referee! !
!VisualInspector categoriesFor: #addObject!public! !
!VisualInspector categoriesFor: #addObject:!public! !
!VisualInspector categoriesFor: #createGraphicFor:!public! !
!VisualInspector categoriesFor: #expand:!public! !
!VisualInspector categoriesFor: #expandSelection!public! !
!VisualInspector categoriesFor: #graphicFor:!public! !
!VisualInspector categoriesFor: #initialize!public! !
!VisualInspector categoriesFor: #onViewOpened!public! !

!VisualInspector class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1140850688 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil 482 31 nil 7 nil nil nil 544 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 672 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 441 818 701 61 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 220 0 0 0 94 1 0 0 250 0 0 0] 34 3 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 928 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 7 nil nil nil 928 nil 8 4294904911 1180998 4 ##(Smalltalk.CommandDescription) #expandSelection 8 'expand' 1 1 nil nil false nil nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 818 1 1 818 233 61 928 770 #isEnabled: 8 #(false) 928 770 #text: 34 1 8 'expand' 928 866 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 0 0 0 30 0 0 0] 8 #() 818 193 193 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 1360 nil 994 1024 nil 7 nil nil nil 1360 nil 8 4294904911 1058 #addObject 8 'Add' 1 1 nil nil false nil nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 818 233 1 818 233 61 1360 770 #isEnabled: 8 #(false) 1360 770 #text: 34 1 8 'Add' 1360 866 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 116 0 0 0 0 0 0 0 232 0 0 0 30 0 0 0] 8 #() 1344 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 1728 nil 994 1024 nil 7 nil nil nil 1728 nil 8 4294904911 1058 nil nil 1 1 nil nil false nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 818 465 1 818 237 61 1728 770 #isEnabled: 8 #(false) 1728 866 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 232 0 0 0 0 0 0 0 94 1 0 0 30 0 0 0] 8 #() 1344 nil 29 1344 nil 27 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 2032 nil nil nil 5 nil nil nil 2032 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.GraphicObjectsView) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 818 1 1 818 701 441 2032 866 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 220 0 0 0] 672 1344 nil 27 170 192 34 2 2032 8 'graphics' nil 706 138 144 34 1 770 #createAt:extent: 34 2 818 11 11 818 701 501 416 866 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 99 1 0 0 255 0 0 0] 34 2 2032 544 1344 nil 27 )! !
!VisualInspector class categoriesFor: #resource_Default_view!public!resources-views! !

