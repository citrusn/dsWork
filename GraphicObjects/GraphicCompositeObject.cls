"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObjectAbstract subclass: #GraphicCompositeObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject guid: (GUID fromString: '{6536a8e5-b09e-11d5-b8e3-000021fb21b2}')!
GraphicCompositeObject comment: '(Shell show:''testGraph'') view subViews first addGraphicObject: GraphicCompositeObject test'!
!GraphicCompositeObject categoriesForClass!Unclassified! !
!GraphicCompositeObject methodsFor!

bottomRight: aPoint

	|delta|
	self invalidate.
	delta :=  (aPoint - self bottomRight ) rounded.
	self subObjects do:[:aGraph| aGraph bottomRight: (aGraph bottomRight + delta) ].
	self calculateBounds.
	self invalidate.!

bounds


	^self calculateBounds! !
!GraphicCompositeObject categoriesFor: #bottomRight:!public! !
!GraphicCompositeObject categoriesFor: #bounds!public! !

!GraphicCompositeObject class methodsFor!

measeure

	|path|
	path := GraphicOpenPolygon !

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered
	by instances of the receiver."

	^super publishedEventsOfInstances
		add: #graphicAdded:;
		add: #graphicRemoved:;
		yourself.
!

test
	|image|
	^self new add: (image := GraphicImage test ) ;
		       add: (((GraphicText text: 'anObject') extent: 105@12 )position: image center + (0@ (image extent y )) ).! !
!GraphicCompositeObject class categoriesFor: #measeure!public! !
!GraphicCompositeObject class categoriesFor: #publishedEventsOfInstances!public! !
!GraphicCompositeObject class categoriesFor: #test!public! !

