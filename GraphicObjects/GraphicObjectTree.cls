"Filed out from Dolphin Smalltalk 7"!

VirtualTreeModel subclass: #GraphicObjectTree
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectTree guid: (GUID fromString: '{c16bc920-e789-11d5-b8e3-9d717074350f}')!
GraphicObjectTree comment: ''!
!GraphicObjectTree categoriesForClass!Unclassified! !
!GraphicObjectTree methodsFor!

withRoots: aCollection
	"Private - Initialize the receiver with the roots in aCollection"

	"Deliberately omit super initialize"
	getChildrenBlock := Message selector: #subObjects.
	getParentBlock := Message selector: #parentGraphic.
	hasChildrenBlock := [:i | (i respondsTo: #subObjects) and:[ i subObjects size >0 ] ].
	self basicRoots: aCollection.! !
!GraphicObjectTree categoriesFor: #withRoots:!initializing!private! !

