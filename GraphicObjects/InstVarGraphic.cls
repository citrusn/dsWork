"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObject subclass: #InstVarGraphic
	instanceVariableNames: 'index'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
InstVarGraphic guid: (GUID fromString: '{d4ace4a0-c2b4-11d5-b8e3-000021fb21b2}')!
InstVarGraphic comment: ''!
!InstVarGraphic categoriesForClass!Unclassified! !
!InstVarGraphic methodsFor!

addRepresentedObjectTo: arena 

	
	arena model add: (GraphicConnection from: self to: ((arena model add: (GraphicNormalObject new metaObject: self representedObject)) position: self rightCenter + (20@0))).!

defaultHandlesFor: arena


	^OrderedCollection with: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent |self addRepresentedObjectTo: arena ] ; alignmentSelector: #rightCenter).!

index
	"Private - Answer the value of the receiver's ''index'' instance variable."

	^index!

index: anObject
	"Private - Set the value of the receiver's ''index'' instance variable to the argument, anObject."

	index := anObject!

metaObject: anObject

	metaObject := anObject.
	self removeAll.
	self add: (GraphicText new text:  self text ; extent:100@16 ).
	self add: (GraphicRectangle new extent: 100@16).!

representedObject


	^metaObject instVarAt: index!

text 


	^(metaObject class allInstVarNames at: index), ' :', (metaObject instVarAt: index) printString! !
!InstVarGraphic categoriesFor: #addRepresentedObjectTo:!public! !
!InstVarGraphic categoriesFor: #defaultHandlesFor:!public! !
!InstVarGraphic categoriesFor: #index!accessing!private! !
!InstVarGraphic categoriesFor: #index:!accessing!private! !
!InstVarGraphic categoriesFor: #metaObject:!public! !
!InstVarGraphic categoriesFor: #representedObject!public! !
!InstVarGraphic categoriesFor: #text!public! !

!InstVarGraphic class methodsFor!

object: anObject index: anIndex

	
	^self new index: anIndex ; metaObject: anObject! !
!InstVarGraphic class categoriesFor: #object:index:!public! !

