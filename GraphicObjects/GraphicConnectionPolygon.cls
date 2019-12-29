"Filed out from Dolphin Smalltalk 7"!

GraphicOpenPolygon subclass: #GraphicConnectionPolygon
	instanceVariableNames: 'fromAction endAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicConnectionPolygon guid: (GUID fromString: '{c68864a0-b7c7-11d5-b8e3-000021fb21b2}')!
GraphicConnectionPolygon comment: ''!
!GraphicConnectionPolygon categoriesForClass!Unclassified! !
!GraphicConnectionPolygon methodsFor!

endAction
	"Private - Answer the value of the receiver's ''endAction'' instance variable."

	^endAction!

endAction: anObject
	"Private - Set the value of the receiver's ''endAction'' instance variable to the argument, anObject."

	endAction := anObject!

fromAction
	"Private - Answer the value of the receiver's ''fromAction'' instance variable."

	^fromAction!

fromAction: anObject
	"Private - Set the value of the receiver's ''fromAction'' instance variable to the argument, anObject."

	fromAction := anObject!

points


	^(OrderedCollection ofSize: points size +2 ) at: 1 put: fromAction value ; 
			at: points size + 2 put: endAction value
			; yourself! !
!GraphicConnectionPolygon categoriesFor: #endAction!accessing!private! !
!GraphicConnectionPolygon categoriesFor: #endAction:!accessing!private! !
!GraphicConnectionPolygon categoriesFor: #fromAction!accessing!private! !
!GraphicConnectionPolygon categoriesFor: #fromAction:!accessing!private! !
!GraphicConnectionPolygon categoriesFor: #points!public! !

!GraphicConnectionPolygon class methodsFor!

fromGraphic: aGraphic selector: aSymbol to: endGraphic selector: endSymbol


	^self new fromAction: (aGraphic aspectValue:aSymbol) ; endAction: (endGraphic aspectValue:endSymbol)!

fromGraphic: aGraphic to: anotherGraphic


	^self fromGraphic: aGraphic selector: #center to: anotherGraphic selector: #center! !
!GraphicConnectionPolygon class categoriesFor: #fromGraphic:selector:to:selector:!public! !
!GraphicConnectionPolygon class categoriesFor: #fromGraphic:to:!public! !

