"Filed out from Dolphin Smalltalk 7"!

GraphicCompositeObject subclass: #BarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BarChart guid: (GUID fromString: '{a3c67580-c1ed-11d5-b8e3-000021fb21b2}')!
BarChart comment: ''!
!BarChart categoriesForClass!Unclassified! !
!BarChart methodsFor!

barFor: anInteger


	^(GraphicRectangle new extent: 10@ anInteger )!

createWith: aCollection
	| bar max |
	self removeAll.
	max := aCollection inject: 0 into: [:maxim :anInt | maxim max: anInt].
	aCollection keysAndValuesDo: 
			[:index :anInteger |
			(bar := self add: (self barFor: anInteger)) position: (index * bar extent x) @ (max - bar extent y)].
	self add: (GraphicLine from: self bottomLeft - (20 @ 0) to: self bottomRight + (20 @ 0)).
	self add: (GraphicLine from: self bottomLeft - (0 @ 20) to: self topLeft + (0 @ 20)).
	self calculateBounds!

metaObject: aCollection


	self createWith: aCollection! !
!BarChart categoriesFor: #barFor:!public! !
!BarChart categoriesFor: #createWith:!public! !
!BarChart categoriesFor: #metaObject:!public! !

