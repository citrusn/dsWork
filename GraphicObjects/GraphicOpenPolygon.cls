"Filed out from Dolphin Smalltalk 7"!

GraphicPath subclass: #GraphicOpenPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicOpenPolygon guid: (GUID fromString: '{5882ef60-b6a6-11d5-b8e3-000021fb21b2}')!
GraphicOpenPolygon comment: ''!
!GraphicOpenPolygon categoriesForClass!Unclassified! !
!GraphicOpenPolygon methodsFor!

asClosedPolygon


	^GraphicClosedPolygon new points: self points ; position: self position.!

asOpenPolygon


	^GraphicOpenPolygon new points: self points.!

containsPoint: aPoint


	|prev tmp|
	(super containsPoint: aPoint )ifFalse:[^false].
	prev := self points first.
	self points detect:[:point | tmp := point = prev ifTrue:[ false] 
							 ifFalse:[
		(aPoint dist: (aPoint nearestPointAlongLineFrom: prev to: point) )<= 3 ].
	prev:= point. 
	tmp] ifNone:[^false].
	^true!

drawOn: aCanvas

	
	self points isEmpty ifTrue:[^self].
	graphicAttributes installOn: aCanvas.
	aCanvas polyline: self points .
! !
!GraphicOpenPolygon categoriesFor: #asClosedPolygon!public! !
!GraphicOpenPolygon categoriesFor: #asOpenPolygon!public! !
!GraphicOpenPolygon categoriesFor: #containsPoint:!public! !
!GraphicOpenPolygon categoriesFor: #drawOn:!public! !

