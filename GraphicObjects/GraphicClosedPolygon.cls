"Filed out from Dolphin Smalltalk 7"!

GraphicPath subclass: #GraphicClosedPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicClosedPolygon guid: (GUID fromString: '{5882ef61-b6a6-11d5-b8e3-000021fb21b2}')!
GraphicClosedPolygon comment: 'DrawingShell show'!
!GraphicClosedPolygon categoriesForClass!Unclassified! !
!GraphicClosedPolygon methodsFor!

asOpenPolygon


	^GraphicOpenPolygon new points: self points ; position: self position.
!

containsPoint: aPoint


	|cachedBounds end count prev |
	cachedBounds := self bounds .
	(cachedBounds containsPoint: aPoint) ifFalse:[^false].
	count := 0.
	prev := points first.
	(self points  ) do:[:point| (prev to: point intersectsLineFrom: aPoint to: cachedBounds origin) ifTrue:[ count := count +1].
				prev := point.
				] .
	(prev to: points first intersectsLineFrom: aPoint to: cachedBounds origin) ifTrue:[ count := count +1].


	^count odd!

drawOn: aCanvas

	
	points isEmpty ifTrue:[^self].
	graphicAttributes installOn: aCanvas.
	aCanvas polygon: self points.

!

fillColor

	^graphicAttributes fillColor!

fillColor:arg1 

	^graphicAttributes fillColor:arg1 !

fillHatch

	^graphicAttributes fillHatch!

fillHatch:arg1 

	^graphicAttributes fillHatch:arg1 !

fillStyle

	^graphicAttributes fillStyle!

fillStyle:arg1 

	^graphicAttributes fillStyle:arg1 ! !
!GraphicClosedPolygon categoriesFor: #asOpenPolygon!public! !
!GraphicClosedPolygon categoriesFor: #containsPoint:!public! !
!GraphicClosedPolygon categoriesFor: #drawOn:!public! !
!GraphicClosedPolygon categoriesFor: #fillColor!public! !
!GraphicClosedPolygon categoriesFor: #fillColor:!public! !
!GraphicClosedPolygon categoriesFor: #fillHatch!public! !
!GraphicClosedPolygon categoriesFor: #fillHatch:!public! !
!GraphicClosedPolygon categoriesFor: #fillStyle!public! !
!GraphicClosedPolygon categoriesFor: #fillStyle:!public! !

!GraphicClosedPolygon class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default fillColor: Color gray.! !
!GraphicClosedPolygon class categoriesFor: #defaultGraphicAttributes!public! !

