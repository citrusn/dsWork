| package |
package := Package name: 'GraphicObjects'.
package paxVersion: 1;
	basicComment: 'GraphicObjects para Dolphin Smalltalk
por Andres Otaduy
<aotaduy@frro.utn.edu.ar>

GraphicObjects es un FrameWork para la creacion de herramientas de manipulacion directa y visualizacion de objetos.

Muchas de las ideas presentes en GraphicObjects han sido tomadas del conocido GF-ST de Instantiations Inc (www.instantiations.com).

El framework se basa en tres conceptos, objetos graficos (GraphicObject) que son los objetos que pueden ser visualizados en un Dibujo (GraphicDrawing), Herramientas (GraphicObjectManipulationTool) que representan a la herramienta seleccionada para manipular los objetos graficos (seleccion creacion conexion, etc) las herramientas reciben mensajes cuando el usuario hace algun gesto sobre la arena de trabajo (GraphicDrawingPresenter) ya sean clicks, dobleclicks, presion de teclas, etc y por ultimo los handles (GraphicHandle), los cuales representan objetos graficos de existencia temporal (no forman parte del Dibujo) que se utilizan para manipular algun aspecto de los objetos graficos. Estos handles reciben los gestos del mouse y accionan sobre los objetos graficos modificandolos, enviandoles un mensaje, etc.

El framework todavia se encuentra en desarrollo, faltan mejorar muchos aspectos todavia, pero en este momento se encuentra bastante utilizable. 

En el futuro se agregara:
	1) Impresion de un GraphicDrawing.
	2) Utilizacion de medidas reales (cms) y no de pantalla (pixels).
	3) Posibilidad de configurar mas facilmente los objetos graficos incrementando la reusabilidad ( el objetivo es bajar la necesidad de subclasificacion)
	4) Manejo de curvas.
	5) Mejorar el tema de las propiedades visuales de los objetos graficos (estilo de linea, relleno, etc)
	6) Paths compuestos (formas libres con huecos).
	7) Mejoras en el manejo de texto.
	8) Alguna sugerencia mas ???

Ejemplos :

	DrawingShell show model add: (GraphicNormalObject new metaObject: Object).
	DrawingShell show model add: (GraphicNormalObject new metaObject: #(1 ''peep'' #pop #(1 2 3))).
	DrawingShell show model add: BorderLayoutGraphicObject example3.
	DrawingShell show model add: ProportionalLayoutGraphicObject example1
	'.


package classNames
	add: #AlignmentGraphicObject;
	add: #BarChart;
	add: #BorderLayoutGraphicObject;
	add: #DelegatorMethodBuilder;
	add: #DrawingShell;
	add: #EditableColorPresenter;
	add: #GraphicAspectHandle;
	add: #GraphicCloneHandle;
	add: #GraphicClosedPolygon;
	add: #GraphicCollection;
	add: #GraphicColorHandle;
	add: #GraphicCompositeObject;
	add: #GraphicCompositeObjectAbstract;
	add: #GraphicConnectHandle;
	add: #GraphicConnection;
	add: #GraphicConnection2;
	add: #GraphicConnectionHandle;
	add: #GraphicConnectionPolygon;
	add: #GraphicDeleteHandle;
	add: #GraphicDrawing;
	add: #GraphicDrawingPresenter;
	add: #GraphicEllipse;
	add: #GraphicFixedBoundsObject;
	add: #GraphicHandle;
	add: #GraphicImage;
	add: #GraphicLeftButtonHandle;
	add: #GraphicLine;
	add: #GraphicMoveHandle;
	add: #GraphicNormalObject;
	add: #GraphicObject;
	add: #GraphicObjectBrushAttribute;
	add: #GraphicObjectBrushAttributePresenter;
	add: #GraphicObjectCreationTool;
	add: #GraphicObjectManipulationTool;
	add: #GraphicObjectPathCreationTool;
	add: #GraphicObjectPenAttribute;
	add: #GraphicObjectPenAttributePresenter;
	add: #GraphicObjectSelectionTool;
	add: #GraphicObjectsView;
	add: #GraphicObjectTextAttributes;
	add: #GraphicObjectTrackingTool;
	add: #GraphicObjectTree;
	add: #GraphicOpenPolygon;
	add: #GraphicPath;
	add: #GraphicRectangle;
	add: #GraphicSimpleClass;
	add: #GraphicSimpleObject;
	add: #GraphicText;
	add: #GraphicTrackerHandle;
	add: #GraphicValueObject;
	add: #IndexedInstVarGraphic;
	add: #InstVarGraphic;
	add: #ManagedLayoutGraphicComposite;
	add: #ProportionalLayoutGraphicObject;
	add: #ValueBlockAdaptor;
	add: #VisualInspector;
	yourself.

package methodNames
	add: #Point -> #to:intersectsLineFrom:to:;
	add: #UserLibrary -> #dragDetect:point:;
	add: 'Shell class' -> #resource_testGraph;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Color\Dolphin Color Presenter';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scrollbars\Dolphin Scrollbars';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Tree\Dolphin Tree Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'Lapiz';
	yourself).

package!

"Class Definitions"!

Object subclass: #DelegatorMethodBuilder
	instanceVariableNames: 'targetClass targetVar selectors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GraphicObjectManipulationTool
	instanceVariableNames: 'arena'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GraphicObjectPenAttribute
	instanceVariableNames: 'borderColor borderWidth borderStyle cachedPen oldCanvasState'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectManipulationTool subclass: #GraphicObjectPathCreationTool
	instanceVariableNames: 'path'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectManipulationTool subclass: #GraphicObjectTrackingTool
	instanceVariableNames: 'tracker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectTrackingTool subclass: #GraphicObjectCreationTool
	instanceVariableNames: 'newObjectPrototype'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectTrackingTool subclass: #GraphicObjectSelectionTool
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectPenAttribute subclass: #GraphicObjectBrushAttribute
	instanceVariableNames: 'fillStyle fillColor fillHatch cachedBrush'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectBrushAttribute subclass: #GraphicObjectTextAttributes
	instanceVariableNames: 'font textAlign textColor angle fontName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #GraphicObject
	instanceVariableNames: 'parentGraphic metaObject graphicAttributes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObject subclass: #GraphicConnection2
	instanceVariableNames: 'fromAction endAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObject subclass: #GraphicFixedBoundsObject
	instanceVariableNames: 'bounds'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObject subclass: #GraphicHandle
	instanceVariableNames: 'target arena positionAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObject subclass: #GraphicPath
	instanceVariableNames: 'points'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject subclass: #GraphicCompositeObjectAbstract
	instanceVariableNames: 'subObjects openEditing'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject subclass: #GraphicEllipse
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject subclass: #GraphicLine
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject subclass: #GraphicRectangle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicFixedBoundsObject subclass: #GraphicValueObject
	instanceVariableNames: 'valueModel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObjectAbstract subclass: #GraphicCompositeObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObjectAbstract subclass: #ManagedLayoutGraphicComposite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject subclass: #BarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject subclass: #GraphicCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject subclass: #GraphicDrawing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject subclass: #GraphicSimpleObject
	instanceVariableNames: 'ellipse text'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicCompositeObject subclass: #InstVarGraphic
	instanceVariableNames: 'index'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
InstVarGraphic subclass: #IndexedInstVarGraphic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ManagedLayoutGraphicComposite subclass: #AlignmentGraphicObject
	instanceVariableNames: 'orientation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ManagedLayoutGraphicComposite subclass: #BorderLayoutGraphicObject
	instanceVariableNames: 'north south centerGraphic east west'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ManagedLayoutGraphicComposite subclass: #ProportionalLayoutGraphicObject
	instanceVariableNames: 'proportions horizontal'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BorderLayoutGraphicObject subclass: #GraphicNormalObject
	instanceVariableNames: 'instVars'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BorderLayoutGraphicObject subclass: #GraphicSimpleClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicLine subclass: #GraphicConnection
	instanceVariableNames: 'from to'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicValueObject subclass: #GraphicImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicValueObject subclass: #GraphicText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicHandle subclass: #GraphicColorHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicHandle subclass: #GraphicDeleteHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicHandle subclass: #GraphicLeftButtonHandle
	instanceVariableNames: 'action'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicHandle subclass: #GraphicTrackerHandle
	instanceVariableNames: 'tracker'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle subclass: #GraphicAspectHandle
	instanceVariableNames: 'adaptor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle subclass: #GraphicCloneHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle subclass: #GraphicConnectHandle
	instanceVariableNames: 'connection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle subclass: #GraphicConnectionHandle
	instanceVariableNames: 'connectionEndSelector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicTrackerHandle subclass: #GraphicMoveHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicPath subclass: #GraphicClosedPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicPath subclass: #GraphicOpenPolygon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicOpenPolygon subclass: #GraphicConnectionPolygon
	instanceVariableNames: 'fromAction endAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VirtualTreeModel subclass: #GraphicObjectTree
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValueAdaptor subclass: #ValueBlockAdaptor
	instanceVariableNames: 'getValueBlock setValueBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #GraphicDrawingPresenter
	instanceVariableNames: 'selection handles tool'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #GraphicObjectPenAttributePresenter
	instanceVariableNames: 'borderColor borderWidth borderStyle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #VisualInspector
	instanceVariableNames: 'graphics objects'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicObjectPenAttributePresenter subclass: #GraphicObjectBrushAttributePresenter
	instanceVariableNames: 'fillStyle fillColor fillHatch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #DrawingShell
	instanceVariableNames: 'drawingPresenter attributesWindow'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ColorPresenter subclass: #EditableColorPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #GraphicObjectsView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Point methodsFor!

to: aPoint intersectsLineFrom: lineStart to: lineEnd

"	Returns wheter the line determined by the receiver end aPoint intersects with the line from lineStart to: lineEnd

intersect:= ( (ccw (ll.pl, ll.p2, 12.pl )*ccw (ll.pl, ll.p2, 12.p2))<=0)
	and ((ccw (12.pl, 12.p2, ll.pl )*ccw (12.pl, 12.p2, ll.p2))<=0);
end;"

	^ (self ccw: aPoint and:lineStart) * (self ccw: aPoint and:lineEnd)<=0 and:[  (lineStart ccw: lineEnd and:self ) * (lineStart ccw: lineEnd and:aPoint)<=0].! !
!Point categoriesFor: #to:intersectsLineFrom:to:!public! !

!Shell class methodsFor!

resource_testGraph
	"Answer the literal data from which the 'testGraph' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_testGraph)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 nil 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 544 nil 466 496 nil 5 nil nil nil 544 nil 8 4294905513 852486 ##(Smalltalk.NullConverter) nil nil 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 351 786 903 51 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 175 0 0 0 195 1 0 0 200 0 0 0] 8 #() 786 193 193 nil 27 nil nil 410 ##(Smalltalk.ScrollingDecorator) 34 18 nil 416 34 2 8 1143996416 131073 912 nil 466 496 nil 5 nil nil nil 912 1573190 1 ##(Smalltalk.ScrollingDecoratorLayout) true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 912 34 2 8 1141047296 131073 1056 nil nil nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #setPathTool 8 'Path' 1 1 nil nil nil 1170 1 1202 #setSelectionTool 8 'Selection' 1 1 nil nil nil 1170 1 1202 #setEllipseTool 8 'Ellipse' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1056 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.GraphicObjectsView) #resource_Default_view nil 674 138 144 34 2 738 #createAt:extent: 34 2 786 1 1 786 903 501 1056 738 #contextMenu: 34 1 1136 1056 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 195 1 0 0 250 0 0 0] 8 #() 896 nil 27 8 'drawing' nil 786 1 1 true 786 17 17 674 138 144 34 1 738 #createAt:extent: 34 2 786 1 1 786 903 351 912 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 195 1 0 0 175 0 0 0] 34 1 1056 896 nil 27 170 192 1568 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 674 138 144 34 2 738 #createAt:extent: 34 2 786 201 201 786 2881 1513 416 738 #updateMenuBar 1568 416 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 100 0 0 0 4 6 0 0 88 3 0 0] 34 2 544 912 896 nil 27 )! !
!Shell class categoriesFor: #resource_testGraph!public!resources-views! !

!UserLibrary methodsFor!

dragDetect: aWindowHandle point: aPoint
	"Captures the mouse and tracks its movement until the user releases the left button, presses the ESC key, or moves the mouse outside the drag rectangle around the specified point. 
	The width and height of the drag rectangle are specified by the SM_CXDRAG and SM_CYDRAG values returned by the GetSystemMetrics function.
	BOOL DragDetect( HWND  hwnd, POINT pt )."
	
	<stdcall: bool DragDetect handle  POINT>
	^self invalidCall! !
!UserLibrary categoriesFor: #dragDetect:point:!public!win32 functions-mouse input! !

"End of package definition"!

"Source Globals"!

"Classes"!

DelegatorMethodBuilder guid: (GUID fromString: '{6e46bf41-b61b-11d5-b8e3-000021fb21b2}')!
DelegatorMethodBuilder comment: ''!
!DelegatorMethodBuilder categoriesForClass!Unclassified! !
!DelegatorMethodBuilder methodsFor!

generateMethods

	|stream|
	(self selectors "reject:[:aSymbol | 	targetClass respondsTo: aSymbol ]")
				do:[:aSymbol | 
	stream := String writeStream.
	self writeHeaderOn: stream symbol: aSymbol.
	stream cr;
		   cr;
		   tab;
			nextPut: $^; 
		   nextPutAll: targetVar displayString;
		space.
			self writeHeaderOn:stream symbol: aSymbol.
	targetClass compile: stream contents.
	]
		!

selectors
	"Private - Answer the value of the receiver's ''selectors'' instance variable."

	^selectors!

selectors: anObject
	"Private - Set the value of the receiver's ''selectors'' instance variable to the argument, anObject."

	selectors := anObject!

targetClass
	"Private - Answer the value of the receiver's ''targetClass'' instance variable."

	^targetClass!

targetClass: anObject
	"Private - Set the value of the receiver's ''targetClass'' instance variable to the argument, anObject."

	targetClass := anObject!

targetVar
	"Private - Answer the value of the receiver's ''targetVar'' instance variable."

	^targetVar!

targetVar: anObject
	"Private - Set the value of the receiver's ''targetVar'' instance variable to the argument, anObject."

	targetVar := anObject!

writeHeaderOn:stream symbol: aSymbol


	|readStream count|
	readStream := aSymbol asString readStream.
	count := 0.
	[readStream atEnd] 
		whileFalse:[
			readStream peek = $: ifTrue:[ stream nextPut: readStream next;
									         nextPutAll: 'arg';
										display: (count := count +1);
								    space.]
							 ifFalse:[stream nextPut: readStream next.].
				].
		
	! !
!DelegatorMethodBuilder categoriesFor: #generateMethods!public! !
!DelegatorMethodBuilder categoriesFor: #selectors!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #selectors:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetClass!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetClass:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetVar!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #targetVar:!accessing!private! !
!DelegatorMethodBuilder categoriesFor: #writeHeaderOn:symbol:!public! !

!DelegatorMethodBuilder class methodsFor!

class: aClass target: aString


	!

class: aClass target: aString selectors: aCollection


	^self new targetClass: aClass;
			targetVar: aString;
			selectors: aCollection;
			generateMethods.!

class: aClass target: aString targetClass: aTarget

	|aCollection|
	aCollection := ChoicePrompter multipleChoices: aTarget selectors asArray.
	aCollection isNil ifTrue:[^self].

	^self new targetClass: aClass;
			targetVar: aString;
			selectors: aCollection;
			generateMethods.! !
!DelegatorMethodBuilder class categoriesFor: #class:target:!public! !
!DelegatorMethodBuilder class categoriesFor: #class:target:selectors:!public! !
!DelegatorMethodBuilder class categoriesFor: #class:target:targetClass:!public! !

GraphicObjectManipulationTool guid: (GUID fromString: '{a6c9d000-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectManipulationTool comment: ''!
!GraphicObjectManipulationTool categoriesForClass!Unclassified! !
!GraphicObjectManipulationTool methodsFor!

arena
	"Private - Answer the value of the receiver's ''arena'' instance variable."

	^arena!

arena: anObject
	"Private - Set the value of the receiver's ''arena'' instance variable to the argument, anObject."

	arena := anObject!

cursor



	^Cursor arrow!

onKeyPressed: aKeyEvent
	"Default handler for a key press event.
	Accept the default window processing."

!

onKeyReleased: aKeyEvent
	"Default handler for a key up event.
	Accept the default window processing."
!

onKeyTyped: aKeyEvent
	"Default handler for a keyboard event.
	Accept the default window processing."
!

onLeftButtonDoubleClicked: aMouseEvent hit: aGraphicObject
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	!

onLeftButtonPressedBack: aMouseEvent
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	!

onLeftButtonReleased: aMouseEvent
	"Default handler for a Left button up mouse event.
	Accept the default window processing."

!

onMiddleButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse middle button double-click event.
	Accept the default window processing."

!

onMiddleButtonPressed: aMouseEvent
	"Default handler for a middle button down mouse event.
	Accept the default window processing."

	!

onMiddleButtonReleased: aMouseEvent
	"Default handler for a Middle button up mouse event.
	Accept the default window processing."
!

onMouseMoved: aMouseEvent
	"Default handler for a mouse move event.
	Accept the default processing.
	Note: We don't trigger an event here to save churning."

	!

onMouseWheel: aMouseEvent
	"Default handler for a ouseWheel mouse event.
	Accept the default window processing."

!

onRightButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse Right button double-click event.
	Accept the default window processing."

!

onRightButtonPressed: aMouseEvent
	"Default handler for a Right button down mouse event.
	Accept the default window processing."

	!

onRightButtonReleased: aMouseEvent
	"Default handler for a Right button up mouse event.
	Accept the default window processing."

! !
!GraphicObjectManipulationTool categoriesFor: #arena!accessing!private! !
!GraphicObjectManipulationTool categoriesFor: #arena:!accessing!private! !
!GraphicObjectManipulationTool categoriesFor: #cursor!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyReleased:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onKeyTyped:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonDoubleClicked:hit:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonPressed:hit:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonPressedBack:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onLeftButtonReleased:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonDoubleClicked:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonPressed:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMiddleButtonReleased:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onMouseMoved:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onMouseWheel:!event handling!idb goodies!private! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonDoubleClicked:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonPressed:!event handling!public! !
!GraphicObjectManipulationTool categoriesFor: #onRightButtonReleased:!event handling!public! !

!GraphicObjectManipulationTool class methodsFor!

for: aGraphicView



	^self new arena: aGraphicView! !
!GraphicObjectManipulationTool class categoriesFor: #for:!public! !

GraphicObjectPenAttribute guid: (GUID fromString: '{6e46bf3e-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectPenAttribute comment: ''!
!GraphicObjectPenAttribute categoriesForClass!Unclassified! !
!GraphicObjectPenAttribute methodsFor!

borderColor
	"Private - Answer the value of the receiver's ''borderColor'' instance variable."

	^borderColor!

borderColor: anObject
	"Private - Set the value of the receiver's ''borderColor'' instance variable to the argument, anObject."

	self invalidatePen.
	borderColor := anObject!

borderStyle
	"Private - Answer the value of the receiver's ''borderStyle'' instance variable."

	^borderStyle!

borderStyle: anObject
	"Private - Set the value of the receiver's ''borderStyle'' instance variable to the argument, anObject."
	self invalidatePen.
	borderStyle := anObject!

borderWidth
	"Private - Answer the value of the receiver's ''borderWidth'' instance variable."

	^borderWidth!

borderWidth: anObject
	"Private - Set the value of the receiver's ''borderWidth'' instance variable to the argument, anObject."

	self invalidatePen.
	borderWidth := anObject!

cachedPen
	"Private - Answer the value of the receiver's ''cachedPen'' instance variable."

	^cachedPen!

cachedPen: anObject
	"Private - Set the value of the receiver's ''cachedPen'' instance variable to the argument, anObject."

	cachedPen := anObject!

installOn: aCanvas


	aCanvas pen: self pen!

invalidatePen


	self cachedPen: nil!

oldCanvasState
	"Private - Answer the value of the receiver's ''oldCanvasState'' instance variable."

	^oldCanvasState!

oldCanvasState: anObject
	"Private - Set the value of the receiver's ''oldCanvasState'' instance variable to the argument, anObject."

	oldCanvasState := anObject!

pen
	""

	cachedPen isNil ifTrue:[ self cachedPen: (Pen withStyle: self borderStyle width: self borderWidth color: self borderColor )].

	^cachedPen!

presenterClass


	^GraphicObjectPenAttributePresenter!

presentIn: aCompositePresenter

	|presenter|
	presenter := self presenterClass createIn: aCompositePresenter on: self.
	aCompositePresenter view extent: 0@20 + presenter view extent .
	presenter view position: 0@0.
! !
!GraphicObjectPenAttribute categoriesFor: #borderColor!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderColor:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderStyle!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderStyle:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderWidth!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #borderWidth:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #cachedPen!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #cachedPen:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #installOn:!public! !
!GraphicObjectPenAttribute categoriesFor: #invalidatePen!public! !
!GraphicObjectPenAttribute categoriesFor: #oldCanvasState!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #oldCanvasState:!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #pen!accessing!private! !
!GraphicObjectPenAttribute categoriesFor: #presenterClass!public! !
!GraphicObjectPenAttribute categoriesFor: #presentIn:!public! !

!GraphicObjectPenAttribute class methodsFor!

default


	^self new borderColor: Color black; borderWidth: 1 ; borderStyle: (Win32Constants at: 'PS_SOLID').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect color: #borderColor);
		add: (Aspect integer: #borderWidth).
	^aspects.! !
!GraphicObjectPenAttribute class categoriesFor: #default!public! !
!GraphicObjectPenAttribute class categoriesFor: #publishedAspectsOfInstances!public! !

GraphicObjectPathCreationTool guid: (GUID fromString: '{a8bb9281-b2fe-11d5-b8e3-000021fb21b2}')!
GraphicObjectPathCreationTool comment: 'DrawingShell show'!
!GraphicObjectPathCreationTool categoriesForClass!Unclassified! !
!GraphicObjectPathCreationTool methodsFor!

cursor



	^Cursor cross!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	 self arena setSelectionTool.
!

onLeftButtonPressedBack: aMouseEvent


	self path isNil ifTrue:[ path:= GraphicClosedPolygon new.
					self arena model add: path.
					self arena addSelection: path.].

	self path addPoint: aMouseEvent position.!

path
	"Private - Answer the value of the receiver's ''path'' instance variable."

	^path!

path: anObject
	"Private - Set the value of the receiver's ''path'' instance variable to the argument, anObject."

	path := anObject! !
!GraphicObjectPathCreationTool categoriesFor: #cursor!public! !
!GraphicObjectPathCreationTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectPathCreationTool categoriesFor: #onLeftButtonPressedBack:!public! !
!GraphicObjectPathCreationTool categoriesFor: #path!accessing!private! !
!GraphicObjectPathCreationTool categoriesFor: #path:!accessing!private! !

GraphicObjectTrackingTool guid: (GUID fromString: '{30721920-efe9-11d5-b8e3-d4bacc772835}')!
GraphicObjectTrackingTool comment: ''!
!GraphicObjectTrackingTool categoriesForClass!Unclassified! !
!GraphicObjectTrackingTool methodsFor!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	arena view drawFocusRect: (tracker origin corner: aPreviousPoint) normalize asParameter.
	arena  view drawFocusRect: (tracker origin corner: aPoint) normalize asParameter.

	^aPoint

!

onLeftButtonPressedBack: aMouseEvent
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	(tracker := MouseTracker forPresenter: arena startingAt: aMouseEvent screenPosition)
		origin: aMouseEvent position;
		startTracking: self.
!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (tracker origin corner: aPoint) normalize.

	^aPoint! !
!GraphicObjectTrackingTool categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicObjectTrackingTool categoriesFor: #onLeftButtonPressedBack:!event handling!public! !
!GraphicObjectTrackingTool categoriesFor: #startTrackingAt:!private!tracking! !

GraphicObjectCreationTool guid: (GUID fromString: '{a6c9d002-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectCreationTool comment: ''!
!GraphicObjectCreationTool categoriesForClass!Unclassified! !
!GraphicObjectCreationTool methodsFor!

cursor



	^Cursor arrow!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|selectionRect|

	Cursor wait showWhile:[
		(self arena model add: 
				(self newObjectPrototype clone  
					bounds: (Rectangle vertex: tracker origin vertex: aPoint))
		) invalidate ; bringToFront.
 ].
	arena  view drawFocusRect: (tracker origin corner: aPoint )normalize.
!

newObjectPrototype
	"Private - Answer the value of the receiver's ''newObjectPrototype'' instance variable."

	^newObjectPrototype!

newObjectPrototype: anObject
	"Private - Set the value of the receiver's ''newObjectPrototype'' instance variable to the argument, anObject."

	newObjectPrototype := anObject!

onButtonPressed: aMouseEvent


	Cursor wait showWhile:[(self arena model add: self newObjectPrototype clone ) position: aMouseEvent position ]!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

self onLeftButtonPressedBack: aMouseEvent.! !
!GraphicObjectCreationTool categoriesFor: #cursor!public! !
!GraphicObjectCreationTool categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicObjectCreationTool categoriesFor: #newObjectPrototype!public! !
!GraphicObjectCreationTool categoriesFor: #newObjectPrototype:!public! !
!GraphicObjectCreationTool categoriesFor: #onButtonPressed:!public! !
!GraphicObjectCreationTool categoriesFor: #onLeftButtonPressed:hit:!public! !

!GraphicObjectCreationTool class methodsFor!

ellipse


	^self new newObjectPrototype: GraphicEllipse new!

line


	^self new newObjectPrototype: GraphicLine new!

text


	^self new newObjectPrototype: GraphicText test! !
!GraphicObjectCreationTool class categoriesFor: #ellipse!public! !
!GraphicObjectCreationTool class categoriesFor: #line!public! !
!GraphicObjectCreationTool class categoriesFor: #text!public! !

GraphicObjectSelectionTool guid: (GUID fromString: '{a6c9d001-b10e-11d5-b8e3-000021fb21b2}')!
GraphicObjectSelectionTool comment: ''!
!GraphicObjectSelectionTool categoriesForClass!Unclassified! !
!GraphicObjectSelectionTool methodsFor!

cursor



	^Cursor cross!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|selectionRect|
	arena  view drawFocusRect: (Rectangle vertex: tracker origin vertex: aPoint) asParameter.
	selectionRect := (tracker origin corner: aPoint )normalize.
	arena basicSelection: ( arena model subObjects select:[:aGraphObject| selectionRect contains: aGraphObject bounds]).!

onKeyPressed: aKeyEvent
	"Default handler for a key press event.
	Accept the default window processing."

	
	aKeyEvent code = 46 "VK_DELETE" ifTrue:[ self arena removeSelection].!

onLeftButtonDoubleClickedBack: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	^self arena selectAll!

onLeftButtonPressed: aMouseEvent hit: aGraphicObject
	"Default handler for a left button down mouse event.
	Accept the default window processing."

	aMouseEvent isShiftDown ifTrue:[ ^(self arena isSelected: aGraphicObject ) ifTrue:[ self arena removeSelection: aGraphicObject] 
												 ifFalse:[self arena addSelection: aGraphicObject] ].
	arena basicSelection: (OrderedCollection  with: aGraphicObject)! !
!GraphicObjectSelectionTool categoriesFor: #cursor!public! !
!GraphicObjectSelectionTool categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicObjectSelectionTool categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicObjectSelectionTool categoriesFor: #onLeftButtonDoubleClickedBack:!event handling!public! !
!GraphicObjectSelectionTool categoriesFor: #onLeftButtonPressed:hit:!event handling!public! !

GraphicObjectBrushAttribute guid: (GUID fromString: '{6e46bf3f-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectBrushAttribute comment: ''!
!GraphicObjectBrushAttribute categoriesForClass!Unclassified! !
!GraphicObjectBrushAttribute methodsFor!

brush
	""

	cachedBrush isNil ifTrue:[ self cachedBrush: (Brush  color: fillColor )].

	^cachedBrush!

cachedBrush
	"Private - Answer the value of the receiver's ''cachedBrush'' instance variable."

	^cachedBrush!

cachedBrush: anObject
	"Private - Set the value of the receiver's ''cachedBrush'' instance variable to the argument, anObject."


	cachedBrush := anObject!

fillColor
	"Private - Answer the value of the receiver's ''fillColor'' instance variable."

	^fillColor!

fillColor: anObject
	"Private - Set the value of the receiver's ''fillColor'' instance variable to the argument, anObject."
	self invalidateBrush.
	fillColor := anObject!

fillHatch
	"Private - Answer the value of the receiver's ''fillHatch'' instance variable."

	^fillHatch!

fillHatch: anObject
	"Private - Set the value of the receiver's ''fillHatch'' instance variable to the argument, anObject."
	self invalidateBrush.

	fillHatch := anObject!

fillStyle
	"Private - Answer the value of the receiver's ''fillStyle'' instance variable."

	^fillStyle!

fillStyle: anObject
	"Private - Set the value of the receiver's ''fillStyle'' instance variable to the argument, anObject."
	self invalidateBrush.

	fillStyle := anObject!

installOn: aCanvas


	super installOn: aCanvas.
	aCanvas brush: self brush!

invalidateBrush


	self cachedBrush: nil!

presenterClass


	^GraphicObjectBrushAttributePresenter! !
!GraphicObjectBrushAttribute categoriesFor: #brush!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #cachedBrush!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #cachedBrush:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillColor!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillColor:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillHatch!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillHatch:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillStyle!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #fillStyle:!accessing!private! !
!GraphicObjectBrushAttribute categoriesFor: #installOn:!public! !
!GraphicObjectBrushAttribute categoriesFor: #invalidateBrush!public! !
!GraphicObjectBrushAttribute categoriesFor: #presenterClass!public! !

!GraphicObjectBrushAttribute class methodsFor!

default


	^super  default fillColor: Color none ; fillHatch: 1 ; fillStyle: (Win32Constants at: 'BS_SOLID').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect color: #fillColor).
	
	^aspects.! !
!GraphicObjectBrushAttribute class categoriesFor: #default!public! !
!GraphicObjectBrushAttribute class categoriesFor: #publishedAspectsOfInstances!public! !

GraphicObjectTextAttributes guid: (GUID fromString: '{6e46bf40-b61b-11d5-b8e3-000021fb21b2}')!
GraphicObjectTextAttributes comment: ''!
!GraphicObjectTextAttributes categoriesForClass!Unclassified! !
!GraphicObjectTextAttributes methodsFor!

angle

	^font  angle!

angle:arg1 

	^font  angle:arg1 !

font
	"Private - Answer the value of the receiver's ''font'' instance variable."

	^font!

font: anObject
	"Private - Set the value of the receiver's ''font'' instance variable to the argument, anObject."

	font := anObject!

installOn: aCanvas


	super installOn: aCanvas.
	aCanvas setTextColor: self textColor ;
			font: self font.!

isBold

	^font  isBold!

isBold:arg1 

	^font  isBold:arg1 !

isItalic

	^font  isItalic!

isItalic:arg1 

	^font  isItalic:arg1 !

isStruckThrough

	^font  isStruckThrough!

isStruckThrough:arg1 

	^font  isStruckThrough:arg1 !

isUnderlined

	^font  isUnderlined!

isUnderlined:arg1 

	^font  isUnderlined:arg1 !

name

	^font  name!

name:arg1 

	^font  name:arg1 !

pixelSize

	^font  pixelSize!

pointSize

	^font  pointSize!

pointSize:arg1 

	^font  pointSize:arg1 !

textAlign
	"Private - Answer the value of the receiver's ''textAlign'' instance variable."

	^textAlign!

textAlign: anObject
	"Private - Set the value of the receiver's ''textAlign'' instance variable to the argument, anObject."

	textAlign := anObject!

textColor
	"Private - Answer the value of the receiver's ''textColor'' instance variable."

	^textColor!

textColor: anObject
	"Private - Set the value of the receiver's ''textColor'' instance variable to the argument, anObject."

	textColor := anObject! !
!GraphicObjectTextAttributes categoriesFor: #angle!public! !
!GraphicObjectTextAttributes categoriesFor: #angle:!public! !
!GraphicObjectTextAttributes categoriesFor: #font!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #font:!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #installOn:!public! !
!GraphicObjectTextAttributes categoriesFor: #isBold!public! !
!GraphicObjectTextAttributes categoriesFor: #isBold:!public! !
!GraphicObjectTextAttributes categoriesFor: #isItalic!public! !
!GraphicObjectTextAttributes categoriesFor: #isItalic:!public! !
!GraphicObjectTextAttributes categoriesFor: #isStruckThrough!public! !
!GraphicObjectTextAttributes categoriesFor: #isStruckThrough:!public! !
!GraphicObjectTextAttributes categoriesFor: #isUnderlined!public! !
!GraphicObjectTextAttributes categoriesFor: #isUnderlined:!public! !
!GraphicObjectTextAttributes categoriesFor: #name!public! !
!GraphicObjectTextAttributes categoriesFor: #name:!public! !
!GraphicObjectTextAttributes categoriesFor: #pixelSize!public! !
!GraphicObjectTextAttributes categoriesFor: #pointSize!public! !
!GraphicObjectTextAttributes categoriesFor: #pointSize:!public! !
!GraphicObjectTextAttributes categoriesFor: #textAlign!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textAlign:!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textColor!accessing!private! !
!GraphicObjectTextAttributes categoriesFor: #textColor:!accessing!private! !

!GraphicObjectTextAttributes class methodsFor!

default


	^super  default textColor: Color black ; font: (Font name: 'Arial' pointSize: 8 )  ; textAlign: (Win32Constants at: 'DT_CENTER').!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
	
		add: (Aspect font: #font);
		add: (Aspect color: #textColor);
		add: (Aspect integer: #angle).

	^aspects.! !
!GraphicObjectTextAttributes class categoriesFor: #default!public! !
!GraphicObjectTextAttributes class categoriesFor: #publishedAspectsOfInstances!public! !

GraphicObject guid: (GUID fromString: '{7e3e11a1-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicObject comment: ''!
!GraphicObject categoriesForClass!Unclassified! !
!GraphicObject methodsFor!

attach: aSelfLocation to: aGraphicObject location: aGraphicObjectLocation

	"Attachs the receiver to aGraphicObject"

	aGraphicObject 	when: #positionChanged 
				send: #move:nextTo:location: 
				to: self 
				withArguments: (Array with: (aSelfLocation , ':')  asSymbol with: aGraphicObject with: aGraphicObjectLocation).!

attachTo: aGraphicObject location: aSymbol

	"Attachs the receiver to aGraphicObject"

	aGraphicObject 	when: #positionChanged 
				send: #moveNextTo:location: 
				to: self 
				withArguments: (Array with: aGraphicObject with:aSymbol).!

borderColor

	^graphicAttributes borderColor!

borderColor:arg1 

	^graphicAttributes borderColor:arg1 !

borderStyle:arg1 

	^graphicAttributes borderStyle:arg1 !

borderWidth

	^graphicAttributes borderWidth!

borderWidth:arg1 

	^graphicAttributes borderWidth:arg1 !

bottom

	^self bounds bottom!

bottomCenter

	^self bounds bottomCenter!

bottomLeft

	^self bounds bottomLeft!

bottomRight

	^self bounds bottomRight!

bounds
	"Private - Answer the value of the receiver's ''bounds'' instance variable."

	^self calculateBounds!

bringToFront

	self parentGraphic bringToFront: self. !

center

	^self bounds center!

clone


	^self deepCopy!

containsPoint: aPoint


	^self bounds containsPoint: aPoint!

corner

	^self bounds corner!

defaultHandleClasses


	^OrderedCollection new addAll: (Array with:GraphicConnectHandle with: GraphicMoveHandle with: GraphicDeleteHandle with: GraphicCloneHandle )   ; yourself!

defaultHandlesFor: aGraphicView



	^self defaultHandleClasses collect:[:aClass | aClass for: self arena: aGraphicView].!

delete

	^self parentGraphic remove: self!

drawOn: aCanvas


		^graphicAttributes installOn: aCanvas.

!

extent

	^self bounds extent!

graphicAttributes
	"Private - Answer the value of the receiver's ''graphicAttributes'' instance variable."

	^graphicAttributes!

graphicAttributes: anObject
	"Private - Set the value of the receiver's ''graphicAttributes'' instance variable to the argument, anObject."

	graphicAttributes := anObject!

height

	^self bounds height!

initialize


	graphicAttributes := self class defaultGraphicAttributes.
	!

invalidate


	^self invalidateRectangle: self bounds!

invalidateRectangle: aRectangleOrNil


	^self parentGraphic isNil ifFalse:[ self trigger: #invalidateRectangle: with: aRectangleOrNil.
							self parentGraphic invalidateRectangle: aRectangleOrNil].!

left

	^self bounds left!

leftCenter

	^self bounds leftCenter!

metaObject
	"Private - Answer the value of the receiver's ''metaObject'' instance variable."

	^metaObject!

metaObject: anObject
	"Private - Set the value of the receiver's ''metaObject'' instance variable to the argument, anObject."

	metaObject := anObject!

move: aSelfLocation nextTo: aGraphicObject location: aSymbol

	^self perform: aSelfLocation with: (aGraphicObject perform: aSymbol)!

move: aSelfLocation NextTo: aGraphicObject location: aSymbol

	^self perform: aSelfLocation with: (aGraphicObject perform: aSymbol)!

moveNextTo: aGraphicObject location: aSymbol

	^self perform: (aSymbol, ':') asSymbol with: (aGraphicObject perform: aSymbol)!

origin

	^self bounds origin!

parentGraphic
	"Private - Answer the value of the receiver's ''parentGraphic'' instance variable."

	^parentGraphic!

parentGraphic: anObject
	"Private - Set the value of the receiver's ''parentGraphic'' instance variable to the argument, anObject."

	parentGraphic := anObject!

pen
	"Private - Answer the value of the receiver's ''pen'' instance variable."

	^Pen withStyle: (self class penStyleForStyleName: self borderStyle)
		width: self borderWidth 
		color: self borderColor !

position


	^self bounds origin!

publishedAspects
	"Answer a Set of AspectDescriptors that describe the aspects published
	by instances of this class. Add in a instance based aspect for the arrangement
	based on the layout manager of the parent"

	| aspects attributesAspects|
	aspects := super publishedAspects.
	attributesAspects := self graphicAttributes publishedAspects.
	aspects addAll: attributesAspects.
	^aspects.!

right

	^self bounds right!

rightCenter

	^self bounds rightCenter!

sendToBack

	self parentGraphic sendToBack: self. !

top

	^self bounds top!

topCenter

	^self bounds topCenter!

topLeft

	^self bounds topLeft!

topRight

	^self bounds topRight!

width

	^self bounds width! !
!GraphicObject categoriesFor: #attach:to:location:!public! !
!GraphicObject categoriesFor: #attachTo:location:!public! !
!GraphicObject categoriesFor: #borderColor!public! !
!GraphicObject categoriesFor: #borderColor:!public! !
!GraphicObject categoriesFor: #borderStyle:!public! !
!GraphicObject categoriesFor: #borderWidth!public! !
!GraphicObject categoriesFor: #borderWidth:!public! !
!GraphicObject categoriesFor: #bottom!public! !
!GraphicObject categoriesFor: #bottomCenter!public! !
!GraphicObject categoriesFor: #bottomLeft!public! !
!GraphicObject categoriesFor: #bottomRight!public! !
!GraphicObject categoriesFor: #bounds!accessing!private! !
!GraphicObject categoriesFor: #bringToFront!public! !
!GraphicObject categoriesFor: #center!public! !
!GraphicObject categoriesFor: #clone!copying!public! !
!GraphicObject categoriesFor: #containsPoint:!public! !
!GraphicObject categoriesFor: #corner!public! !
!GraphicObject categoriesFor: #defaultHandleClasses!public! !
!GraphicObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicObject categoriesFor: #delete!public! !
!GraphicObject categoriesFor: #drawOn:!public! !
!GraphicObject categoriesFor: #extent!public! !
!GraphicObject categoriesFor: #graphicAttributes!accessing!private! !
!GraphicObject categoriesFor: #graphicAttributes:!accessing!private! !
!GraphicObject categoriesFor: #height!public! !
!GraphicObject categoriesFor: #initialize!public! !
!GraphicObject categoriesFor: #invalidate!public! !
!GraphicObject categoriesFor: #invalidateRectangle:!public! !
!GraphicObject categoriesFor: #left!public! !
!GraphicObject categoriesFor: #leftCenter!public! !
!GraphicObject categoriesFor: #metaObject!accessing!private! !
!GraphicObject categoriesFor: #metaObject:!accessing!private! !
!GraphicObject categoriesFor: #move:nextTo:location:!public! !
!GraphicObject categoriesFor: #move:NextTo:location:!public! !
!GraphicObject categoriesFor: #moveNextTo:location:!public! !
!GraphicObject categoriesFor: #origin!public! !
!GraphicObject categoriesFor: #parentGraphic!accessing!private! !
!GraphicObject categoriesFor: #parentGraphic:!accessing!private! !
!GraphicObject categoriesFor: #pen!accessing!private! !
!GraphicObject categoriesFor: #position!public! !
!GraphicObject categoriesFor: #publishedAspects!public! !
!GraphicObject categoriesFor: #right!public! !
!GraphicObject categoriesFor: #rightCenter!public! !
!GraphicObject categoriesFor: #sendToBack!public! !
!GraphicObject categoriesFor: #top!public! !
!GraphicObject categoriesFor: #topCenter!public! !
!GraphicObject categoriesFor: #topLeft!public! !
!GraphicObject categoriesFor: #topRight!public! !
!GraphicObject categoriesFor: #width!public! !

!GraphicObject class methodsFor!

defaultGraphicAttributes


	^GraphicObjectPenAttribute default!

new


	^super new initialize!

penStyleForStyleName: aSymbol


	^(#(solid dot dash dashDot  ) indexOf: aSymbol ) -1


!

test


	^ (self  new bounds: ( 10@10 extent: 30) )! !
!GraphicObject class categoriesFor: #defaultGraphicAttributes!public! !
!GraphicObject class categoriesFor: #new!public! !
!GraphicObject class categoriesFor: #penStyleForStyleName:!public! !
!GraphicObject class categoriesFor: #test!public! !

GraphicConnection2 guid: (GUID fromString: '{93cc2360-f040-11d5-b8e3-a73e87cebb00}')!
GraphicConnection2 comment: 'DrawingShell show'!
!GraphicConnection2 categoriesForClass!Unclassified! !
!GraphicConnection2 methodsFor!

basicTranslateBy: anOffset

!

bounds


	^(Rectangle vertex: fromAction value vertex: endAction value) expandedBy: 4!

containsPoint: aPoint



	(super containsPoint: aPoint )ifFalse:[^false].

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: fromAction value to: endAction value) )<= 3!

defaultHandlesFor: aGraphicView

	|col|
	col := OrderedCollection new.
"	col addAll: (super defaultHandlesFor: aGraphicView)."
	col
	add: (GraphicDeleteHandle for: self arena: aGraphicView ).
	^col!

drawOn: aCanvas

	super drawOn: aCanvas.	
	^(Lapiz canvas: aCanvas ) penUp; 
					      goto: fromAction value; 
					      penDown;
					      arrowTo: endAction value.



!

endAction
	"Private - Answer the value of the receiver's ''endAction'' instance variable."

	^endAction!

endAction: anObject
	"Private - Set the value of the receiver's ''endAction'' instance variable to the argument, anObject."

	endAction := anObject!

from: anotherGraphicObject 

	self fromAction isNil ifFalse:[ fromAction receiver removeEventsTriggeredFor: self].
	self fromAction: (MessageSend receiver: anotherGraphicObject selector: #center).
	anotherGraphicObject when: #invalidateRectangle: send: #invalidate to: self .


!

fromAction
	"Private - Answer the value of the receiver's ''fromAction'' instance variable."

	^fromAction!

fromAction: anObject
	"Private - Set the value of the receiver's ''fromAction'' instance variable to the argument, anObject."

	fromAction := anObject!

to: anotherGraphicObject 

	self endAction isNil ifFalse:[ endAction receiver removeEventsTriggeredFor: self].
	self endAction: (MessageSend receiver: anotherGraphicObject selector: #center).
	anotherGraphicObject when: #invalidateRectangle: send: #invalidate to: self .
! !
!GraphicConnection2 categoriesFor: #basicTranslateBy:!public! !
!GraphicConnection2 categoriesFor: #bounds!public! !
!GraphicConnection2 categoriesFor: #containsPoint:!public! !
!GraphicConnection2 categoriesFor: #defaultHandlesFor:!public! !
!GraphicConnection2 categoriesFor: #drawOn:!public! !
!GraphicConnection2 categoriesFor: #endAction!accessing!private! !
!GraphicConnection2 categoriesFor: #endAction:!accessing!private! !
!GraphicConnection2 categoriesFor: #from:!public! !
!GraphicConnection2 categoriesFor: #fromAction!accessing!private! !
!GraphicConnection2 categoriesFor: #fromAction:!accessing!private! !
!GraphicConnection2 categoriesFor: #to:!public! !

!GraphicConnection2 class methodsFor!

from: aGraphicObject
to: anotherGraphicObject

^self new from: aGraphicObject ;
			to:  anotherGraphicObject .
	
! !
!GraphicConnection2 class categoriesFor: #from:to:!public! !

GraphicFixedBoundsObject guid: (GUID fromString: '{aacb6500-b164-11d5-b8e3-000021fb21b2}')!
GraphicFixedBoundsObject comment: ''!
!GraphicFixedBoundsObject categoriesForClass!Unclassified! !
!GraphicFixedBoundsObject methodsFor!

basicBounds: anObject


	bounds:= anObject!

basicCenter: aPoint


	self basicTranslateBy: aPoint - self center!

basicPosition: aPoint

	

	bounds moveTo: aPoint.
!

basicTranslateBy: delta


		bounds moveBy: delta.!

bottomLeft: aPoint


	self invalidate.
	self bounds bottomLeft: aPoint.
	self trigger: #sizeChanged.
"	self invalidate."!

bottomRight: aPoint


	self invalidate.
	self bounds bottomRight: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

bounds
	"Private - Answer the value of the receiver's ''bounds'' instance variable."

	^bounds!

bounds: anObject
	"Private - Set the value of the receiver's ''bounds'' instance variable to the argument, anObject."

	self invalidate.
	bounds := anObject!

calculateBounds


	^bounds!

center: aPoint


	self invalidate.
	self basicCenter: aPoint.
	self trigger: #positionChanged.
	self invalidate.!

children
	^ OrderedCollection new!

corner: aPoint


	self invalidate.
	self basicTranslateBy: aPoint - self corner.
	self trigger: #positionChanged.
	self invalidate.!

defaultHandlesFor: aGraphicView



	^(super defaultHandlesFor: aGraphicView) addAll:(self resizeHandlesFor: aGraphicView) ; yourself!

extent: aPoint


	self invalidate.
	self bounds extent: aPoint.
	self bounds: self bounds normalize.
	self trigger: #sizeChanged.
	self invalidate.!

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

	^graphicAttributes fillStyle:arg1 !

initialize


	super initialize.
	bounds := Rectangle origin: 10@10 extent: 20.!

origin: aPoint


	self invalidate.
	self bounds origin: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

position: aPoint

	
"	self invalidate."
	bounds moveTo: aPoint.
	self trigger: #positionChanged.
	self invalidate.!

resizeHandlesFor: aGraphicView


	^#(bottomRight topLeft topRight bottomLeft) collect:[:aSymbol | 
		(GraphicAspectHandle for: self arena:aGraphicView ) adaptor: (self  aspectValue:aSymbol).].


!

topLeft: aPoint


	self invalidate.
	self bounds topLeft: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

topRight: aPoint


	self invalidate.
	self bounds topRight: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

translateBy: delta

	self invalidate.
	self basicTranslateBy: delta.
	self trigger: #positionChanged.
	self invalidate.! !
!GraphicFixedBoundsObject categoriesFor: #basicBounds:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicCenter:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicPosition:!public! !
!GraphicFixedBoundsObject categoriesFor: #basicTranslateBy:!public! !
!GraphicFixedBoundsObject categoriesFor: #bottomLeft:!public! !
!GraphicFixedBoundsObject categoriesFor: #bottomRight:!public! !
!GraphicFixedBoundsObject categoriesFor: #bounds!accessing!private! !
!GraphicFixedBoundsObject categoriesFor: #bounds:!accessing!private! !
!GraphicFixedBoundsObject categoriesFor: #calculateBounds!public! !
!GraphicFixedBoundsObject categoriesFor: #center:!public! !
!GraphicFixedBoundsObject categoriesFor: #children!public! !
!GraphicFixedBoundsObject categoriesFor: #corner:!public! !
!GraphicFixedBoundsObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicFixedBoundsObject categoriesFor: #extent:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillColor!public! !
!GraphicFixedBoundsObject categoriesFor: #fillColor:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillHatch!public! !
!GraphicFixedBoundsObject categoriesFor: #fillHatch:!public! !
!GraphicFixedBoundsObject categoriesFor: #fillStyle!public! !
!GraphicFixedBoundsObject categoriesFor: #fillStyle:!public! !
!GraphicFixedBoundsObject categoriesFor: #initialize!public! !
!GraphicFixedBoundsObject categoriesFor: #origin:!public! !
!GraphicFixedBoundsObject categoriesFor: #position:!public! !
!GraphicFixedBoundsObject categoriesFor: #resizeHandlesFor:!public! !
!GraphicFixedBoundsObject categoriesFor: #topLeft:!public! !
!GraphicFixedBoundsObject categoriesFor: #topRight:!public! !
!GraphicFixedBoundsObject categoriesFor: #translateBy:!public! !

!GraphicFixedBoundsObject class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicFixedBoundsObject class categoriesFor: #defaultGraphicAttributes!public! !

GraphicHandle guid: (GUID fromString: '{7e3e11a5-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicHandle comment: ''!
!GraphicHandle categoriesForClass!Unclassified! !
!GraphicHandle methodsFor!

alignmentSelector: anObject
	"Private - Set the value of the receiver's ''alignmentSelector'' instance variable to the argument, anObject."



	positionAction := MessageSend receiver: self target selector: anObject!

arena
	"Private - Answer the value of the receiver's ''arena'' instance variable."

	^arena!

arena: anObject
	"Private - Set the value of the receiver's ''arena'' instance variable to the argument, anObject."

	arena := anObject!

calculateBounds


	^(self positionAction value) + self offset -3 extent: 6!

drawOn: aCanvas

	aCanvas setDefaultAttributes.
	^aCanvas rectangle: self bounds

!

initialize


	self alignmentSelector: #topLeft	!

invalidate

"	super invalidate."
	self arena view invalidateRect: (self bounds expandedBy:1) erase: false!

offset


	^0!

onLeftButtonDoubleClicked: aMouseEvent 
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

!

onLeftButtonDrag: aMouseEvent


	
!

onLeftButtonPressed: aMouseEvent

	"Some visual feedBack"
	arena view  canvas fillRectangle: self bounds   brush: Brush white.
	 (Delay forMilliseconds:100) wait. 
	self drawOn: arena view  canvas 

	!

positionAction
	"Private - Answer the value of the receiver's ''positionAction'' instance variable."

	^positionAction!

positionAction: anObject
	"Private - Set the value of the receiver's ''positionAction'' instance variable to the argument, anObject."

	positionAction := anObject!

target
	"Private - Answer the value of the receiver's ''target'' instance variable."

	^target!

target: anObject
	"Private - Set the value of the receiver's ''target'' instance variable to the argument, anObject."
	
	anObject  when: #invalidateRectangle: send: #invalidate to: self.
"	anObject  when: #positionChanged send: #invalidate to: self.
	anObject  when: #sizeChanged send: #invalidate to: self."
	target := anObject.
	! !
!GraphicHandle categoriesFor: #alignmentSelector:!accessing!public! !
!GraphicHandle categoriesFor: #arena!accessing!private! !
!GraphicHandle categoriesFor: #arena:!accessing!private! !
!GraphicHandle categoriesFor: #calculateBounds!public! !
!GraphicHandle categoriesFor: #drawOn:!public! !
!GraphicHandle categoriesFor: #initialize!public! !
!GraphicHandle categoriesFor: #invalidate!public! !
!GraphicHandle categoriesFor: #offset!public! !
!GraphicHandle categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!GraphicHandle categoriesFor: #onLeftButtonDrag:!public! !
!GraphicHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicHandle categoriesFor: #positionAction!public! !
!GraphicHandle categoriesFor: #positionAction:!public! !
!GraphicHandle categoriesFor: #target!accessing!private! !
!GraphicHandle categoriesFor: #target:!accessing!private! !

!GraphicHandle class methodsFor!

for: aGraphicObject arena:aGraphicView



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			 initialize


!

for: aGraphicObject arena:aGraphicView positionAction: anAction



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			positionAction: anAction
			 


! !
!GraphicHandle class categoriesFor: #for:arena:!public! !
!GraphicHandle class categoriesFor: #for:arena:positionAction:!public! !

GraphicPath guid: (GUID fromString: '{a6c9d003-b10e-11d5-b8e3-000021fb21b2}')!
GraphicPath comment: '(Shell show:''testGraph'') view subViews first addGraphicObject: self test


DrawingShell show'!
!GraphicPath categoriesForClass!Unclassified! !
!GraphicPath methodsFor!

add: aPoint


	self addPoint: aPoint!

addPoint: aPoint

	self invalidate.
	self points add: aPoint.
	self trigger: #sizeChanged.
	self invalidate.!

addPoint: aPoint afterIndex: anIndex

	self invalidate.
	self points add: aPoint afterIndex: anIndex.
	self trigger: #sizeChanged.
	self invalidate.!

addPointHandleFor: aGraphicView index: anIndex

	
	|prev next| 
	prev := ValueKeyedAdaptor subject: self key: anIndex .
	next := ValueKeyedAdaptor subject: self key: anIndex  +1.

	^(GraphicLeftButtonHandle for: self  
					arena: aGraphicView 
					positionAction: [ (Rectangle vertex: prev value vertex: next value )  center]) action:[:aMouseEvent| self addPoint: aMouseEvent position afterIndex: anIndex ].


!

at: anIndex


	^points at: anIndex!

at: index put: aPoint

	"Deberia invalidar solo el rectangulo desde el punto anterior y el siguiiente"
	self invalidate.
	 points at: index put: aPoint.
	self trigger: #sizeChanged.
	self invalidate



!

basicTranslateBy: delta

	self points: (points collect:[:aPoint | aPoint + delta]).!

calculateBounds

	"Ugly but fast!!"
		|newOrigin newCorner|

	self points isEmpty ifTrue:[^0@0 corner: 0@0].
	newOrigin:= newCorner := self points first.
	1 to: self points size do:[:i | 

		newOrigin := (self points at:i) min: newOrigin.
		newCorner := (self points at:i) max: newCorner.
			].
	^newOrigin corner: newCorner.

!

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

defaultHandleClasses


	^Array "with:GraphicConnectHandle "with: GraphicMoveHandle with: GraphicDeleteHandle with: GraphicCloneHandle with: GraphicColorHandle !

defaultHandlesFor: aGraphicView


	^((1 to: points size) collect:[:i | self pointHandleFor: aGraphicView index: i]) asOrderedCollection addAll: (super defaultHandlesFor: aGraphicView);
		addAll: ((1 to: points size -1) collect:[:i | self addPointHandleFor: aGraphicView index: i]) ; yourself.
	
	!

initialize


	super initialize.
	points := OrderedCollection new.
!

pointHandleFor: aGraphicView index: anIndex



	^GraphicAspectHandle for: self  
					arena: aGraphicView 
					adaptor: ( ValueKeyedAdaptor subject: self key: anIndex)!

points
	"Private - Answer the value of the receiver's ''points'' instance variable."

	^points!

points: anObject
	"Private - Set the value of the receiver's ''points'' instance variable to the argument, anObject."

	points := anObject!

translateBy: delta

	self invalidate.
	self basicTranslateBy: delta.
	self trigger: #positionChanged.
	self invalidate.! !
!GraphicPath categoriesFor: #add:!public! !
!GraphicPath categoriesFor: #addPoint:!public! !
!GraphicPath categoriesFor: #addPoint:afterIndex:!public! !
!GraphicPath categoriesFor: #addPointHandleFor:index:!public! !
!GraphicPath categoriesFor: #at:!public! !
!GraphicPath categoriesFor: #at:put:!public! !
!GraphicPath categoriesFor: #basicTranslateBy:!public! !
!GraphicPath categoriesFor: #calculateBounds!public! !
!GraphicPath categoriesFor: #containsPoint:!public! !
!GraphicPath categoriesFor: #defaultHandleClasses!public! !
!GraphicPath categoriesFor: #defaultHandlesFor:!public! !
!GraphicPath categoriesFor: #initialize!public! !
!GraphicPath categoriesFor: #pointHandleFor:index:!public! !
!GraphicPath categoriesFor: #points!public! !
!GraphicPath categoriesFor: #points:!public! !
!GraphicPath categoriesFor: #translateBy:!public! !

!GraphicPath class methodsFor!

boundingRectForPoints: points


	|newOrigin newCorner|

	points isEmpty ifTrue:[^0@0 corner: 0@0].
	newOrigin:= newCorner := points first.
	1 to:  points size do:[:i | 

		newOrigin := (points at:i) min: newOrigin.
		newCorner := (points at:i) max: newCorner.
			].
	^newOrigin corner: newCorner.
!

test

	^self new points: ((1 to: 10 ) collect:[:i |  i  @ i * 10] )! !
!GraphicPath class categoriesFor: #boundingRectForPoints:!public! !
!GraphicPath class categoriesFor: #test!public! !

GraphicCompositeObjectAbstract guid: (GUID fromString: '{c68d3441-e6a6-11d5-b8e3-e0f30574409d}')!
GraphicCompositeObjectAbstract comment: ''!
!GraphicCompositeObjectAbstract categoriesForClass!Unclassified! !
!GraphicCompositeObjectAbstract methodsFor!

add: aGraphicObject



	self subObjects add: aGraphicObject.
	aGraphicObject parentGraphic: self.
	self trigger: #graphicAdded: with: aGraphicObject.
	bounds := self calculateBounds.

	^aGraphicObject!

addAll: aCollection



	self subObjects addAll: aCollection.
	aCollection do:[:aGraphicObject | 		aGraphicObject parentGraphic: self.
								self trigger: #graphicAdded: with: aGraphicObject.].
	bounds := self calculateBounds.

	^aCollection!

basicPosition: aPoint

	|delta|
	delta :=  aPoint - self position .
	self subObjects do:[:aGraphObj | aGraphObj basicPosition: aGraphObj position + delta].
	"self calculateBounds." "podria ser "super basicPosition:aPoint.
!

basicTranslateBy: aPoint



	super basicTranslateBy: aPoint.
	self subObjects do:[:aGraphObj | aGraphObj basicTranslateBy: aPoint].
!

bringToFront: aGraphicObject
	aGraphicObject == self subObjects first ifTrue: [^self].
	subObjects addFirst: (subObjects remove: aGraphicObject).
	self invalidate.
	^aGraphicObject!

calculateBounds
	subObjects isEmpty ifTrue: [^bounds].
	^self subObjects inject: self subObjects first bounds
		into: [:max :aGraphicObject | max merge: aGraphicObject bounds]!

children
	"Private - Answer the value of the receiver's ''subObjects'' instance variable."

	^subObjects!

decompose

	^self parentGraphic addAll: self subObjects ; remove: self!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	openEditing ifTrue: [self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)].].
	col addAll: (super defaultHandlesFor: arena).


	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent | arena model addAll: self subObjects; remove: self] ; 	alignmentSelector: #bottomCenter).
	
	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent | self switchEditing] ; alignmentSelector: #rightCenter).
	^col!

drawOn: aCanvas


"	aCanvas  selectClipRegion: (Region rectangle: (self bounds))."
	self subObjects  reverseDo:[:aGraphicObject |aGraphicObject drawOn: aCanvas].
"	aCanvas  selectClipRegion: nil"!

graphicObjectAt: aPoint


	^self subObjects detect:[:aGraphicObject | aGraphicObject containsPoint: aPoint] ifNone:[nil].!

initialize

	super initialize.
	subObjects := OrderedCollection new.
	openEditing := false.!

position: aPoint

	|delta|
	self invalidate.
	delta :=  aPoint - self position .
	
	self subObjects do:[:aGraphObj | aGraphObj basicPosition: aGraphObj position + delta].
	"self calculateBounds." "podria ser "super position:aPoint.
	self invalidate.!

remove: oldElement


		^self remove: oldElement ifAbsent: [self errorNotFound: oldElement]
!

remove: aGraphicObject ifAbsent: aBlock


	self invalidate.
	self subObjects remove: aGraphicObject ifAbsent: aBlock.
	aGraphicObject parentGraphic: nil.
	self trigger: #graphicRemoved: with: aGraphicObject.
	self calculateBounds.

	^aGraphicObject!

removeAll
	| ans |
	ans := subObjects.
	subObjects := OrderedCollection new.
	^ans!

sendToBack: aGraphicObject
	aGraphicObject == self subObjects last ifTrue: [^self].
	(subObjects addLast: (subObjects remove: aGraphicObject)) invalidate.
	self invalidate.
	^aGraphicObject!

subObjects
	"Private - Answer the value of the receiver's ''subObjects'' instance variable."

	^subObjects!

subObjects: anObject
	"Private - Set the value of the receiver's ''subObjects'' instance variable to the argument, anObject."

	subObjects := anObject!

switchEditing

	openEditing := openEditing not! !
!GraphicCompositeObjectAbstract categoriesFor: #add:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #addAll:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #basicPosition:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #basicTranslateBy:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #bringToFront:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #calculateBounds!public! !
!GraphicCompositeObjectAbstract categoriesFor: #children!private! !
!GraphicCompositeObjectAbstract categoriesFor: #decompose!public! !
!GraphicCompositeObjectAbstract categoriesFor: #defaultHandlesFor:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #drawOn:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #graphicObjectAt:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #initialize!public! !
!GraphicCompositeObjectAbstract categoriesFor: #position:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #remove:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #remove:ifAbsent:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #removeAll!public! !
!GraphicCompositeObjectAbstract categoriesFor: #sendToBack:!public! !
!GraphicCompositeObjectAbstract categoriesFor: #subObjects!accessing!private! !
!GraphicCompositeObjectAbstract categoriesFor: #subObjects:!accessing!private! !
!GraphicCompositeObjectAbstract categoriesFor: #switchEditing!public! !

GraphicEllipse guid: (GUID fromString: '{7e3e11a2-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicEllipse comment: ''!
!GraphicEllipse categoriesForClass!Unclassified! !
!GraphicEllipse methodsFor!

containsPoint: aPoint

	"Robado de Squeak>>EllipseMorph>>containsPoint: "

	| radius other delta xOverY |
	(bounds containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	(bounds width = 1 or: [bounds height = 1])
		ifTrue: [^ true].  "Degenerate case -- code below fails by a bit"

	radius :=  bounds height asFloat / 2.
	other :=  bounds width asFloat / 2.
	delta :=  aPoint - bounds topLeft - (other@radius).
	xOverY :=  bounds width asFloat / bounds height asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared!

drawOn: aCanvas


	super drawOn: aCanvas.
	aCanvas ellipse: self bounds.

!

initialize

	super initialize.
	! !
!GraphicEllipse categoriesFor: #containsPoint:!public! !
!GraphicEllipse categoriesFor: #drawOn:!public! !
!GraphicEllipse categoriesFor: #initialize!public! !

!GraphicEllipse class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicEllipse class categoriesFor: #defaultGraphicAttributes!public! !

GraphicLine guid: (GUID fromString: '{7e3e11a3-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicLine comment: ''!
!GraphicLine categoriesForClass!Unclassified! !
!GraphicLine methodsFor!

containsPoint: aPoint



	(super containsPoint: aPoint )ifFalse:[^false].

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: self bounds origin to: self bounds corner) )<= 3!

drawOn: aCanvas

		graphicAttributes installOn: aCanvas.
	^aCanvas moveTo: self bounds origin; 
			lineTo: self bounds corner


! !
!GraphicLine categoriesFor: #containsPoint:!public! !
!GraphicLine categoriesFor: #drawOn:!public! !

!GraphicLine class methodsFor!

from: origin
to: corner

	^self new bounds:  (origin corner: corner)! !
!GraphicLine class categoriesFor: #from:to:!public! !

GraphicRectangle guid: (GUID fromString: '{6e46bf3d-b61b-11d5-b8e3-000021fb21b2}')!
GraphicRectangle comment: ''!
!GraphicRectangle categoriesForClass!Unclassified! !
!GraphicRectangle methodsFor!

drawOn: aCanvas


	graphicAttributes installOn: aCanvas.
	aCanvas rectangle: (self bounds insetBy: graphicAttributes borderWidth -1).

! !
!GraphicRectangle categoriesFor: #drawOn:!public! !

!GraphicRectangle class methodsFor!

defaultGraphicAttributes


	^GraphicObjectBrushAttribute default! !
!GraphicRectangle class categoriesFor: #defaultGraphicAttributes!public! !

GraphicValueObject guid: (GUID fromString: '{aacb6501-b164-11d5-b8e3-000021fb21b2}')!
GraphicValueObject comment: ''!
!GraphicValueObject categoriesForClass!Unclassified! !
!GraphicValueObject methodsFor!

initialize

	super initialize.
	valueModel := nil asValue!

valueModel
	"Private - Answer the value of the receiver's ''valueModel'' instance variable."

	^valueModel!

valueModel: anObject
	"Private - Set the value of the receiver's ''valueModel'' instance variable to the argument, anObject."

	valueModel when:#valueChanged send: #invalidate to: self.

	valueModel := anObject! !
!GraphicValueObject categoriesFor: #initialize!public! !
!GraphicValueObject categoriesFor: #valueModel!accessing!private! !
!GraphicValueObject categoriesFor: #valueModel:!accessing!private! !

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

ManagedLayoutGraphicComposite guid: (GUID fromString: '{9fc61f81-e727-11d5-b8e3-9162307a4e65}')!
ManagedLayoutGraphicComposite comment: ''!
!ManagedLayoutGraphicComposite categoriesForClass!Unclassified! !
!ManagedLayoutGraphicComposite methodsFor!

add: aGraphicObject

	super add: aGraphicObject.
	self reorganize.

	^aGraphicObject!

addAll: aCollection

	super addAll: aCollection.
	self reorganize.

	^aCollection!

basicBounds: aRectangle

	super bounds: aRectangle.
	self reorganize!

bottomRight: aPoint

	super bottomRight: aPoint.
	self reorganize.
!

bounds: aRect

	super bounds: aRect.
	self reorganize.

	!

remove: aGraphicObject ifAbsent: aBlock



	super remove: aGraphicObject ifAbsent: aBlock.
	self reorganize.

	^aGraphicObject!

reorganize! !
!ManagedLayoutGraphicComposite categoriesFor: #add:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #addAll:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #basicBounds:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #bottomRight:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #bounds:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #remove:ifAbsent:!public! !
!ManagedLayoutGraphicComposite categoriesFor: #reorganize!public! !

BarChart guid: (GUID fromString: '{a3c67580-c1ed-11d5-b8e3-000021fb21b2}')!
BarChart comment: ''!
!BarChart categoriesForClass!Unclassified! !
!BarChart methodsFor!

barFor: anInteger
	^GraphicRectangle new extent: 10 @ anInteger!

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

GraphicCollection guid: (GUID fromString: '{a3c67583-c1ed-11d5-b8e3-000021fb21b2}')!
GraphicCollection comment: ''!
!GraphicCollection categoriesForClass!Unclassified! !
!GraphicCollection methodsFor!

metaObject: aCollection

	|col grap|
	super metaObject: aCollection.
	col := self add: (GraphicSimpleObject new metaObject: aCollection).
	aCollection asArray keysAndValuesDo:[:index :object | 

					grap := (subObjects add: (GraphicSimpleObject new metaObject: object) )position: index * 10 @ 40.
					subObjects add: (GraphicConnection from: col to:grap)].
self calculateBounds.
	! !
!GraphicCollection categoriesFor: #metaObject:!public! !

GraphicDrawing guid: (GUID fromString: '{a6c1aac0-b5d1-11d5-b8e3-000021fb21b2}')!
GraphicDrawing comment: ''!
!GraphicDrawing categoriesForClass!Unclassified! !
!GraphicDrawing methodsFor!

invalidateRectangle: aRectangleOrNil


	^self trigger: #invalidateRectangle: with: aRectangleOrNil!

metaObject: aCollection


	self createWith: aCollection! !
!GraphicDrawing categoriesFor: #invalidateRectangle:!public! !
!GraphicDrawing categoriesFor: #metaObject:!public! !

GraphicSimpleObject guid: (GUID fromString: '{a3c67581-c1ed-11d5-b8e3-000021fb21b2}')!
GraphicSimpleObject comment: ''!
!GraphicSimpleObject categoriesForClass!Unclassified! !
!GraphicSimpleObject methodsFor!

defaultHandlesFor: aPresenter

	
	^ellipse defaultHandlesFor: aPresenter!

graphicAttributes


	^ellipse graphicAttributes!

metaObject: anObject


	text := GraphicText new text: anObject printString.
	ellipse := GraphicRectangle new extent: 40@40.
	ellipse when: #sizeChanged send: #onEllipseSizeChanged to: self.
	self add: text ; add: ellipse.


	!

onEllipseSizeChanged 

	self invalidate.
	text extent: ellipse extent.
	text center: ellipse center.
	self calculateBounds.
	self invalidate.

	! !
!GraphicSimpleObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicSimpleObject categoriesFor: #graphicAttributes!public! !
!GraphicSimpleObject categoriesFor: #metaObject:!public! !
!GraphicSimpleObject categoriesFor: #onEllipseSizeChanged!public! !

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

IndexedInstVarGraphic guid: (GUID fromString: '{9fc61f82-e727-11d5-b8e3-9162307a4e65}')!
IndexedInstVarGraphic comment: ''!
!IndexedInstVarGraphic categoriesForClass!Unclassified! !
!IndexedInstVarGraphic methodsFor!

representedObject


	^metaObject basicAt: index!

text 


	^(index displayString), ' :', (metaObject basicAt: index) printString! !
!IndexedInstVarGraphic categoriesFor: #representedObject!public! !
!IndexedInstVarGraphic categoriesFor: #text!public! !

AlignmentGraphicObject guid: (GUID fromString: '{c68d3440-e6a6-11d5-b8e3-e0f30574409d}')!
AlignmentGraphicObject comment: 'DrawingShell show'!
!AlignmentGraphicObject categoriesForClass!Unclassified! !
!AlignmentGraphicObject methodsFor!

beHorizontal

	orientation := #horizontal.
	self reorganize
	!

beVertical

	orientation := #vertical.
	self reorganize
	!

bottomRight: aPoint

	^super bottomRight: (aPoint max: self calculateBounds corner) .

!

bounds


	^bounds!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	col addAll: (super defaultHandlesFor: arena).
	"self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)]."
	col add: ((GraphicLeftButtonHandle for: self arena: arena selector: #switchOrientation) 
			 alignmentSelector: #topCenter).
	^col!

initialize


super initialize.
	orientation := #horizontal.
	self when:#sizeChanged send: #reorganize  to: self.!

orientation
	"Private - Answer the value of the receiver's ''orientation'' instance variable."

	^orientation!

orientation: anObject
	"Private - Set the value of the receiver's ''orientation'' instance variable to the argument, anObject."

	orientation := anObject!

reorganize
	
	subObjects isEmpty ifTrue:[^self].
	orientation == #vertical ifTrue:[^self reorganizeVertical].
	self reorganizeHorizontal
	!

reorganizeHorizontal

	|delta margen centerY|
	margen :=   ( self extent x- self totalExtent x  / subObjects size ) floor .
	centerY := self center y.
	delta := self origin x + (self subObjects first extent x /2) rounded.
	self subObjects do:[:aGraphicObject | aGraphicObject basicCenter:   delta @ centerY.
								delta := delta + ( aGraphicObject extent x ) + margen].
	self invalidate.
	!

reorganizeVertical

	|delta margen centerX|
	margen :=   ( self extent y - self totalExtent y  / subObjects size ) floor .
	centerX := self center x.
	delta := self origin y + (self subObjects first extent y /2) rounded.

	self subObjects do:[:aGraphicObject | aGraphicObject basicCenter:    centerX @ delta.
								delta := delta + ( aGraphicObject extent y ) + margen].
	self invalidate.
	!

reorganizeVertical1

	|delta|
	delta := (subObjects inject: 0@0 into:[:suma : aGraphicObject | suma + aGraphicObject extent]) / subObjects size.
	self subObjects do:[:aGraphicObject | aGraphicObject basicPosition: self origin + delta.
								delta := delta + (0@aGraphicObject extent  y )].
	self calculateBounds.
	self invalidate.
	!

reorganizeVertical2

	|delta margen|
	margen :=  0@ ( self extent y - self totalExtent y / (subObjects size )  ) floor .
	
	delta := 0@0.
	self subObjects do:[:aGraphicObject | aGraphicObject basicPosition: self origin + delta.
								delta := delta + (0@ aGraphicObject extent y) + margen].
	self invalidate.
	!

switchOrientation

	orientation == #horizontal ifTrue:[ ^self beVertical].
	self beHorizontal.
	!

totalExtent


	^subObjects inject: 0@0 into:[:suma : aGraphicObject | suma + aGraphicObject extent].! !
!AlignmentGraphicObject categoriesFor: #beHorizontal!public! !
!AlignmentGraphicObject categoriesFor: #beVertical!public! !
!AlignmentGraphicObject categoriesFor: #bottomRight:!public! !
!AlignmentGraphicObject categoriesFor: #bounds!public! !
!AlignmentGraphicObject categoriesFor: #defaultHandlesFor:!public! !
!AlignmentGraphicObject categoriesFor: #initialize!public! !
!AlignmentGraphicObject categoriesFor: #orientation!accessing!private! !
!AlignmentGraphicObject categoriesFor: #orientation:!accessing!private! !
!AlignmentGraphicObject categoriesFor: #reorganize!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeHorizontal!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical1!public! !
!AlignmentGraphicObject categoriesFor: #reorganizeVertical2!public! !
!AlignmentGraphicObject categoriesFor: #switchOrientation!public! !
!AlignmentGraphicObject categoriesFor: #totalExtent!public! !

!AlignmentGraphicObject class methodsFor!

horizontal

	^self new orientation: #horizontal!

vertical

	^self new orientation: #vertical! !
!AlignmentGraphicObject class categoriesFor: #horizontal!public! !
!AlignmentGraphicObject class categoriesFor: #vertical!public! !

BorderLayoutGraphicObject guid: (GUID fromString: '{9fc61f80-e727-11d5-b8e3-9162307a4e65}')!
BorderLayoutGraphicObject comment: ''!
!BorderLayoutGraphicObject categoriesForClass!Unclassified! !
!BorderLayoutGraphicObject methodsFor!

centerGraphic
	"Private - Answer the value of the receiver's ''centerGraphic'' instance variable."

	^centerGraphic!

centerGraphic: anObject
	"Private - Set the value of the receiver's ''centerGraphic'' instance variable to the argument, anObject."

	centerGraphic notNil ifTrue:[ self remove: centerGraphic ifAbsent:[]].
	centerGraphic := anObject.
	self add: anObject!

east
	"Private - Answer the value of the receiver's ''east'' instance variable."

	^east!

east: anObject
	"Private - Set the value of the receiver's ''east'' instance variable to the argument, anObject."

	east notNil ifTrue:[ self remove: east ifAbsent:[] ].
	east := anObject.
	self add: anObject!

hasCenter

	^self centerGraphic notNil !

hasEast

	^self east notNil !

hasNorth

	^self north notNil !

hasSouth

	^self south notNil !

hasWest

	^self west notNil !

north
	"Private - Answer the value of the receiver's ''north'' instance variable."

	^north!

north: anObject
	"Private - Set the value of the receiver's ''north'' instance variable to the argument, anObject."

	north notNil ifTrue:[ self remove: north ifAbsent:[] ].
	north := anObject.
	self add: anObject!

reorganize
	"Performs a layout operation on the contents in aContainerView"

	| top bottom left right d insets |
	insets := 0@0 corner: 0@0.
	top := self top .
	bottom := self bottom.
	left := self left.
	right := self  right.

	self hasNorth ifTrue: [
		self north extent: (right - left) @ self north height.
		d := self north extent.
		self north bounds: (left@top extent: right - left @ d y).
		top := top + d y ].
	self hasSouth ifTrue: [
		self south extent: right - left @ self south height.
		d := self south extent.
		self south bounds: (left @ (bottom - d y) extent: right - left @ d y).
		bottom := bottom - d y ].
	self hasEast ifTrue: [
		self east extent: self east width @ (bottom - top).
		d := self east extent.
		self east bounds: ((right - d x)@top extent: d x @ (bottom - top)).
		right := right - d x ].
	self hasWest ifTrue: [
		self west extent: self west width @ (bottom - top).
		d := self west extent.
		self west bounds: (left@top extent: d x @ (bottom - top)).
		left := left + d x ].
	self hasCenter ifTrue: [
		self centerGraphic bounds: (left @ top extent: right - left @ (bottom - top))].!

south
	"Private - Answer the value of the receiver's ''south'' instance variable."

	^south!

south: anObject
	"Private - Set the value of the receiver's ''south'' instance variable to the argument, anObject."

	south notNil ifTrue:[ self remove: south ifAbsent:[] ].
	south := anObject.
	self add: anObject!

west
	"Private - Answer the value of the receiver's ''west'' instance variable."

	^west!

west: anObject
	"Private - Set the value of the receiver's ''west'' instance variable to the argument, anObject."

	west notNil ifTrue:[ self remove: west ifAbsent:[] ].
	west := anObject.
	self add: anObject! !
!BorderLayoutGraphicObject categoriesFor: #centerGraphic!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #centerGraphic:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #east!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #east:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #hasCenter!public! !
!BorderLayoutGraphicObject categoriesFor: #hasEast!public! !
!BorderLayoutGraphicObject categoriesFor: #hasNorth!public! !
!BorderLayoutGraphicObject categoriesFor: #hasSouth!public! !
!BorderLayoutGraphicObject categoriesFor: #hasWest!public! !
!BorderLayoutGraphicObject categoriesFor: #north!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #north:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #reorganize!geometry!public! !
!BorderLayoutGraphicObject categoriesFor: #south!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #south:!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #west!accessing!private! !
!BorderLayoutGraphicObject categoriesFor: #west:!accessing!private! !

!BorderLayoutGraphicObject class methodsFor!

example1
	
	"DrawingShell show model add: self example1"
	^self new centerGraphic: (GraphicEllipse new);	
			north: (GraphicRectangle new);
			east: (GraphicRectangle new);
			 west: (GraphicRectangle new);
			south: (GraphicEllipse new).

!

example2
	
	"DrawingShell show model add: self example1"
	^self new centerGraphic: (GraphicEllipse new);	
			 west: (GraphicRectangle new).


!

example3
	
	"DrawingShell show model add: self example3"
	^self new centerGraphic: self example1;	
			 north: self example2;
			bottomRight: 200@200.


! !
!BorderLayoutGraphicObject class categoriesFor: #example1!public! !
!BorderLayoutGraphicObject class categoriesFor: #example2!public! !
!BorderLayoutGraphicObject class categoriesFor: #example3!public! !

ProportionalLayoutGraphicObject guid: (GUID fromString: '{c16bc921-e789-11d5-b8e3-9d717074350f}')!
ProportionalLayoutGraphicObject comment: 'DrawingShell show'!
!ProportionalLayoutGraphicObject categoriesForClass!Unclassified! !
!ProportionalLayoutGraphicObject methodsFor!

add: anObject 


	^self   add: anObject proportion: 1
!

add: anObject proportion: aProportion

	self proportions at: anObject put: aProportion.
	super  add: anObject .

^anObject!

addAll: aCollection


	aCollection do:[:aGraphicObject | self add: aGraphicObject].
	^aCollection!

bounds: aRectangle

	super bounds: aRectangle.
	self reorganize!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	col addAll: (super defaultHandlesFor: arena).
	"self subObjects do:[:i | col addAll: (i defaultHandlesFor: arena)]."
	col add: ((GraphicLeftButtonHandle for: self arena: arena ) 
			action: [:aMouseEvent |self switchOrientation] ; alignmentSelector: #topCenter).
	^col!

initialize

	super initialize.
	proportions := LookupTable new.
	horizontal := false.!

majorDimensionPoint: aNumber


	horizontal  ifTrue:[ ^aNumber @ 1].
	^1@ aNumber !

nextOriginSelector

	horizontal  ifTrue:[ ^#topRight].
	^#bottomLeft!

proportionFor: aGraphicObject

	^self majorDimensionPoint: (proportions at: aGraphicObject) !

proportions
	"Private - Answer the value of the receiver's ''proportions'' instance variable."

	^proportions!

proportions: anObject
	"Private - Set the value of the receiver's ''proportions'' instance variable to the argument, anObject."

	proportions := anObject!

remove: aGraphicObject ifAbsent: aBlock


	super remove: aGraphicObject ifAbsent: aBlock.
	proportions removeKey: aGraphicObject.

	^aGraphicObject!

reorganize
	|origin total|
	total := self totalProportions.
	origin := self origin.	
	self subObjects do:[:aGraphicObject |
		aGraphicObject 
			basicBounds: (origin extent:( (self extent * (self proportionFor: aGraphicObject)) /(self majorDimensionPoint: total) ) floor).
		origin := aGraphicObject perform: self nextOriginSelector.
		 ].
	self invalidate.!

switchOrientation


	horizontal := horizontal not.
	self reorganize!

totalProportions


	^proportions inject:0 into:[:sum :aNumber | sum + aNumber].
! !
!ProportionalLayoutGraphicObject categoriesFor: #add:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #add:proportion:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #addAll:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #bounds:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #defaultHandlesFor:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #initialize!public! !
!ProportionalLayoutGraphicObject categoriesFor: #majorDimensionPoint:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #nextOriginSelector!public! !
!ProportionalLayoutGraphicObject categoriesFor: #proportionFor:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #proportions!accessing!private! !
!ProportionalLayoutGraphicObject categoriesFor: #proportions:!accessing!private! !
!ProportionalLayoutGraphicObject categoriesFor: #remove:ifAbsent:!public! !
!ProportionalLayoutGraphicObject categoriesFor: #reorganize!public! !
!ProportionalLayoutGraphicObject categoriesFor: #switchOrientation!public! !
!ProportionalLayoutGraphicObject categoriesFor: #totalProportions!public! !

!ProportionalLayoutGraphicObject class methodsFor!

example1
	
	"DrawingShell show model add: self example1"
	^self new add: BorderLayoutGraphicObject example1 proportion: 2 ; 
			add: BorderLayoutGraphicObject example1 proportion: 1 ;
			extent: 200@200;
			yourself


! !
!ProportionalLayoutGraphicObject class categoriesFor: #example1!public! !

GraphicNormalObject guid: (GUID fromString: '{d4ace4a1-c2b4-11d5-b8e3-000021fb21b2}')!
GraphicNormalObject comment: 'DrawingShell show model add: (self new metaObject: Object)'!
!GraphicNormalObject categoriesForClass!Unclassified! !
!GraphicNormalObject methodsFor!

defaultHandlesFor: arena

	|col|
	col := OrderedCollection new.
	instVars subObjects do:[:i|  col addAll: ( i defaultHandlesFor: arena) ].
	col addAll: (super defaultHandlesFor: arena).
	^col!

metaObject: anObject


	self north: (GraphicText text: anObject basicPrintString).
	instVars := AlignmentGraphicObject  vertical .
	1 to: anObject class instSize do:[:i | 
		instVars add: (InstVarGraphic object: anObject index: i).
		].
	anObject isIndexable  ifTrue:[ 1 to: (anObject basicSize min:7)  do:[:i | 
		instVars add: (IndexedInstVarGraphic object: anObject index: i).
		].].
	self extent: instVars calculateBounds extent + self north extent.
	self centerGraphic: (instVars).


	self reorganize.
! !
!GraphicNormalObject categoriesFor: #defaultHandlesFor:!public! !
!GraphicNormalObject categoriesFor: #metaObject:!public! !

GraphicSimpleClass guid: (GUID fromString: '{c16bc922-e789-11d5-b8e3-9d717074350f}')!
GraphicSimpleClass comment: ''!
!GraphicSimpleClass categoriesForClass!Unclassified! !
!GraphicSimpleClass methodsFor!

metaObject: anObject

	self west: (GraphicImage image: anObject icon).
	self centerGraphic: (GraphicText text: anObject printString).
! !
!GraphicSimpleClass categoriesFor: #metaObject:!public! !

GraphicConnection guid: (GUID fromString: '{547ea8a1-aeb8-11d5-b8e3-000021fb21b2}')!
GraphicConnection comment: '(Shell show:''testGraph'' ) view subViews first addGraphicObject: GraphicEllipse test ; addGraphicObject: GraphicObject testa DevelopmentSessionManager


self allInstances'!
!GraphicConnection categoriesForClass!Unclassified! !
!GraphicConnection methodsFor!

bounds


	^Rectangle vertex: from center vertex: to center!

containsPoint: aPoint



	"(super containsPoint: aPoint )ifFalse:[^false]."

	^(aPoint dist: (aPoint nearestPointAlongLineFrom: from bounds center  to: to bounds center) )<= 3!

defaultHandlesFor: aGraphicView

	|col|
	col := OrderedCollection new.
"	col addAll: (super defaultHandlesFor: aGraphicView)."
	col add: ((GraphicConnectionHandle for: self arena: aGraphicView ) connectionEndSelector: #to:); 
	add: (GraphicDeleteHandle for: self arena: aGraphicView ).
	^col!

drawOn: aCanvas
	^(Lapiz canvas: aCanvas)
		penUp;
		goto: from center;
		penDown;
		arrowTo: to center!

from
	"Private - Answer the value of the receiver's ''from'' instance variable."

	^from!

from: anObject
	"Private - Set the value of the receiver's ''from'' instance variable to the argument, anObject."

	anObject when: #invalidateRectangle:  send: #invalidate to: self.
	from := anObject.
"	self invalidate."
	self trigger: #fromChanged.!

recalculateCorner


	bounds corner: to center.
!

recalculateOrigin


	bounds origin: from center.
!

to
	"Private - Answer the value of the receiver's ''to'' instance variable."

	^to!

to: anObject
	"Private - Set the value of the receiver's ''to'' instance variable to the argument, anObject."

	anObject when: #invalidateRectangle:  send: #invalidate  to: self.
	to := anObject.
	self invalidate.
	self trigger: #toChanged.! !
!GraphicConnection categoriesFor: #bounds!public! !
!GraphicConnection categoriesFor: #containsPoint:!public! !
!GraphicConnection categoriesFor: #defaultHandlesFor:!public! !
!GraphicConnection categoriesFor: #drawOn:!public! !
!GraphicConnection categoriesFor: #from!accessing!private! !
!GraphicConnection categoriesFor: #from:!accessing!private! !
!GraphicConnection categoriesFor: #recalculateCorner!public! !
!GraphicConnection categoriesFor: #recalculateOrigin!public! !
!GraphicConnection categoriesFor: #to!accessing!private! !
!GraphicConnection categoriesFor: #to:!accessing!private! !

!GraphicConnection class methodsFor!

from: aGraphicObject
to: anotherGraphicObject


	^self new from: aGraphicObject;
			to: anotherGraphicObject;
			bounds: (Rectangle vertex:aGraphicObject center vertex: anotherGraphicObject center)
! !
!GraphicConnection class categoriesFor: #from:to:!public! !

GraphicImage guid: (GUID fromString: '{6536a8e1-b09e-11d5-b8e3-000021fb21b2}')!
GraphicImage comment: ''!
!GraphicImage categoriesForClass!Unclassified! !
!GraphicImage methodsFor!

drawOn: aCanvas


	^self image drawOn: aCanvas at: self bounds origin extent: self bounds extent


!

image
	"Private - Answer the value of the receiver's ''image'' instance variable."

	^valueModel value!

image: anObject
	"Private - Set the value of the receiver's ''image'' instance variable to the argument, anObject."

	valueModel value: anObject! !
!GraphicImage categoriesFor: #drawOn:!public! !
!GraphicImage categoriesFor: #image!accessing!private! !
!GraphicImage categoriesFor: #image:!accessing!private! !

!GraphicImage class methodsFor!

image: anImage

	
	^self new image: anImage!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects layoutManager |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect image: #image).

	^aspects.!

test


	^self image: Object icon.! !
!GraphicImage class categoriesFor: #image:!public! !
!GraphicImage class categoriesFor: #publishedAspectsOfInstances!constants!development!public! !
!GraphicImage class categoriesFor: #test!public! !

GraphicText guid: (GUID fromString: '{09070681-af1f-11d5-b8e3-000021fb21b2}')!
GraphicText comment: ''!
!GraphicText categoriesForClass!Unclassified! !
!GraphicText methodsFor!

angle

	^0!

drawOn: aCanvas

	graphicAttributes installOn: aCanvas.
	aCanvas setBkMode: 1.
	^aCanvas formatText: self text in: self bounds align: #center 


!

font

	^graphicAttributes font!

font:arg1 

	^graphicAttributes font:arg1 !

text
	"Private - Answer the value of the receiver's ''text'' instance variable."

	^valueModel value!

text: anObject
	"Private - Set the value of the receiver's ''text'' instance variable to the argument, anObject."

	valueModel value: anObject!

textColor

	^graphicAttributes textColor!

textColor:arg1 

	^graphicAttributes textColor:arg1 ! !
!GraphicText categoriesFor: #angle!public! !
!GraphicText categoriesFor: #drawOn:!public! !
!GraphicText categoriesFor: #font!public! !
!GraphicText categoriesFor: #font:!public! !
!GraphicText categoriesFor: #text!accessing!private! !
!GraphicText categoriesFor: #text:!accessing!private! !
!GraphicText categoriesFor: #textColor!public! !
!GraphicText categoriesFor: #textColor:!public! !

!GraphicText class methodsFor!

defaultGraphicAttributes


	^GraphicObjectTextAttributes default!

publishedAspectsOfInstances
	"Answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects add: (Aspect string: #text).
	^aspects!

test


	^self text: 'Hola que tal'!

text: aString


	^self new text: aString! !
!GraphicText class categoriesFor: #defaultGraphicAttributes!public! !
!GraphicText class categoriesFor: #publishedAspectsOfInstances!constants!development!public! !
!GraphicText class categoriesFor: #test!public! !
!GraphicText class categoriesFor: #text:!public! !

GraphicColorHandle guid: (GUID fromString: '{e6d9dc00-b35f-11d5-b8e3-000021fb21b2}')!
GraphicColorHandle comment: ''!
!GraphicColorHandle categoriesForClass!Unclassified! !
!GraphicColorHandle methodsFor!

drawOn: aCanvas


	aCanvas brush: Brush gray ; rectangle: self bounds
!

initialize

	super initialize.
	 self alignmentSelector:#topRight.
	!

offset



	^##(10@-10)!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	ColorDialog showModalOn: (self target aspectValue: #borderColor). 
	self target invalidate.


	
! !
!GraphicColorHandle categoriesFor: #drawOn:!public! !
!GraphicColorHandle categoriesFor: #initialize!public! !
!GraphicColorHandle categoriesFor: #offset!public! !
!GraphicColorHandle categoriesFor: #onLeftButtonPressed:!public! !

GraphicDeleteHandle guid: (GUID fromString: '{6536a8e2-b09e-11d5-b8e3-000021fb21b2}')!
GraphicDeleteHandle comment: ''!
!GraphicDeleteHandle categoriesForClass!Unclassified! !
!GraphicDeleteHandle methodsFor!

drawOn: aCanvas


	^aCanvas moveTo: self bounds origin ; 
			lineTo: self bounds corner; 
			moveTo: self bounds bottomLeft; 
			lineTo: self bounds topRight.

!

offset



	^##(-10@ 20)!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	self target delete.
	self target invalidate.
	arena removeAllHandles.

	
!

targetOriginOffset



	^##(-10@ 20)! !
!GraphicDeleteHandle categoriesFor: #drawOn:!public! !
!GraphicDeleteHandle categoriesFor: #offset!public! !
!GraphicDeleteHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicDeleteHandle categoriesFor: #targetOriginOffset!public! !

GraphicLeftButtonHandle guid: (GUID fromString: '{07047ec0-b67d-11d5-b8e3-000021fb21b2}')!
GraphicLeftButtonHandle comment: ''!
!GraphicLeftButtonHandle categoriesForClass!Unclassified! !
!GraphicLeftButtonHandle methodsFor!

action
	"Private - Answer the value of the receiver's ''action'' instance variable."

	^action!

action: anObject
	"Private - Set the value of the receiver's ''action'' instance variable to the argument, anObject."

	action := anObject!

onLeftButtonPressed: aMouseEvent

	super onLeftButtonPressed: aMouseEvent.
	^action value: aMouseEvent 

	
! !
!GraphicLeftButtonHandle categoriesFor: #action!accessing!private! !
!GraphicLeftButtonHandle categoriesFor: #action:!accessing!private! !
!GraphicLeftButtonHandle categoriesFor: #onLeftButtonPressed:!public! !

!GraphicLeftButtonHandle class methodsFor!

for: aGraphicObject arena:aGraphicView



	^self basicNew 
			arena: aGraphicView ; 
			target: aGraphicObject ;
			 initialize


!

for: aGraphicObject arena:aGraphicView action: aMonadicValuable



	^(self for: aGraphicObject arena:aGraphicView ) action: aMonadicValuable


!

for: aGraphicObject arena:aGraphicView selector: aSymbol



	^(self for: aGraphicObject arena:aGraphicView ) action: (MessageSend receiver: aGraphicObject selector: aSymbol )


! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:!public! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:action:!public! !
!GraphicLeftButtonHandle class categoriesFor: #for:arena:selector:!public! !

GraphicTrackerHandle guid: (GUID fromString: '{6536a8e3-b09e-11d5-b8e3-000021fb21b2}')!
GraphicTrackerHandle comment: ''!
!GraphicTrackerHandle categoriesForClass!Unclassified! !
!GraphicTrackerHandle methodsFor!

cancelTrackingAt: arg1
	"This is an auto-generated target implementation for the protocol <mouseTrackerTarget>
	and remains to be correctly implemented."

	^Error notYetImplemented!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	self eraseTrackingAt: aPreviousPoint.
	self drawTrackingAt: aPoint.

	^aPoint

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	
	self eraseTrackingAt: aPoint.
	self invalidate.
	^aPoint!

eraseTrackingAt: aPoint


	self drawTrackingAt: aPoint.

	^aPoint

!

onLeftButtonDrag: aMouseEvent


		(tracker := MouseTracker forPresenter: arena startingAt: aMouseEvent screenPosition)
		origin: aMouseEvent position;
		startTracking: self.
!

onLeftButtonPressed: aMouseEvent


	!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	self drawTrackingAt: aPoint.
	^aPoint!

tracker
	"Private - Answer the value of the receiver's ''tracker'' instance variable."

	^tracker!

tracker: anObject
	"Private - Set the value of the receiver's ''tracker'' instance variable to the argument, anObject."

	tracker := anObject! !
!GraphicTrackerHandle categoriesFor: #cancelTrackingAt:!public! !
!GraphicTrackerHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #eraseTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #onLeftButtonDrag:!public! !
!GraphicTrackerHandle categoriesFor: #onLeftButtonPressed:!public! !
!GraphicTrackerHandle categoriesFor: #startTrackingAt:!private!tracking! !
!GraphicTrackerHandle categoriesFor: #tracker!accessing!private! !
!GraphicTrackerHandle categoriesFor: #tracker:!accessing!private! !

GraphicTrackerHandle methodProtocol: #mouseTrackerTarget attributes: #(#readOnly) selectors: #(#~~ #~= #= #== #cancelTrackingAt: #class #continueTrackingAt:from: #copy #doesNotUnderstand: #endTrackingAt: #error: #hash #identityHash #isKindOf: #isMemberOf: #isNil #notNil #perform: #perform:with: #perform:with:with: #perform:with:with:with: #perform:withArguments: #printOn: #printString #respondsTo: #startTrackingAt: #yourself)!

GraphicAspectHandle guid: (GUID fromString: '{aea81b00-b209-11d5-b8e3-000021fb21b2}')!
GraphicAspectHandle comment: ''!
!GraphicAspectHandle categoriesForClass!Unclassified! !
!GraphicAspectHandle methodsFor!

adaptor
	"Private - Answer the value of the receiver's ''adaptor'' instance variable."

	^adaptor!

adaptor: anObject
	"Private - Set the value of the receiver's ''adaptor'' instance variable to the argument, anObject."

	adaptor := anObject!

calculateBounds

	"Ignore the alignmentSelector"

	^(self adaptor value  ) + self offset - 2 extent: 5!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"

	self adaptor value: aPoint.
	self arena view invalidateRectangle: self bounds.
	
	^aPoint

!

drawOn: aCanvas

	aCanvas setDefaultAttributes.
	^aCanvas fillRectangle: self bounds brush: Brush black

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	"arena drawFocusRect: (self bounds  copy moveTo:  aPoint ) asParameter."
"	arena invalidateRect: (tracker origin corner: aPoint).
	self target drawOn: arena canvas."!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	| clone|
	self adaptor value: aPoint.
	super endTrackingAt: aPoint.


	^aPoint! !
!GraphicAspectHandle categoriesFor: #adaptor!public! !
!GraphicAspectHandle categoriesFor: #adaptor:!public! !
!GraphicAspectHandle categoriesFor: #calculateBounds!public! !
!GraphicAspectHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicAspectHandle categoriesFor: #drawOn:!public! !
!GraphicAspectHandle categoriesFor: #drawTrackingAt:!public! !
!GraphicAspectHandle categoriesFor: #endTrackingAt:!public! !

!GraphicAspectHandle class methodsFor!

for: aGraphicObject arena:aGraphicView adaptor: aValueAdaptor



	^self for: aGraphicObject arena:aGraphicView  adaptor: aValueAdaptor  positionAction: aValueAdaptor


!

for: aGraphicObject arena:aGraphicView adaptor: aValueAdaptor positionAction: anAction



	^(self for: aGraphicObject arena:aGraphicView ) adaptor: aValueAdaptor ; positionAction: anAction


!

resizeFor: aGraphicObject arena: aGraphicView 



	^(self for: aGraphicObject arena:aGraphicView ) adaptor: (aGraphicObject aspectValue:#corner).


! !
!GraphicAspectHandle class categoriesFor: #for:arena:adaptor:!public! !
!GraphicAspectHandle class categoriesFor: #for:arena:adaptor:positionAction:!public! !
!GraphicAspectHandle class categoriesFor: #resizeFor:arena:!public! !

GraphicCloneHandle guid: (GUID fromString: '{6536a8e4-b09e-11d5-b8e3-000021fb21b2}')!
GraphicCloneHandle comment: ''!
!GraphicCloneHandle categoriesForClass!Unclassified! !
!GraphicCloneHandle methodsFor!

drawOn: aCanvas


	^aCanvas polygon: (Array with: self position with: 0@4 + self position with: 4@0 + self position)

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (target  bounds copy moveBy:  aPoint - tracker origin) asParameter.!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	| clone|
	super endTrackingAt: aPoint.
	self arena model  add: (clone := self target clone translateBy:  aPoint - tracker origin ).
	self arena removeAllHandles.
	self arena addHandleTo: clone.

	^aPoint!

offset



	^##(20@-10)! !
!GraphicCloneHandle categoriesFor: #drawOn:!public! !
!GraphicCloneHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicCloneHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicCloneHandle categoriesFor: #offset!public! !

GraphicConnectHandle guid: (GUID fromString: '{547ea8a0-aeb8-11d5-b8e3-000021fb21b2}')!
GraphicConnectHandle comment: '(Shell show:''testGraph'' ) view subViews first addGraphicObject: GraphicEllipse test ; addGraphicObject: GraphicObject test'!
!GraphicConnectHandle categoriesForClass!Unclassified! !
!GraphicConnectHandle methodsFor!

drawOn: aCanvas


	^aCanvas ellipse: self bounds

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (self bounds moveTo:  aPoint ) asParameter.!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|dest|
	super endTrackingAt: aPoint.
	self arena model remove: connection. 
	dest := self arena model graphicObjectAt: aPoint.
	dest isNil ifTrue:[
				^aPoint. ].
	connection   to: dest; from: self target .
	self arena model add: connection.
	connection bringToFront.


	^aPoint!

initialize


	 self alignmentSelector:#center.
	!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

"	super startTrackingAt: aPoint."

	connection := 	self arena model add: (GraphicConnection2 new from: self target ; to: self target ).
	^aPoint! !
!GraphicConnectHandle categoriesFor: #drawOn:!public! !
!GraphicConnectHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicConnectHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicConnectHandle categoriesFor: #initialize!public! !
!GraphicConnectHandle categoriesFor: #startTrackingAt:!private!tracking! !

GraphicConnectionHandle guid: (GUID fromString: '{a8bb9280-b2fe-11d5-b8e3-000021fb21b2}')!
GraphicConnectionHandle comment: ''!
!GraphicConnectionHandle categoriesForClass!Unclassified! !
!GraphicConnectionHandle methodsFor!

calculateBounds


	^(self target perform: (connectionEndSelector copyFrom: 1 to: connectionEndSelector size -1)asSymbol )  center extent: 5!

connectionEndSelector
	"Private - Answer the value of the receiver's ''connectionEndSelector'' instance variable."

	^connectionEndSelector!

connectionEndSelector: anObject
	"Private - Set the value of the receiver's ''connectionEndSelector'' instance variable to the argument, anObject."

	connectionEndSelector := anObject!

drawOn: aCanvas


	^aCanvas ellipse: self bounds

!

drawTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	arena view drawFocusRect: (self bounds moveTo:  aPoint ) asParameter.!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."

	|dest|
	super endTrackingAt: aPoint.
	dest := self arena model graphicObjectAt: aPoint.
	dest isNil ifTrue:[^aPoint].
	self target perform: connectionEndSelector with: dest.


	^aPoint!

startTrackingAt: aPoint
	"Private - Start ink tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."

	self drawTrackingAt: aPoint.
	^aPoint! !
!GraphicConnectionHandle categoriesFor: #calculateBounds!public! !
!GraphicConnectionHandle categoriesFor: #connectionEndSelector!accessing!private! !
!GraphicConnectionHandle categoriesFor: #connectionEndSelector:!accessing!private! !
!GraphicConnectionHandle categoriesFor: #drawOn:!public! !
!GraphicConnectionHandle categoriesFor: #drawTrackingAt:!private!tracking! !
!GraphicConnectionHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicConnectionHandle categoriesFor: #startTrackingAt:!private!tracking! !

GraphicMoveHandle guid: (GUID fromString: '{7e3e11a6-ae3e-11d5-b8e3-000021fb21b2}')!
GraphicMoveHandle comment: ''!
!GraphicMoveHandle categoriesForClass!Unclassified! !
!GraphicMoveHandle methodsFor!

calculateBounds


	^target bounds!

containsPoint: aPoint

	^self target containsPoint: aPoint!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue ink tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	"Continue with the visual feedback"
	self eraseTrackingAt: aPreviousPoint.
	self drawTrackingAt: aPoint.
		self arena selection do:[:aGraphicObject |  	aGraphicObject translateBy: aPoint - aPreviousPoint].
	^aPoint

!

drawOn: ignore!

endTrackingAt: aPoint
	"Private - End selection tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."


	super endTrackingAt: aPoint.


	^aPoint!

offset



	^##(-10@ -10)!

target: anObject
	"Private - Set the value of the receiver's ''target'' instance variable to the argument, anObject."
	
"	anObject  when: #invalidateRectangle: send: #invalidate to: self."
"	anObject  when: #positionChanged send: #invalidate to: self.
	anObject  when: #sizeChanged send: #invalidate to: self."
	target := anObject.
	!

targetOriginOffset



	^##(-10@-10)! !
!GraphicMoveHandle categoriesFor: #calculateBounds!public! !
!GraphicMoveHandle categoriesFor: #containsPoint:!public! !
!GraphicMoveHandle categoriesFor: #continueTrackingAt:from:!private!tracking! !
!GraphicMoveHandle categoriesFor: #drawOn:!public! !
!GraphicMoveHandle categoriesFor: #endTrackingAt:!private!tracking! !
!GraphicMoveHandle categoriesFor: #offset!public! !
!GraphicMoveHandle categoriesFor: #target:!accessing!private! !
!GraphicMoveHandle categoriesFor: #targetOriginOffset!public! !

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

ValueBlockAdaptor guid: (GUID fromString: '{6e46bf3b-b61b-11d5-b8e3-000021fb21b2}')!
ValueBlockAdaptor comment: ''!
!ValueBlockAdaptor categoriesForClass!Unclassified! !
!ValueBlockAdaptor methodsFor!

setValue: newValue
	"Set the value of the receiver to be the <Object> newValue
	without notifying dependents of the change."

	^setValueBlock value: subject value: newValue!

value
	"Answer the <Object> value of the receiver."

	^getValueBlock value: subject! !
!ValueBlockAdaptor categoriesFor: #setValue:!accessing!public! !
!ValueBlockAdaptor categoriesFor: #value!accessing!public! !

!ValueBlockAdaptor class methodsFor!

subject: anObject getter: getBlock setter: setBlock


	^(self subject: anObject )getValueBlock: getBlock;
					   setValueBlock: setBlock ;
						yourself.! !
!ValueBlockAdaptor class categoriesFor: #subject:getter:setter:!public! !

GraphicDrawingPresenter guid: (GUID fromString: '{a6c1aac1-b5d1-11d5-b8e3-000021fb21b2}')!
GraphicDrawingPresenter comment: 'DrawingShell show'!
!GraphicDrawingPresenter categoriesForClass!Unclassified! !
!GraphicDrawingPresenter methodsFor!

addHandles: aCollection


	aCollection do:[:aGraphicObject | self addHandleTo: aGraphicObject ].!

addHandleTo: aGraphicObject



	(self handles addAll: (aGraphicObject defaultHandlesFor: self)) do:[:aHandle | aHandle invalidate].
	
	self view invalidateRect: aGraphicObject bounds!

addSelection: aGraphic


	selection add: aGraphic.
	self addHandleTo: aGraphic.
	self trigger: #selectionChanged!

alignSelection

	|group|
	self hasSelection ifFalse:[^self].
	group := ProportionalLayoutGraphicObject new.
	group addAll: (self removeSelection).
	self model add: group.
	


	!

basicSelection: aCollection

	self removeAllHandles.
	self addHandles: aCollection.
	selection := aCollection.
	self trigger: #selectionChanged.!

bringToFrontSelection


	self selection do:[:aGraphic| self model bringToFront: aGraphic].!

connectSelection

	( self selection copyFrom: 1 to: selection size -1 )do:[:aGraphicObject| 
			(self model add: 
				(GraphicConnection2 
					from: aGraphicObject 
					to:(self selection after: aGraphicObject ))) bringToFront].
	(self model add: 
		(GraphicConnection2 
			from: 	self selection last
			to:(self selection first))) bringToFront.!

cursor
	^self tool cursor!

groupSelection

	|group|
	self hasSelection ifFalse:[^self].
	group := GraphicCompositeObject new.
	group addAll: (self removeSelection).
	self model add: group.
	


	!

handleAtPoint: aPoint


^self handles detect:[:graphObject | graphObject containsPoint: aPoint] ifNone:[nil].!

handles
	"Private - Answer the value of the receiver's ''handles'' instance variable."

	^handles!

handles: anObject
	"Private - Set the value of the receiver's ''handles'' instance variable to the argument, anObject."

	handles := anObject!

hasSelection


	^selection notEmpty!

initialize

	super initialize.
	handles := OrderedCollection new.
	tool := GraphicObjectSelectionTool for: self.
	selection := OrderedCollection new.!

isSelected: aGraphic


	^selection includes: aGraphic!

onKeyPressed: aKeyEvent

	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	
	tool onKeyPressed: aKeyEvent.
	^super onKeyPressed: aKeyEvent.
!

onKeyTyped: aKeyEvent

	"Default handler for a mouse left button double-click event.
	Accept the default window processing."


	tool onKeyPressed: aKeyEvent.
	^super onKeyPressed: aKeyEvent.
!

onLeftButtonDoubleClicked: aMouseEvent
	"Default handler for a mouse left button double-click event.
	Accept the default window processing."

	|hit hitGraph hitHandle|
	super onLeftButtonDoubleClicked: aMouseEvent.
	hit := aMouseEvent position.
	
	hitHandle := self handleAtPoint: hit.
	hitHandle == nil  ifFalse:[  ^hitHandle onLeftButtonDoubleClicked: aMouseEvent ].

	hitGraph := self model graphicObjectAt: hit.
	hitGraph == nil  ifFalse:[^self  tool onLeftButtonDoubleClicked: aMouseEvent hit: hitGraph ].
	tool onLeftButtonDoubleClickedBack: aMouseEvent.
!

onLeftButtonPressed: aMouseEvent

	|hit hitGraph hitHandle|

	hit := aMouseEvent position.
	
	hitHandle := self handleAtPoint: hit.
	hitHandle == nil  ifFalse:[ (self view dragDetect: aMouseEvent) ifTrue:[ ^hitHandle onLeftButtonDrag: aMouseEvent]
										ifFalse:[ ^hitHandle onLeftButtonPressed: aMouseEvent] ].

	hitGraph := self model graphicObjectAt: hit.
	hitGraph == nil  ifFalse:[^self  tool onLeftButtonPressed: aMouseEvent hit: hitGraph ].
	tool onLeftButtonPressedBack: aMouseEvent .
	^super  onLeftButtonPressed: aMouseEvent.!

removeAllHandles

	handles do:[:i | self view  invalidateRect: i bounds. 
				i setEvents: nil].
	handles := OrderedCollection new.!

removeHandle: aHandle



	self handles remove: aHandle ifAbsent:[].
	aHandle invalidate.!

removeHandleFrom: aGraphicObject



	(self handles select:[:aHandle | aHandle target == aGraphicObject ]) do:[:aHandle | self removeHandle: aHandle ].
	
	self view invalidateRect: aGraphicObject bounds!

removeSelection

	|answer|
	selection isNil ifTrue:[^self].
	answer := self selection copy.
	selection do:[:aGraphicObject |  aGraphicObject delete].
	self removeAllHandles.
	^answer.!

removeSelection: aGraphic


	selection remove: aGraphic.
	self removeHandleFrom: aGraphic.
	self trigger: #selectionChanged.!

selectAll


	^self selection: model subObjects copy!

selection


^selection!

selection: aCollection


	self basicSelection: aCollection!

sendToBackSelection


	self selection do:[:aGraphic| self model sendToBack: aGraphic].!

setClosedPathTool


	^self tool: GraphicObjectPathCreationTool new!

setEllipseTool


	^self tool: GraphicObjectCreationTool ellipse!

setLineTool


	^self tool: GraphicObjectCreationTool line!

setOpenPathTool


	^self tool: GraphicObjectPathCreationTool new!

setPathTool


	^self tool: GraphicObjectPathCreationTool new!

setRectangleTool


	^self tool: (GraphicObjectCreationTool new newObjectPrototype: GraphicRectangle new).!

setSelectionTool


	^self tool: GraphicObjectSelectionTool new!

setTextTool
	^self tool: GraphicObjectCreationTool text!

showTree


	TreePresenter showOn: (GraphicObjectTree withRoots: self selection).!

tool
	"Private - Answer the value of the receiver's ''tool'' instance variable."

	^tool!

tool: anObject
	"Private - Set the value of the receiver's ''tool'' instance variable to the argument, anObject."

	tool := anObject.
	tool arena: self.
	self view setCursor: tool cursor! !
!GraphicDrawingPresenter categoriesFor: #addHandles:!public! !
!GraphicDrawingPresenter categoriesFor: #addHandleTo:!public! !
!GraphicDrawingPresenter categoriesFor: #addSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #alignSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #basicSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #bringToFrontSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #connectSelection!public! !
!GraphicDrawingPresenter categoriesFor: #cursor!public! !
!GraphicDrawingPresenter categoriesFor: #groupSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #handleAtPoint:!public! !
!GraphicDrawingPresenter categoriesFor: #handles!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #handles:!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #hasSelection!public! !
!GraphicDrawingPresenter categoriesFor: #initialize!initializing!public! !
!GraphicDrawingPresenter categoriesFor: #isSelected:!public! !
!GraphicDrawingPresenter categoriesFor: #onKeyPressed:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onKeyTyped:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!GraphicDrawingPresenter categoriesFor: #onLeftButtonPressed:!public! !
!GraphicDrawingPresenter categoriesFor: #removeAllHandles!public! !
!GraphicDrawingPresenter categoriesFor: #removeHandle:!public! !
!GraphicDrawingPresenter categoriesFor: #removeHandleFrom:!public! !
!GraphicDrawingPresenter categoriesFor: #removeSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #removeSelection:!public! !
!GraphicDrawingPresenter categoriesFor: #selectAll!commands!public! !
!GraphicDrawingPresenter categoriesFor: #selection!public! !
!GraphicDrawingPresenter categoriesFor: #selection:!public! !
!GraphicDrawingPresenter categoriesFor: #sendToBackSelection!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setClosedPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setEllipseTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setLineTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setOpenPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setPathTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setRectangleTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setSelectionTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #setTextTool!commands!public! !
!GraphicDrawingPresenter categoriesFor: #showTree!public! !
!GraphicDrawingPresenter categoriesFor: #tool!accessing!private! !
!GraphicDrawingPresenter categoriesFor: #tool:!accessing!private! !

!GraphicDrawingPresenter class methodsFor!

defaultModel


	^GraphicDrawing new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.GraphicObjectsView) 34 12 nil nil 34 2 8 1141047296 1 416 nil 196934 1 ##(Smalltalk.RGB) 31580641 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 8 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #setSelectionTool 8 'Selection' 1 1 nil nil nil 562 1 594 #groupSelection 8 'Group' 1 1 nil nil nil 562 1 594 #alignSelection 8 'Group with Alignment' 1 1 nil nil nil 514 nil true 34 5 562 1 594 #setRectangleTool 8 'Rectangle' 1 1 nil nil nil 562 1 594 #setClosedPathTool 8 'Closed Path' 1 1 nil nil nil 562 1 594 #setOpenPathTool 8 'Open Path' 1 1 nil nil nil 562 1 594 #setTextTool 8 'Text' 1 1 nil nil nil 562 1 594 #setEllipseTool 8 'Ellipse' 1 1 nil nil nil 8 'New' nil 1 nil nil nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 562 1 594 #showTree 8 'Show Tree' 1 1 nil nil nil 1026 4097 562 1 594 #connectSelection 8 'Connect' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 416 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 3839 21 1298 225 201 416 1250 #contextMenu: 34 1 528 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 239 7 0 0 110 0 0 0] 8 #() 1298 193 193 nil 27 )! !
!GraphicDrawingPresenter class categoriesFor: #defaultModel!public! !
!GraphicDrawingPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

GraphicObjectPenAttributePresenter guid: (GUID fromString: '{446c68c1-c11f-11d5-b8e3-000021fb21b2}')!
GraphicObjectPenAttributePresenter comment: ''!
!GraphicObjectPenAttributePresenter categoriesForClass!Unclassified! !
!GraphicObjectPenAttributePresenter methodsFor!

createComponents

	 super createComponents.
	borderColor := self add: EditableColorPresenter new name:'borderColor' .
	borderWidth := self add: NumberPresenter new name:'borderWidth' .
	borderStyle := self add: ChoicePresenter new name:'borderStyle' .
!

model: anObject 

	 super model: anObject .
	borderColor model: (self model aspectValue: #borderColor) .
	borderWidth model: (self model aspectValue: #borderWidth) .
	borderStyle model: (self model aspectValue: #borderStyle) .
! !
!GraphicObjectPenAttributePresenter categoriesFor: #createComponents!public! !
!GraphicObjectPenAttributePresenter categoriesFor: #model:!public! !

VisualInspector guid: (GUID fromString: '{aea81b01-b209-11d5-b8e3-000021fb21b2}')!
VisualInspector comment: 'No funciona.

De hecho esta clase es el proposito original por el que existe GraphicObjects, para hacer un VisualInspector, pero el proyecto paso a segundo plano, 
por ahora me concentro mas en hacer de GraphicObjects algo digno de usar, 

не работает
Фактически, этот класс является исходной целью, для которой существуют объекты GraphicObject, для создания VisualInspector, но проект произошел в фоновом режиме,
пока я больше сосредотачиваюсь на том, чтобы сделать GraphicObjects чем-то стоящим,'!
!VisualInspector categoriesForClass!Unclassified! !
!VisualInspector methodsFor!

addObject
	| str |
	str := Prompter prompt: 'Expresion'.
	self addObject: (Compiler evaluate: str logged: false)!

addObject: anObject
	^self graphicFor: anObject!

createGraphicFor: anObject
	| gr |
	^(gr := GraphicCompositeObject new)
		add: (GraphicImage new
					valueModel: (anObject aspectValue: #icon);
					bounds: (0 @ 0 extent: anObject icon extent // 2));
		add: (GraphicText new
					valueModel: (anObject aspectValue: #displayString);
					center: gr center + (0 @ (anObject icon extent y // 2));
					extent: 100 @ 30);
		metaObject: anObject;
		yourself!

expand: aGraphicObject
	| anObject |
	anObject := aGraphicObject metaObject.
	1 to: anObject class instSize + anObject basicSize
		do: 
			[:i |
			graphics addGraphicObject: (GraphicConnection from: aGraphicObject
						to: ((self addObject: (anObject instVarAt: i))
								position: aGraphicObject position + 30 + ((i * 20) @ (i \\ 4 * 20))))]!

expandSelection
	graphics selection isNil ifTrue: [^self].
	graphics selection isEmpty ifTrue: [^self].
	graphics selection do: [:i | self expand: i]!

graphicFor: anObject


	|graph|
	^objects at: anObject ifAbsentPut:[graphics addGraphicObject: (self createGraphicFor: anObject)].

!

initialize
	super initialize.
	objects := IdentityDictionary new!

onViewOpened
	super onViewOpened.
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

GraphicObjectBrushAttributePresenter guid: (GUID fromString: '{446c68c2-c11f-11d5-b8e3-000021fb21b2}')!
GraphicObjectBrushAttributePresenter comment: ''!
!GraphicObjectBrushAttributePresenter categoriesForClass!Unclassified! !
!GraphicObjectBrushAttributePresenter methodsFor!

createComponents

	 super createComponents.
	fillStyle := self add: ChoicePresenter new name:'fillStyle' .
	fillColor := self add: EditableColorPresenter new name:'fillColor' .
	fillHatch := self add: ChoicePresenter new name:'fillHatch' .
!

model: anObject 

	 super model: anObject .
	fillStyle model: (self model aspectValue: #fillStyle) .
	fillColor model: (self model aspectValue: #fillColor) .
	fillHatch model: (self model aspectValue: #fillHatch) .
! !
!GraphicObjectBrushAttributePresenter categoriesFor: #createComponents!public! !
!GraphicObjectBrushAttributePresenter categoriesFor: #model:!public! !

DrawingShell guid: (GUID fromString: '{a6c1aac2-b5d1-11d5-b8e3-000021fb21b2}')!
DrawingShell comment: ''!
!DrawingShell categoriesForClass!Unclassified! !
!DrawingShell methodsFor!

attributesWindow
	"(attributesWindow isNil or: [attributesWindow view isOpen not])
		ifTrue: 
			[attributesWindow := Shell showAsToolboxFor: self.
			attributesWindow caption: 'Propiedades'].
	^attributesWindow"!

createComponents
	super createComponents.
	drawingPresenter := self add: GraphicDrawingPresenter new name: 'drawing'!

createSchematicWiring
	super createSchematicWiring.
	drawingPresenter
		when: #selectionChanged
		send: #onDrawingSelectionChanged
		to: self!

model: aDrawing
	super model: aDrawing.
	drawingPresenter model: self model!

onDrawingSelectionChanged
	"drawingPresenter selection isEmpty ifTrue: [^self].
	self attributesWindow subPresenters isEmpty
		ifFalse: [self attributesWindow remove: self attributesWindow subPresenters first].
	drawingPresenter selection first graphicAttributes presentIn: self attributesWindow"!

showProperties
	drawingPresenter hasSelection ifFalse: [^self].
	^PublishedAspectInspector shellOn: drawingPresenter selection first! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 551 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140853548 131137 544 nil 466 496 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 32 198 66 49 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1] 328198 ##(Smalltalk.Point) 193 193 nil 544 466 496 8 4294905609 170 192 8 #() 170 192 784 138 144 784 170 176 784 nil 1 nil 706 33 33 706 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 706 1 1 706 2849 51 544 978 #updateSizePosted 784 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 144 5 0 0 25 0 0 0] 8 #() 706 193 193 nil 27 nil nil nil 410 ##(Smalltalk.ScrollingDecorator) 34 18 nil 416 34 2 8 1144061952 131073 1152 nil 786694 ##(Smalltalk.IndexedColor) 33554447 nil 7 nil nil nil 1152 1573190 1 ##(Smalltalk.ScrollingDecoratorLayout) true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 1152 34 2 8 1140916224 131073 1312 nil 196934 1 ##(Smalltalk.RGB) 16777345 nil 7 nil nil nil 1312 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.GraphicDrawingPresenter) #resource_Default_view nil 914 138 144 34 1 978 #createAt:extent: 34 2 706 1 1 706 2849 1347 1312 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 144 5 0 0 161 2 0 0] 784 1136 nil 27 8 'drawing' nil 706 1 1 true 706 17 17 914 138 144 34 1 978 #createAt:extent: 34 2 706 1 51 706 2849 1347 1152 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 144 5 0 0 186 2 0 0] 34 1 1312 1136 nil 27 170 192 784 nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #showProperties 8 'properties' 1 1 nil nil nil 8 'Object' nil 1 nil nil 23937 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 706 3839 21 706 2881 1513 416 978 #updateMenuBar 784 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 31 13 0 0 254 2 0 0] 34 3 544 1152 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140853548 131137 2192 nil 466 496 nil 519 nil 626 nil true 658 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 32 198 66 49 15 1 0 0 196 53 15 1 3 0 144 39 0 0 0 0 191 0 253 85 63 1] 706 193 193 nil 2192 466 496 8 4294905609 170 192 784 170 192 34 6 23945 853766 ##(Smalltalk.ToolbarButton) 23945 nil 2192 1 1938 #bringToFrontSelection 8 'Move First' 1 1 nil 657990 3 ##(Smalltalk.DIBSection) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Core\Contributions\IDB\Resources\ImageLibrary.bmp' nil nil 7 706 225 33 65 nil 5 23947 2418 23947 nil 2192 1 1938 #sendToBackSelection 8 'Move Last' 1 1 nil 2496 11 23949 1246982 ##(Smalltalk.ToolbarSystemButton) 23949 nil 2192 1 1938 #removeSelection 8 'Delete Item' 1 1 nil 1 11 34 3 2432 2576 2640 170 176 34 4 1 15 2496 1 nil 1 nil 706 33 33 706 45 45 nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 706 3 -3 706 139 51 2192 978 #updateSizePosted 784 2192 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 254 255 255 255 70 0 0 0 23 0 0 0] 8 #() 1136 nil 27 1136 nil 27 )! !
!DrawingShell class categoriesFor: #defaultModel!public! !
!DrawingShell class categoriesFor: #resource_Default_view!public!resources-views! !

EditableColorPresenter guid: (GUID fromString: '{446c68c3-c11f-11d5-b8e3-000021fb21b2}')!
EditableColorPresenter comment: ''!
!EditableColorPresenter categoriesForClass!Unclassified! !
!EditableColorPresenter methodsFor!

onLeftButtonDoubleClicked: aMouseEvent

	
	super onLeftButtonDoubleClicked: aMouseEvent.
	ColorDialog showModalOn: self model.! !
!EditableColorPresenter categoriesFor: #onLeftButtonDoubleClicked:!public! !

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
		ifTrue: [UserLibrary default dragDetect: self asParameter point: aMouseEvent screenPosition asParameter]
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

setCursor: aCursor
	#todo.
	"^Error notYetImplemented"!

tool
	presenter == self ifTrue: [^nil].
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
!GraphicObjectsView categoriesFor: #setCursor:!public! !
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

"Binary Globals"!

