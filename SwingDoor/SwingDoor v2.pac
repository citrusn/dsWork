| package |
package := Package name: 'SwingDoor v2'.
package paxVersion: 1;
	basicComment: 'm := TagHistory sampleCompressMathModel.
db := TagHistory sampleDbModel.

d := CompressPresenter showOn: m history.
CompressPresenter showOn: db history.
'.

package aboutBlockBytes: (ByteArray fromBase64String: 'IVNUQiA0IFIAAAAPAAAAwuXw8ej/IPEg8ODh7vL7').

package classNames
	add: #ChartPresenter;
	add: #ChartView;
	add: #CompressDataPoint;
	add: #CompressManager;
	add: #CompressManagerShell;
	add: #CompressPresenter;
	yourself.

package methodNames
	add: #Float -> #manexp;
	add: 'TagHistory class' -> #calculateMathModel;
	add: 'TagHistory class' -> #defaultConnectionDb;
	add: 'TagHistory class' -> #newDb:start:end:;
	add: 'TagHistory class' -> #sampleCompressMathModel;
	add: 'TagHistory class' -> #sampleDbModel;
	add: 'TagHistory class' -> #sampleMathModel;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Database\Database Connection Base';
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\Object Arts\Dolphin\System\Random\Dolphin Random Stream';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Slider\Dolphin Slider Control';
	add: '..\Object Arts\Dolphin\MVP\Views\SpinButton\Dolphin SpinButton Control';
	add: '..\Object Arts\Dolphin\MVP\Views\Styled Views\Dolphin Styled Views';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Tooltips\Dolphin Tooltips';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: 'IEC104';
	add: '..\Object Arts\Dolphin\IDE\Base\Internal Bitmaps and Icons';
	add: '..\Object Arts\Samples\MVP\Scribble\Scribble';
	yourself).

package!

"Class Definitions"!

Object subclass: #CompressManager
	instanceVariableNames: 'isNeedInit points corridorStartPoint lastRetainDate currentPoint prevPoint pivotU pivotL su sl u s l corridorTimeSec errorOffset'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DataPoint subclass: #CompressDataPoint
	instanceVariableNames: 'status'
	classVariableNames: 'PointColors'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #ChartPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #CompressPresenter
	instanceVariableNames: 'startIndex stopIndex stopSpin startSpin scrollPosition trend miniTrend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #CompressManagerShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StyledContainer subclass: #ChartView
	instanceVariableNames: 'zeroPoint hintBubble dateOrigin selectedPoint scaleY scaleX hbandStep trendColor pointSize penWidth selectedBorder topValue botValue trendRect'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Float methodsFor!

manexp
	" returns (mantissa:float(-1.0...1.0),exponent:integer)"

	| exp sign b x |
	x := self asFloat.
	exp := 0.
	sign := 1.
	b := 10.
	x = 0.0 ifTrue: [^Array with: 0 with: 0.0 with: 0].
	
	x < 0.0 ifTrue: [sign := -1.	x := -1 * x].
	[x < 1.0] whileTrue: [x := x * b. exp := exp - 1].
	[x > 1.0] whileTrue: [x := x / b.	exp := exp + 1].
	
	^Array with: sign with: x with: exp! !
!Float categoriesFor: #manexp!public! !

!TagHistory class methodsFor!

calculateMathModel
	| tag aValue dateStart aTimeStamp |
	dateStart := TimeStamp current asSeconds.
	tag := self new initialize.
	0 to: 79
		do: 
			[:i |
			aTimeStamp := TimeStamp fromSeconds: dateStart + (5 * i).
			aValue := (1 * (i / 3 ) sin roundTo: 0.01) * 70 roundTo: 0.01.
			tag historyAdd: aTimeStamp value: aValue
			"Transcript show: i  displayString , ' - ', aValue displayString,  ' value: ', p value displayString; cr."].
	^tag!

defaultConnectionDb
	^DBConnection new
		dsn: 'astue_ms';
		pwd: 'pw';
		connect;
		yourself!

newDb: aTag start: aDateStart end: aDateEnd
	^self new
		tagName: aTag;
		history: (self
					getHistoryTag: aTag
					start: aDateEnd
					end: aDateStart);
		yourself!

sampleCompressMathModel
	"пример данных для отображения графика косинуса от текущей даты"

	DataPointClass := CompressDataPoint.
	^self calculateMathModel!

sampleDbModel
	"
	Пример получения данных из ИАС по тегу за прошлый день. 
	TagHistory sampleDBModel
	"
	| aTag aDateStart aDateEnd |
	aTag := 'MD14PD01PS11SI01SC026.FIA30'.
	aDateStart := Date yesterday asDDMMYYYYstring.
	aDateEnd := Date today asDDMMYYYYstring.
	^self
		newDb: aTag
		start: aDateStart
		end: aDateEnd!

sampleMathModel
	"пример данных для отображения графика косинуса от текущей даты"

	DataPointClass := DataPoint.
	^self calculateMathModel! !
!TagHistory class categoriesFor: #calculateMathModel!public! !
!TagHistory class categoriesFor: #defaultConnectionDb!public! !
!TagHistory class categoriesFor: #newDb:start:end:!public! !
!TagHistory class categoriesFor: #sampleCompressMathModel!public! !
!TagHistory class categoriesFor: #sampleDbModel!public! !
!TagHistory class categoriesFor: #sampleMathModel!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

CompressManager guid: (GUID fromString: '{9d317627-98a9-4619-901b-5a87eb35e992}')!
CompressManager comment: '    ErrorOffset: Double; // погрешность
    CorridorTimeSec: Int; // максимальное время в сек, в течение которого должна быть сохранена хотябы одна точка
    LastRetainDate:  DateTime;  // временная метка последней записи данных
    CurrentPoint: CompressDataPoint; // текущая точка c прибора
    PrevPoint: CompressDataPoint; // предыдущая точка c прибора
    CorridorStartPoint: TCompressDataPoint; // текущая точка начала коридора
    U, L: CompressDataPoint; // опорные точки
    SU, SL: Double; // угловые коэффициенты наклона дверей коридора'!
!CompressManager categoriesForClass!Kernel-Objects! !
!CompressManager methodsFor!

calculateCurrentSlopes
	| su2 sl2 sd2 result |
	result := #srNone.
	sd2 := (currentPoint timestamp asMilliseconds - corridorStartPoint timestamp asMilliseconds) / 1000.
	su2 := (currentPoint value - corridorStartPoint value - self errorOffset) / sd2.
	sl2 := (currentPoint value - corridorStartPoint value + self errorOffset) / sd2.
	su2 > su ifTrue: 
			[su := su2.
			result := #srNewSUIsIsGreater].
	sl2 < sl ifTrue: 
			[sl := sl2.
			result := #srNewSLIsLess].
	su > sl ifTrue: [result := #srSUIsGreaterSL].
	^result!

corridorTimeSec
	^corridorTimeSec!

corridorTimeSec: anObject
	corridorTimeSec := anObject!

errorOffset
	^errorOffset!

errorOffset: anObject
	errorOffset := anObject!

establishPivotPoints
	" Расчет опорных точек "

	u := DataPoint new: corridorStartPoint timestamp
				value: corridorStartPoint value + self errorOffset.
	l := DataPoint new: corridorStartPoint timestamp
				value: corridorStartPoint value - self errorOffset!

initialize
	"Private - Initialize the receiver to contain default values.

	Illustrated Patterns
	Instance Initialization
	"	
	isNeedInit := true.	
	lastRetainDate := nil.
	su := 0.
	sl := 0.           !

initSlopes
	" Инициализация угловых коэффициентов дверей коридора "

	| sd |
	sd := (currentPoint timestamp asMilliseconds - corridorStartPoint timestamp asMilliseconds) / 1000.
	su := (currentPoint value - corridorStartPoint value - self errorOffset) / sd.
	sl := (currentPoint value - corridorStartPoint value + self errorOffset) / sd.
	Transcript
		show: 'initSlopes >> su: ' , su displayString , ' sl: ' , sl displayString;
		cr!

isCorridorTimeExpired
	"Возвращает True если время последней записи превосходит FCorridorTimeSec"

	^currentPoint timestamp asSeconds - lastRetainDate asSeconds >= self corridorTimeSec!

points
	^points!

points: anObject
	points := anObject!

processPoints: aTagHistory
	"Обработка  списка точек points "

	| stored cnt |
	self initialize.
	stored := 0.
	cnt := 0.
	points := aTagHistory history.
	points do: [:each || result |
			each status: #passed.
			cnt := cnt + 1.
			result := self receivePoint: each.
			result ifNotNil:  [:value |
					stored := stored + 1.					
					"Transcript show: 'Точка: ' , cnt displayString; cr."
					value status: #stored]
				"ifNil: [each status: #passed]"].
	"Transcript show: 'Сохранено точек: ' , stored displayString; cr."!

receivePoint: aCompressDataPoint
	| result slope |
	result := nil.
	prevPoint := currentPoint.
	currentPoint := aCompressDataPoint.	"CompressDataPoint new: aTimeStamp value: aValue."	
	isNeedInit
		ifTrue: 
			[corridorStartPoint := currentPoint. " первую точку устанавливаю как коридорную точку  рассчитываю опорные точки"
			self establishPivotPoints.
			isNeedInit := false.
			prevPoint := currentPoint.	"не было"			
			lastRetainDate := currentPoint timestamp.
			result := currentPoint] " текущая точка должна быть сохранена в БД"
		ifFalse: 
			[prevPoint timestamp == corridorStartPoint timestamp
				ifTrue: [self initSlopes]
				ifFalse: 
					[ slope := self calculateCurrentSlopes. " вычисляю коэффициенты наклона дверей коридора"
					"self halt."
					Transcript show: 'SLOPE- ' , slope displayString; cr.
					(#( #srNone 		" точка входит в текущий коридор"
					#srNewSUIsIsGreater " верхняя граница коридора изменилась"
					#srNewSLIsLess		" нижняя граница коридора изменилась"
					) includes: slope) ifTrue: [ self isCorridorTimeExpired 
									ifTrue: [result := currentPoint.  "Transcript show:  'timeout: ' , lastRetainDate displayString; cr."
										lastRetainDate := currentPoint timestamp.  ]]. "если время истекло, то обновляем последнюю временную метку сохранения"
					slope = #srSUIsGreaterSL " двери открылись"
						ifTrue: [" текущая точка не входит в коридор.						
							предыдущая точка открывает коридор происходит перерасчет коэффициентов"
							corridorStartPoint := prevPoint.
							self establishPivotPoints.
							self initSlopes.
							lastRetainDate := prevPoint timestamp.
							" возвращаю данные предыдущей точки для сохранения"
							"ATimeStamp := FCorridorStartPoint.X.
							AValue := FCorridorStartPoint.Y."
							"AStatus := FCorridorStartPoint.Status."
							result := prevPoint] ]].
	^result! !
!CompressManager categoriesFor: #calculateCurrentSlopes!public! !
!CompressManager categoriesFor: #corridorTimeSec!accessing!private! !
!CompressManager categoriesFor: #corridorTimeSec:!accessing!private! !
!CompressManager categoriesFor: #errorOffset!accessing!private! !
!CompressManager categoriesFor: #errorOffset:!accessing!private! !
!CompressManager categoriesFor: #establishPivotPoints!private! !
!CompressManager categoriesFor: #initialize!private! !
!CompressManager categoriesFor: #initSlopes!private! !
!CompressManager categoriesFor: #isCorridorTimeExpired!public! !
!CompressManager categoriesFor: #points!public! !
!CompressManager categoriesFor: #points:!accessing!public! !
!CompressManager categoriesFor: #processPoints:!public! !
!CompressManager categoriesFor: #receivePoint:!public! !

!CompressManager class methodsFor!

corridorTimeSec
	^55!

errorOffset
	^5!

icon
	^CompressManagerShell icon!

new
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^super new
		initialize;
		errorOffset:  self errorOffset;
		corridorTimeSec: self corridorTimeSec;
		yourself!

new: anErrorOffset corridortimeOut: aTimeSec
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^super new
		initialize;
		errorOffset: anErrorOffset;
		corridorTimeSec: aTimeSec;
		yourself! !
!CompressManager class categoriesFor: #corridorTimeSec!public! !
!CompressManager class categoriesFor: #errorOffset!public! !
!CompressManager class categoriesFor: #icon!public! !
!CompressManager class categoriesFor: #new!public! !
!CompressManager class categoriesFor: #new:corridortimeOut:!public! !

CompressDataPoint guid: (GUID fromString: '{f86d3855-3d22-4654-89f8-fc31bd95a6cb}')!
CompressDataPoint comment: ''!
!CompressDataPoint categoriesForClass!Kernel-Objects! !
!CompressDataPoint methodsFor!

color
	^self class PointColors at: self status!

copyFrom: aDataPoint
	self value: aDataPoint value.
	self timestamp: aDataPoint timeStamp
!

displayOn: target
	"Append an 'end-user suitable' textual representation of the receiver to the
	<puttableStream> argument, target.
	GUIDs are a fairly technical concept, but they do appear in the registry in a
	certain format, which we use here."	
	target nextPutAll: '[ '.
	timestamp asIECStringOn: target.
	target nextPutAll: '; '.
	(value) printOn: target decimalPlaces: 2. 
	target nextPutAll: '; ', status.
	target nextPutAll: ' ]'!

printOn: aStream
	"Append a textual representation of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		display: self;
		nextPut: $)!

status
	^status!

status: aStatus
	"self halt."
	status := aStatus!

timestamp
	^timestamp!

timestamp: anObject
	timestamp := anObject!

value
	^value!

value: anObject
	value := anObject! !
!CompressDataPoint categoriesFor: #color!public! !
!CompressDataPoint categoriesFor: #copyFrom:!public! !
!CompressDataPoint categoriesFor: #displayOn:!public! !
!CompressDataPoint categoriesFor: #printOn:!public! !
!CompressDataPoint categoriesFor: #status!accessing!public! !
!CompressDataPoint categoriesFor: #status:!accessing!public! !
!CompressDataPoint categoriesFor: #timestamp!accessing!public! !
!CompressDataPoint categoriesFor: #timestamp:!accessing!public! !
!CompressDataPoint categoriesFor: #value!accessing!public! !
!CompressDataPoint categoriesFor: #value:!accessing!public! !

!CompressDataPoint class methodsFor!

allStatuses
	^#(#entered #passed #stored)!

icon
	^CompressManagerShell icon!

initialize
	"Initialize the 'Common color constants' as defined in GdiPlusColor.h
	self initialize 
	self PointColors
	"

	"SW - Should this be named CommonColors?
	  - Original format retained in a comment for reference. 
		Is it possible to stop the RB from losing the base of a number literal?"

	(PointColors := IdentityDictionary new)
		at: #entered put: (ARGB stdColor: #white);    
		at: #passed put: (ARGB stdColor: #darkGray);
		at: #stored put: (ARGB stdColor: #darkRed)
		!

new: aTimeStamp value: aValue
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^(super new: aTimeStamp value: aValue)
		status: #entered" (self  allStatuses at: ((aValue \\2) ceiling + 1) )";
		yourself!

PointColors
	^PointColors! !
!CompressDataPoint class categoriesFor: #allStatuses!public! !
!CompressDataPoint class categoriesFor: #icon!public! !
!CompressDataPoint class categoriesFor: #initialize!public! !
!CompressDataPoint class categoriesFor: #new:value:!public! !
!CompressDataPoint class categoriesFor: #PointColors!public! !

ChartPresenter guid: (GUID fromString: '{98040bea-f92b-43af-8699-1aba520a28e2}')!
ChartPresenter comment: ''!
!ChartPresenter categoriesForClass!MVP-Presenters! !
!ChartPresenter methodsFor!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.	
	"self
		when: #borderChanged:
		send: #onBorderChanged:
		to: self."!

onBorderChanged:  aBorderPosition
	self halt.
	self parentPresenter trigger: #borderChanged: with: aBorderPosition.! !
!ChartPresenter categoriesFor: #createSchematicWiring!public! !
!ChartPresenter categoriesFor: #onBorderChanged:!public! !

!ChartPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ChartView) 34 29 nil nil 34 2 8 1140850688 131073 416 nil nil nil 7 nil nil nil 416 nil 170 192 8 #() 590342 ##(Smalltalk.Rectangle) 328198 ##(Smalltalk.Point) 1 1 546 1 1 138 144 8 #(#top #bottom #left #right) nil nil 8 #(0 0 0 0) true nil 1 546 111 251 nil nil nil 3 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 41 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 546 3839 21 546 701 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 221 8 0 0 4 1 0 0] 8 #() 546 193 193 nil 27 )! !
!ChartPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

CompressPresenter guid: (GUID fromString: '{35ec23ad-7f5c-4249-b4ab-4c4a3ab62232}')!
CompressPresenter comment: ''!
!CompressPresenter categoriesForClass!MVP-Presenters! !
!CompressPresenter methodsFor!

calcRange
	| r |
	r := (self model size / stopSpin model value * 2.0) ceiling - 1.
	scrollPosition view range: (Interval from: 1 to: r).
	scrollPosition model value: 1
	"Transcript show: 'Trend limit: ' , r displayString; cr."!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	startIndex := self add: NumberPresenter new name: 'minValue'.
	stopIndex := self add: NumberPresenter new name: 'maxValue'.
	startSpin := self add: NumberPresenter new name: 'minValueSpin'.
	stopSpin := self add: NumberPresenter new name: 'maxValueSpin'.
	scrollPosition := self add: NumberPresenter new name: 'trendPosition'.
	trend := self add: ChartPresenter new name: 'chart'.
	miniTrend := self add: ChartPresenter new name: 'miniChart'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	startIndex model: startSpin model.
	stopIndex model: stopSpin model.
	miniTrend
		when: #borderChanged:
		send: #onBorderChanged:
		to: self.
	stopSpin
		when: #valueChanged
		send: #onStopSpinChanged
		to: self.
	startSpin
		when: #valueChanged
		send: #onStartSpinChanged
		to: self.
	scrollPosition
		when: #valueChanged
		send: #onPositionChanged
		to: self!

displayOn: aStream
	"Answer the name of the receiver prepended by 'a' or 'an' as appropriate"

	aStream nextPutAll: 'График из ' , self model size displayString , ' точек'!

model: aListModel
	"Connects the receiver to aListModel. Since the receiver has the same model as the
	sketch pad (Scribble) component that it holds we pass this down to it."

	super model: aListModel.
	miniTrend model: self model .
	Transcript
		show: self class displayString , ' >> model: size ' , self model size displayString;
		cr!

onBorderChanged: aPoint
	"Transcript
		show: 'Border position: ' , aPosition displayString;
		cr."

	self setIndexTrendModel: aPoint!

onPositionChanged
	self setTrendModel!

onStartSpinChanged
	"Private "

	Transcript
		show: 'onMinSpinChanged: ' , (startSpin value / 1000.0) displayString;
		cr!

onStopSpinChanged
	"Private "

	self calcRange.
	self setTrendModel!

onViewAvailable
	| sz |
	"Transcript show: self class displayString , '>> onViewAvailable'; cr."
	sz := self model size.
	stopSpin view range: (Interval from: 10 to: sz by: 10).
	startSpin view range: (Interval from: 1 to: 100 by: 5).
	startSpin value: 1.
	stopSpin value: (sz/4) rounded.
	self calcRange.
	self setTrendModel.
	^super onViewAvailable!

printOn: aStream
	"Append, to the <puttableStream>, target, a string whose characters are a 
	the same as those which would result from sending a #printString
	message to the receiver.
	N.B. This is really intended for development use. #displayOn: and #displayString
	are complementary methods for generating strings for presentation to an
	end-user."
	aStream nextPut: $(.		
	self displayOn: aStream.
	aStream nextPut: $)!

setIndexTrendModel: aModelIndex
	"Private - self halt."
	trend model: (self model copyFrom: aModelIndex x to: aModelIndex y)!

setPosition
	"self view refreshContents"!

setTrendModel
	| s p b e |
	"self halt."
	s := stopSpin model value / 2.0.
	p := scrollPosition model value - 1.
	b := (s * p) asInteger + 1.
	e := (s * p) ceiling + stopSpin model value.
	e := e min: self model size.
	self setIndexTrendModel: b@e.
	miniTrend view selectedBorder: b @ e
	"Transcript show: trendPosition value displayString , ' start: ' , b displayString , ' stop: ' , e displayString; cr"!

startSpin
	^startSpin! !
!CompressPresenter categoriesFor: #calcRange!private! !
!CompressPresenter categoriesFor: #createComponents!public! !
!CompressPresenter categoriesFor: #createSchematicWiring!public! !
!CompressPresenter categoriesFor: #displayOn:!public! !
!CompressPresenter categoriesFor: #model:!public! !
!CompressPresenter categoriesFor: #onBorderChanged:!public! !
!CompressPresenter categoriesFor: #onPositionChanged!public! !
!CompressPresenter categoriesFor: #onStartSpinChanged!private! !
!CompressPresenter categoriesFor: #onStopSpinChanged!private! !
!CompressPresenter categoriesFor: #onViewAvailable!event handling!public! !
!CompressPresenter categoriesFor: #printOn:!public! !
!CompressPresenter categoriesFor: #setIndexTrendModel:!private! !
!CompressPresenter categoriesFor: #setPosition!public! !
!CompressPresenter categoriesFor: #setTrendModel!public! !
!CompressPresenter categoriesFor: #startSpin!accessing!public! !

!CompressPresenter class methodsFor!

defaultModel
	^OrderedCollection new!

icon

	"Generated from:
	self createIconMethod: #icon ofSize: 48@48 fromFile: 'C:\Object Arts\Dev\Dolphin6\Object Arts\Samples\MVP\Scribble\Scribble.png'.
	"
	^InternalIcon fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 6 0 0 0 87 2 249 135 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 19 122 73 68 65 84 104 67 189 90 9 112 212 117 150 198 117 44 173 153 173 218 153 170 153 173 89 119 167 166 198 221 153 113 173 93 103 119 157 218 117 145 85 113 64 142 21 65 16 84 64 240 0 33 16 146 16 146 78 231 6 66 194 153 128 128 10 200 33 138 32 247 41 32 160 220 55 68 66 72 128 4 136 228 238 187 147 244 125 252 223 126 223 251 119 172 169 162 179 229 204 84 73 213 171 95 119 39 221 253 125 191 247 125 239 189 223 47 244 234 245 61 253 51 30 126 180 200 248 237 207 91 141 199 31 174 144 94 189 238 251 158 190 246 47 255 26 99 224 192 7 35 195 70 62 17 127 236 215 33 153 50 72 140 65 143 73 244 137 71 6 255 229 159 252 61 124 130 191 119 191 135 253 127 255 84 67 243 227 255 41 93 63 255 137 24 214 17 34 125 127 37 198 227 255 56 224 187 124 189 72 241 15 196 157 243 55 126 71 246 195 161 142 226 223 132 237 25 255 33 50 242 254 239 242 222 63 249 119 244 203 60 197 63 14 52 231 255 194 113 189 224 183 119 78 164 255 151 163 247 163 75 206 247 122 64 14 255 224 126 249 250 71 247 73 248 133 127 146 248 154 215 238 26 174 162 197 134 179 112 85 220 89 244 105 220 153 191 39 238 202 251 50 238 202 63 23 119 231 94 139 185 172 223 196 220 22 71 220 99 9 196 221 217 134 225 201 20 195 155 41 226 201 22 233 176 138 209 89 250 179 164 224 68 182 220 31 245 150 246 15 121 10 71 132 221 214 49 17 135 37 37 228 202 181 4 93 214 146 128 43 175 34 236 204 93 25 180 91 62 13 56 114 246 6 29 214 99 65 91 238 165 160 35 167 46 236 200 110 15 216 10 2 254 214 220 120 103 99 174 56 234 45 114 247 194 84 57 179 250 21 185 60 230 247 114 173 87 47 57 141 176 61 120 159 24 41 255 38 98 179 138 216 45 26 134 195 34 113 135 85 226 78 115 141 49 156 211 177 78 147 168 35 13 143 211 36 238 78 69 100 136 225 201 16 241 102 97 205 255 85 114 2 222 125 63 137 120 138 36 218 97 145 168 59 91 194 206 12 9 57 102 96 77 147 144 51 83 130 120 28 180 167 73 192 54 93 252 237 153 186 6 176 134 218 179 196 223 54 67 58 155 179 197 115 103 186 180 93 155 38 85 159 191 41 27 83 250 202 186 81 79 72 245 99 63 20 223 95 63 0 217 252 84 228 204 56 145 246 233 98 180 207 16 195 150 46 113 59 193 166 75 140 171 109 6 214 76 37 16 119 226 53 68 196 150 38 81 251 84 144 75 21 195 61 13 153 192 123 93 150 127 237 33 3 71 31 10 123 10 64 32 71 35 236 70 184 50 65 128 100 0 20 31 30 180 103 41 104 37 208 6 224 237 105 0 159 41 190 150 76 233 106 202 20 247 237 12 105 171 78 145 107 7 222 144 221 185 207 201 222 204 222 114 126 206 191 139 237 200 8 137 55 165 138 180 1 8 222 99 216 176 171 246 84 128 6 80 108 132 73 130 1 2 32 99 18 32 153 12 205 134 102 194 5 240 30 100 160 35 239 201 30 8 244 186 47 236 46 138 69 72 192 155 43 17 119 174 68 93 0 175 1 34 142 44 37 16 180 17 56 35 3 224 51 196 215 134 172 180 166 131 192 116 241 52 164 137 253 122 170 52 94 124 71 110 28 124 93 110 236 125 85 90 79 143 145 224 237 183 0 60 5 49 89 226 237 83 36 110 195 174 218 0 12 17 85 18 137 44 128 68 20 36 84 62 14 16 83 57 33 51 32 18 119 165 131 0 126 230 205 234 215 163 65 195 29 37 190 136 55 15 191 148 39 49 47 164 228 205 1 145 108 72 42 7 114 202 69 22 72 0 82 106 183 64 58 200 130 102 34 29 68 64 160 37 93 58 27 211 144 133 169 226 184 153 34 206 218 20 241 214 165 72 224 238 52 128 158 132 93 159 140 120 7 59 143 32 1 72 131 187 30 133 76 34 182 12 0 231 10 128 200 64 212 65 18 220 253 68 32 35 113 188 38 110 154 57 119 88 143 4 66 158 178 80 164 35 31 192 173 137 0 9 143 5 36 172 18 113 89 33 37 16 129 249 66 246 108 196 116 9 41 153 116 16 1 137 214 233 226 107 158 6 18 83 164 227 155 84 233 250 102 154 4 241 156 187 107 56 176 251 142 183 197 176 191 13 121 140 3 152 20 16 152 2 128 169 230 142 131 72 4 155 19 181 103 96 157 129 21 187 78 89 81 62 154 13 100 0 89 48 104 102 119 254 152 164 4 234 234 234 30 12 120 22 75 164 179 16 30 96 6 152 9 84 5 200 41 138 108 68 64 36 236 206 3 17 122 2 36 156 89 32 145 33 97 124 89 8 59 23 108 131 169 219 166 136 191 101 42 2 5 0 210 138 224 231 113 128 80 19 58 38 96 125 11 49 30 49 17 160 38 131 28 178 0 112 81 221 249 76 205 64 68 201 240 53 144 33 65 37 128 207 112 194 196 110 202 40 107 98 82 2 109 109 109 63 10 184 23 75 172 43 95 98 157 69 18 67 38 76 18 36 128 240 228 130 0 178 195 76 184 64 6 190 8 129 72 24 59 23 70 122 105 242 16 42 76 8 222 8 35 34 44 133 78 236 36 204 23 167 9 1 58 238 120 19 122 30 139 231 88 29 83 1 142 1 176 148 140 238 60 165 196 128 140 212 11 221 158 160 23 96 98 72 200 240 88 50 146 18 104 104 104 248 177 207 245 1 8 0 124 103 129 73 160 131 217 192 99 47 3 187 207 12 184 41 169 108 100 2 43 9 160 90 104 224 11 72 134 187 135 222 128 47 100 21 201 2 129 76 179 130 56 161 123 103 119 22 38 152 192 97 210 40 171 13 222 27 197 123 162 232 11 17 37 131 207 71 79 208 215 80 5 99 40 32 49 100 222 64 51 51 188 249 5 61 73 232 103 29 142 21 18 239 42 5 129 18 68 161 25 36 0 73 69 59 240 97 204 136 7 85 42 225 9 86 169 8 64 82 86 81 144 209 199 218 140 240 28 175 197 220 204 0 2 107 156 117 220 5 35 59 97 104 205 0 155 21 119 25 59 15 147 154 143 249 126 110 2 170 160 29 5 4 159 73 82 74 2 159 23 199 247 26 29 150 185 73 9 212 214 214 254 157 187 125 173 196 125 115 16 51 65 100 150 196 53 19 36 1 79 160 141 211 15 17 72 73 9 104 16 116 174 120 171 211 228 234 162 129 82 93 222 95 170 43 6 202 181 197 3 241 188 191 180 28 28 163 77 209 36 192 90 158 34 246 47 135 203 245 119 159 145 234 5 125 228 90 69 63 249 102 219 240 68 6 176 203 200 6 101 71 50 17 205 2 95 99 57 181 32 248 25 232 222 30 203 210 164 4 106 106 106 126 233 108 253 72 226 126 16 240 3 188 15 209 5 34 32 17 255 214 15 72 107 162 42 69 221 168 58 168 251 95 207 121 78 110 174 126 17 218 199 78 65 46 81 23 51 132 138 2 130 230 99 150 225 60 248 34 77 170 74 251 72 243 158 23 37 218 6 51 59 39 226 231 211 148 4 101 20 117 81 82 120 159 146 152 110 102 2 18 138 48 67 42 71 72 145 30 112 91 215 36 37 112 243 230 205 71 218 26 63 18 35 80 38 134 127 46 162 196 36 208 85 8 18 185 154 133 56 203 43 187 53 100 212 244 249 107 114 165 180 31 170 15 198 13 245 132 41 155 40 179 2 146 10 220 131 221 3 81 247 229 183 229 114 225 211 18 184 243 14 164 192 209 96 18 178 49 1 160 82 149 0 129 170 31 180 121 209 212 52 52 200 168 47 204 85 179 0 223 197 61 57 155 123 36 208 116 103 157 24 193 133 136 57 34 190 82 196 28 16 41 6 129 89 166 23 208 216 8 190 97 251 40 164 126 148 130 140 64 30 81 39 119 155 32 204 198 23 75 200 38 134 29 107 249 226 85 169 89 58 16 63 167 17 209 152 92 24 35 32 37 195 249 14 50 49 89 106 222 237 111 238 184 250 135 94 72 16 208 157 199 247 161 239 68 225 137 168 3 50 134 159 12 79 238 222 30 9 220 169 91 11 240 11 68 130 243 68 144 9 241 147 196 76 17 100 194 232 68 192 7 158 170 73 82 251 254 96 149 146 134 26 26 192 177 59 81 144 137 64 62 4 78 221 55 238 26 37 183 62 30 146 144 20 119 144 59 201 166 4 67 195 204 222 202 215 228 238 86 72 10 96 105 88 149 139 238 188 217 204 232 3 142 48 74 130 37 25 97 184 115 142 244 72 224 70 205 26 145 208 92 128 159 15 18 32 192 76 248 103 131 68 177 24 12 244 136 202 153 207 72 140 149 200 139 15 68 89 227 152 161 227 6 43 18 181 143 93 162 7 236 103 198 203 141 85 47 40 153 40 134 48 211 23 36 134 198 196 17 217 53 73 90 246 189 40 14 204 74 81 23 154 30 192 145 128 169 125 122 0 13 141 43 27 28 250 2 87 18 139 185 179 206 39 37 112 245 234 213 71 170 42 63 6 129 197 8 100 192 15 2 129 18 128 7 33 128 23 95 145 220 221 62 90 156 231 49 14 160 164 198 176 251 49 14 87 154 5 60 86 179 146 136 69 124 183 167 200 215 37 207 154 178 130 116 76 35 39 86 18 164 148 220 83 165 126 117 63 9 220 70 79 64 70 8 142 187 29 102 245 209 126 194 12 32 48 173 146 64 88 103 36 78 169 89 87 123 204 192 249 51 32 16 94 10 2 75 176 251 200 2 101 20 128 140 2 144 17 252 112 117 254 32 200 169 187 42 1 16 50 65 34 221 210 209 44 0 240 165 130 62 248 66 104 185 219 35 137 158 208 93 122 249 251 49 140 5 53 229 207 0 84 138 202 195 28 219 73 130 82 162 140 186 71 11 102 192 12 29 57 156 217 183 123 36 112 244 203 4 129 240 50 16 88 104 122 129 158 8 204 150 96 147 85 110 125 244 50 136 20 170 148 98 157 248 18 142 24 10 146 50 194 110 1 168 253 244 56 105 194 24 237 191 155 41 206 11 19 197 113 238 109 248 102 178 116 221 154 170 3 33 179 21 133 244 232 145 234 242 103 225 135 20 157 247 217 172 216 208 216 137 217 201 117 68 1 104 206 90 204 10 87 149 146 35 203 222 35 129 125 123 62 65 6 42 16 31 32 32 37 250 32 0 34 144 211 157 205 175 137 255 214 12 60 46 2 1 144 232 64 105 213 170 148 40 153 144 8 189 112 165 236 89 185 84 216 71 174 127 48 88 26 247 140 82 50 13 91 71 74 253 250 161 114 117 225 0 52 185 1 42 51 18 175 94 212 23 94 192 64 135 202 196 44 68 80 235 217 201 153 133 48 140 107 130 54 229 68 9 145 16 188 210 213 35 129 109 91 54 168 7 140 240 123 166 148 130 36 97 26 186 102 217 80 211 208 126 200 9 65 18 49 61 252 144 4 116 13 63 116 212 164 200 23 47 254 70 229 96 206 76 172 74 92 77 253 51 3 238 170 84 169 93 14 18 40 155 213 139 250 97 247 57 105 130 0 124 193 12 70 248 94 148 79 245 131 141 59 143 247 51 19 144 143 153 149 44 3 23 8 127 117 15 9 140 211 255 176 113 227 167 98 132 152 1 128 15 47 55 35 88 174 114 170 89 134 43 17 63 205 13 79 160 83 211 212 241 78 204 70 232 178 218 188 0 240 82 241 211 82 251 193 32 241 84 79 4 24 236 86 2 52 13 172 143 245 247 172 24 51 6 136 253 212 219 200 206 104 188 23 85 9 29 153 29 184 97 203 72 252 236 121 169 156 245 12 58 55 51 192 115 7 72 144 8 9 224 252 193 236 128 192 15 239 33 192 89 104 227 198 141 18 233 34 1 102 224 93 4 188 64 66 193 69 82 75 2 1 122 2 89 160 185 217 228 186 138 117 106 229 144 215 118 244 77 105 252 124 172 158 220 46 230 63 45 190 187 233 24 185 121 174 198 252 164 230 166 71 248 216 42 85 115 251 203 181 37 3 36 216 10 98 58 231 112 42 157 33 55 63 28 140 30 243 191 114 33 167 15 14 74 148 18 229 67 240 56 72 225 54 131 133 129 30 145 206 69 63 77 74 96 211 166 77 226 115 163 2 133 86 36 178 208 157 137 197 82 251 222 72 0 7 25 146 208 48 251 67 188 131 103 135 2 169 89 62 24 95 2 157 98 135 3 56 228 95 41 125 78 26 161 127 86 37 54 185 176 158 35 172 210 124 100 156 220 221 245 138 84 205 199 8 193 18 203 121 9 227 118 176 105 42 102 170 1 32 223 7 36 94 84 35 71 236 38 137 48 38 83 51 27 92 225 67 111 241 35 73 9 108 222 188 89 220 237 144 12 9 4 73 130 25 120 15 158 88 14 9 189 10 208 32 23 88 148 168 76 236 15 37 137 138 148 39 117 107 135 161 254 79 51 207 9 0 22 70 52 238 25 11 83 247 5 33 142 10 121 210 89 151 42 149 37 127 144 206 91 105 104 114 67 196 91 147 42 55 215 12 69 70 254 32 215 222 29 160 30 162 127 174 150 15 68 38 233 3 158 195 173 216 24 108 0 100 20 66 38 120 238 72 122 181 66 9 109 217 178 69 108 205 48 110 2 184 132 223 7 1 146 88 134 47 124 13 32 0 62 64 79 208 216 40 175 244 68 215 108 12 124 249 18 108 201 146 203 51 159 150 214 47 199 155 213 3 122 231 201 205 247 77 186 92 154 249 63 82 57 251 25 217 251 236 47 229 202 188 126 242 213 152 223 225 249 179 82 255 201 75 210 89 159 170 191 199 102 198 121 168 249 192 104 105 220 55 166 219 176 122 238 54 203 41 78 124 36 192 170 228 178 254 119 210 12 108 221 186 85 154 110 39 76 28 162 7 16 74 96 21 198 229 121 144 9 100 164 166 6 248 16 251 196 252 196 212 10 47 208 208 222 124 105 218 63 22 250 30 36 87 43 6 73 245 226 65 114 108 252 239 228 208 136 71 165 110 253 48 57 59 189 183 180 28 121 67 190 46 131 124 56 248 105 240 178 0 243 20 76 220 85 63 77 46 23 247 213 210 249 173 100 8 26 87 57 161 118 250 128 231 111 200 206 150 255 124 82 2 219 182 109 147 91 55 0 80 119 158 29 57 81 137 180 47 44 197 4 58 81 236 103 113 197 167 36 18 217 192 212 106 224 236 192 83 92 180 163 88 73 176 177 53 127 241 186 84 206 233 11 189 143 54 205 140 131 15 163 97 251 171 114 248 229 127 145 219 159 189 140 38 247 150 180 28 26 43 119 62 67 245 153 223 31 125 2 227 57 228 22 230 88 1 189 135 212 7 38 232 16 36 20 208 21 225 204 123 233 30 2 183 110 221 250 91 18 168 169 2 48 5 207 157 231 92 196 126 0 34 28 47 66 229 114 123 195 27 216 217 161 114 251 211 177 210 184 27 149 103 215 91 210 184 115 156 212 175 27 129 215 7 227 68 134 157 47 31 0 96 227 19 29 154 147 42 14 52 122 45 3 29 163 18 93 200 125 74 193 222 221 57 82 90 143 142 19 247 215 147 76 137 160 18 133 104 218 196 76 164 205 12 154 167 116 130 240 1 47 213 148 136 35 251 222 171 21 30 234 73 224 242 121 200 131 93 88 231 33 130 134 164 180 148 154 253 64 7 61 76 169 193 230 66 233 170 203 49 227 6 170 8 46 181 120 253 18 67 6 162 184 4 136 122 112 1 224 225 177 147 183 25 172 64 204 0 87 206 60 86 177 157 158 32 215 87 12 49 103 32 0 214 107 26 222 110 232 238 155 99 68 136 187 207 251 39 94 107 2 124 144 70 110 207 70 137 205 158 124 79 6 186 9 156 58 134 70 165 187 205 160 7 88 117 18 19 106 136 131 93 226 188 160 99 55 141 108 246 4 61 122 118 98 214 193 136 193 123 165 168 183 16 4 18 55 25 200 64 200 5 105 105 57 229 157 18 42 11 234 63 27 90 24 213 134 99 52 159 135 48 70 4 33 29 189 194 36 41 72 40 152 216 125 30 89 1 28 65 73 101 103 39 37 176 125 251 118 57 124 0 93 150 29 152 224 117 148 32 96 26 150 43 64 235 128 135 231 223 246 4 188 134 206 108 248 202 18 103 232 153 218 23 244 232 169 151 0 5 122 159 100 134 217 216 66 144 83 195 142 209 144 207 27 0 78 173 51 64 138 43 181 207 219 63 5 78 2 32 165 143 25 32 70 18 54 203 172 164 4 118 236 216 33 123 118 96 214 209 145 154 19 41 51 240 199 242 97 249 4 129 80 162 140 178 39 96 220 230 57 154 37 213 240 161 164 162 185 197 208 220 162 26 230 25 154 247 73 97 250 192 73 50 22 113 87 79 67 37 234 79 51 106 231 230 77 95 56 65 0 127 115 192 238 3 52 159 131 76 224 91 240 124 156 171 247 178 248 89 69 82 2 59 119 238 148 45 155 208 97 181 15 32 116 50 101 85 162 177 19 153 80 240 144 146 30 59 121 102 96 48 11 32 130 97 79 175 99 112 102 232 190 20 51 165 4 144 30 102 32 87 26 15 140 149 170 69 131 0 156 82 66 54 18 210 225 140 19 194 185 55 168 43 111 194 65 24 77 204 52 47 214 118 244 26 148 210 0 70 140 128 205 250 97 50 2 15 237 222 189 91 62 89 143 99 36 102 33 118 95 45 163 200 130 227 236 12 140 18 35 228 198 202 81 210 122 36 5 6 198 52 202 38 166 126 48 193 11 174 99 12 172 113 148 83 231 121 116 216 213 195 205 44 128 64 216 83 136 221 204 147 107 203 135 200 237 205 175 104 57 13 234 173 30 59 107 66 58 142 124 37 21 196 1 62 200 238 139 209 65 189 160 55 226 22 128 78 120 128 247 176 237 217 155 146 77 163 15 238 217 179 71 62 92 9 2 223 214 127 14 117 11 97 178 82 169 91 55 6 166 27 34 39 39 247 150 131 47 252 179 156 152 240 36 78 94 207 163 100 14 197 160 55 12 241 18 198 141 151 112 77 50 68 238 238 126 29 179 77 142 248 238 100 107 99 187 90 62 88 174 204 127 94 60 215 210 161 127 200 198 9 176 240 129 174 144 144 185 235 148 13 13 108 142 15 221 230 213 202 195 91 112 250 1 242 33 145 64 187 101 79 82 2 251 246 237 147 101 239 66 66 90 62 153 1 14 115 148 15 231 35 234 157 59 190 8 205 165 76 108 39 83 49 254 142 197 193 101 4 174 77 0 126 233 48 92 145 12 85 18 181 239 191 36 55 214 12 151 250 143 95 65 179 154 4 15 20 67 70 179 224 131 89 0 142 108 160 34 105 48 19 186 243 144 7 229 99 199 10 240 126 91 30 130 82 201 69 100 227 143 41 212 63 129 171 124 56 27 125 149 148 192 254 253 251 101 225 2 16 160 222 181 132 210 200 244 3 43 18 203 41 131 183 21 60 102 82 251 184 67 130 145 245 34 204 135 177 162 107 46 46 135 113 183 218 53 7 157 121 142 68 113 159 20 233 152 141 152 41 97 47 8 184 103 131 68 17 98 38 192 131 8 51 0 35 147 68 208 94 160 186 15 208 184 106 94 18 97 228 32 76 18 126 100 128 43 178 115 33 233 169 236 224 193 131 241 121 243 112 51 199 57 71 207 3 221 189 128 77 141 89 128 169 253 236 9 137 57 8 250 55 48 90 199 209 11 226 40 163 49 150 210 78 146 152 45 81 70 39 193 207 145 8 125 224 5 9 122 1 217 8 185 9 190 72 130 204 130 163 8 81 8 208 69 8 16 176 21 152 160 1 214 135 29 39 96 31 74 167 31 245 223 207 191 203 181 209 200 185 248 227 103 146 127 135 15 31 238 170 168 168 192 161 134 192 217 3 18 4 88 137 248 156 114 10 154 7 28 9 224 57 36 101 248 231 3 248 124 84 158 5 88 75 1 154 183 219 101 127 68 0 51 146 23 36 52 11 0 235 134 140 220 51 1 190 24 1 208 32 18 112 20 0 124 129 248 16 220 245 46 72 167 11 64 59 91 173 210 209 130 203 180 166 66 113 55 205 22 111 115 81 83 71 75 246 113 127 91 146 42 68 62 199 143 31 111 89 181 106 21 14 53 148 79 119 16 120 130 128 146 226 72 81 142 157 175 0 120 72 72 111 179 81 125 16 49 148 208 176 167 68 186 236 51 197 213 60 91 218 26 74 164 177 110 150 220 170 45 149 235 85 37 82 117 169 76 42 207 206 147 11 167 230 203 217 19 139 229 244 241 165 114 234 216 7 114 242 216 106 57 117 98 189 156 62 185 89 206 158 222 101 156 59 123 192 126 238 204 145 75 103 207 30 223 121 254 252 249 37 85 85 149 83 174 95 191 254 4 206 146 255 255 255 177 56 119 238 220 229 207 62 251 76 110 86 207 147 59 152 74 107 171 22 72 37 102 163 115 39 23 200 241 35 243 229 208 254 82 217 183 187 84 118 109 159 43 219 54 47 148 13 31 151 201 234 85 165 178 226 253 50 152 127 158 44 94 188 64 42 202 203 101 201 146 37 178 124 249 114 89 185 114 165 172 93 187 86 54 108 216 32 28 213 81 229 226 240 89 235 145 35 71 46 28 59 118 108 215 169 83 167 150 1 96 78 101 101 229 184 43 87 174 60 135 27 242 95 99 164 121 40 169 60 190 203 139 248 144 117 144 145 240 100 182 122 245 106 5 65 48 140 165 75 151 234 243 21 43 86 232 207 214 175 95 47 60 130 114 252 0 48 227 192 129 3 238 67 135 14 213 124 245 213 87 95 156 56 113 226 163 211 167 79 151 93 188 120 49 21 159 57 188 170 170 234 201 250 250 250 95 92 186 116 233 129 239 130 227 207 254 29 164 233 247 248 178 122 236 74 236 228 201 147 130 93 10 31 61 122 212 134 181 26 160 14 1 212 250 11 23 46 204 3 144 116 92 69 142 172 174 174 126 138 215 242 120 126 239 45 193 159 141 226 79 123 227 255 1 215 34 234 28 1 241 112 252 0 0 0 0 73 69 78 68 174 66 96 130]!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 786694 ##(Smalltalk.IndexedColor) 33554457 nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 11 11 nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1149239296 131073 544 nil 482 33554457 nil 5 nil nil nil 544 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.SpinButton) 34 15 nil 544 34 2 8 1140916246 1 688 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 818 #hash 8 #() 1 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 688 nil 8 4294904011 852486 ##(Smalltalk.NullConverter) nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 89 21 1106 31 51 688 1058 #setRange: 34 1 525062 ##(Smalltalk.Interval) 3 201 3 688 1058 #udmSetAccel: 34 1 918854 1 ##(Smalltalk.StructureArray) 8 #[0 0 0 0 1 0 0 0 2 0 0 0 5 0 0 0 5 0 0 0 20 0 0 0] 7 ##(Smalltalk.UDACCEL) nil 17 688 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 0 0 0 10 0 0 0 59 0 0 0 35 0 0 0] 8 #() 1106 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 89 1410 #fixedViewLeft 31 1410 #fixedParentTop 21 1410 #fixedViewTop 51 410 ##(Smalltalk.SpinButton) 34 15 nil 544 34 2 8 1140916246 1 1488 754 nil nil 786 818 #= 8 #() 818 #hash 8 #() 1 898 928 nil 5 nil nil nil 1488 nil 8 4294904011 962 nil nil 994 138 144 34 4 1058 #createAt:extent: 34 2 1106 89 89 1106 31 51 1488 1058 #text: 34 1 8 '0' 1488 1058 #setRange: 34 1 1186 3 201 3 1488 1058 #udmSetAccel: 34 1 1250 8 #[0 0 0 0 1 0 0 0 2 0 0 0 5 0 0 0 5 0 0 0 20 0 0 0] 7 ##(Smalltalk.UDACCEL) nil 17 1488 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 0 0 0 44 0 0 0 59 0 0 0 69 0 0 0] 8 #() 1360 nil 27 1378 1424 89 1440 31 1456 89 1472 51 410 ##(Smalltalk.TextEdit) 34 16 nil 544 34 2 8 1149304832 1 2032 nil 482 33554447 nil 5 nil nil nil 2032 nil 8 4294904097 787206 ##(Smalltalk.NumberToText) nil 8 '' nil 1 994 138 144 34 2 1058 #createAt:extent: 34 2 1106 9 19 1106 17 51 2032 1058 #text: 34 1 8 '0' 2032 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 9 0 0 0 12 0 0 0 34 0 0 0] 8 #() 1360 nil 27 1378 1424 9 1440 81 1456 19 1472 51 410 ##(Smalltalk.TextEdit) 34 16 nil 544 34 2 8 1149304832 1 2400 nil 482 33554447 1106 41 41 5 nil nil nil 2400 nil 8 4294904097 2130 nil 8 '' nil 1 994 138 144 34 2 1058 #createAt:extent: 34 2 1106 9 89 1106 17 51 2400 1058 #text: 34 1 8 '0' 2400 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 44 0 0 0 12 0 0 0 69 0 0 0] 8 #() 1360 nil 27 1378 1424 9 1440 81 1456 89 1472 51 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 2768 nil nil nil 5 nil nil nil 2768 nil 8 4294903521 1180998 4 ##(Smalltalk.CommandDescription) nil 8 'Обновить' 1 1 nil nil false nil nil nil 994 138 144 34 2 1058 #createAt:extent: 34 2 1106 21 181 1106 141 51 2768 1058 #text: 34 1 8 'Обновить' 2768 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 90 0 0 0 80 0 0 0 115 0 0 0] 8 #() 1360 nil 29 1378 1424 21 1440 141 1456 181 1472 51 170 192 34 8 688 8 'maxSpin' 2032 8 'maxValue' 2400 8 'minValue' 1488 8 'minSpin' nil 994 138 144 34 1 1058 #createAt:extent: 34 2 1106 1 1 1106 191 501 544 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 95 0 0 0 250 0 0 0] 34 5 2032 688 2400 1488 2768 1360 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3376 nil nil nil 5 nil nil nil 3376 514 1 1 nil 410 ##(Smalltalk.ChartView) 34 36 nil 3376 34 2 8 1140850688 131073 3456 nil nil nil 5 nil 721670 ##(Smalltalk.GdiplusFont) nil true 2360582 ##(Smalltalk.GdiplusFontFromFontFamilyInitializer) nil nil 21 3 7 nil 3456 nil 170 192 8 #() 590342 ##(Smalltalk.Rectangle) 1106 1 1 1106 1 1 138 144 8 #(#left #right #bottom) 590598 ##(Smalltalk.StyledPen) 482 33554433 3 8 #(4 4 1 4) 1246214 ##(Smalltalk.StyledGradientBrush) 482 33554457 2096 1106 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1 1106 3842 8 0 0 0 0 0 0 224 63 3 8 #(0 0 0 0) true nil 1 1106 121 97 nil nil nil 3 3842 8 0 0 0 0 0 0 224 63 41 nil 1106 7 7 3842 8 0 0 0 0 0 0 4 64 nil 201 -199 3618 1106 1 21 1106 481 171 994 138 144 34 1 1058 #createAt:extent: 34 2 1106 1 311 1106 501 191 3456 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 155 0 0 0 250 0 0 0 250 0 0 0] 8 #() 1360 nil 27 nil nil 410 ##(Smalltalk.ChartView) 34 36 nil 3376 34 2 8 1140850688 131073 4192 nil nil nil 5 nil 3522 nil true 3554 nil nil 21 3 7 nil 4192 nil 170 192 3600 3618 1106 1 1 1106 1 1 138 144 8 #(#top #bottom #left #right) 3714 3744 3 8 #(4 4) 3778 721158 ##(Smalltalk.SystemColor) 31 482 33554447 1106 3842 8 0 0 0 0 0 0 224 63 1 1106 3842 8 0 0 0 0 0 0 224 63 3 8 #(0 0 0 0) true nil 1 1106 121 157 nil nil nil 3 3936 41 nil 1106 17 17 3842 8 0 0 0 0 0 0 4 64 nil 201 -199 3618 1106 1 21 1106 481 291 994 138 144 34 1 1058 #createAt:extent: 34 2 1106 1 1 1106 501 311 4192 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 250 0 0 0 155 0 0 0] 8 #() 1360 nil 27 170 192 34 4 4192 8 'chart' 3456 8 'miniChart' nil 994 138 144 34 1 1058 #createAt:extent: 34 2 1106 201 1 1106 501 501 3376 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 0 0 0 0 94 1 0 0 250 0 0 0] 34 2 4192 3456 1360 nil 27 170 192 3600 nil 994 138 144 34 1 1058 #createAt:extent: 34 2 1106 7679 21 1106 701 501 416 1298 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 93 16 0 0 4 1 0 0] 34 2 544 3376 1360 nil 27 )! !
!CompressPresenter class categoriesFor: #defaultModel!models!public! !
!CompressPresenter class categoriesFor: #icon!public! !
!CompressPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

CompressManagerShell guid: (GUID fromString: '{88ffdd5a-a87a-469a-b4f1-4d38a3eeaf27}')!
CompressManagerShell comment: ''!
!CompressManagerShell categoriesForClass!MVP-Presenters! !
!CompressManagerShell class methodsFor!

icon
	^InternalIcon
		fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 6 0 0 0 87 2 249 135 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 0 241 73 68 65 84 104 67 237 214 49 10 194 48 20 198 241 30 160 23 16 113 213 251 184 58 185 121 2 39 29 196 193 81 7 189 128 75 79 225 224 232 160 32 57 135 55 40 70 108 51 84 120 241 37 69 243 18 250 5 254 203 107 180 249 209 34 102 189 99 174 83 14 0 233 0 144 14 0 233 0 144 14 0 223 54 183 149 110 46 106 143 79 193 1 123 181 53 71 175 23 181 199 39 0 124 179 1 70 197 160 186 246 110 125 93 126 124 230 91 209 0 218 62 25 0 124 139 14 48 59 79 205 45 181 46 159 37 185 167 89 116 0 223 27 39 3 176 253 170 36 3 248 213 156 11 0 0 152 57 23 0 0 48 115 46 0 146 1 216 254 243 36 3 248 247 65 1 224 2 160 229 156 171 59 128 131 218 153 175 174 87 168 57 151 51 32 214 156 1 147 211 184 122 204 161 162 206 64 229 12 88 92 230 90 61 238 193 162 206 64 213 157 87 40 214 0 144 14 0 233 0 144 46 27 22 125 242 66 26 229 250 5 214 19 14 167 60 4 103 168 0 0 0 0 73 69 78 68 174 66 96 130]!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 576 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 576 466 656 8 4294908637 170 192 8 #() 170 192 34 6 60945 853766 ##(Smalltalk.ToolbarButton) 60945 nil 576 1 1180998 4 ##(Smalltalk.CommandDescription) #noCommand 8 'No command' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 60947 1115910 ##(Smalltalk.ToolbarIconButton) 60947 nil 576 1 898 #noCommand 928 1 1 263494 3 ##(Smalltalk.Icon) nil true 992 8 'Object.ico' 1040 946 nil true nil nil nil nil 3 514 33 33 1 60949 1246982 ##(Smalltalk.ToolbarSystemButton) 60949 nil 576 1 898 #noCommand 928 1 1 nil 1 17 34 4 880 1104 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 576 3 nil 1 1232 170 176 34 6 1 119 960 1 1184 117 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1569 51 576 1474 #updateSizePosted 816 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 1648 nil 466 8 4278190080 nil 7 nil 674 nil true 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 1648 nil 8 4294907927 170 192 816 34 1 853766 ##(Smalltalk.StatusBarItem) 1 -1 1648 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 1890 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 1648 nil nil 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 1 1081 514 1569 45 1648 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 28 2 0 0 16 3 0 0 50 2 0 0] 8 #() 1632 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2192 nil nil nil 7 nil nil nil 2192 546 1 1 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 2192 34 2 8 1140850688 131073 2272 nil nil nil 7 nil nil nil 2272 nil 170 192 816 nil 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 1 851 514 1569 181 2272 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 169 1 0 0 16 3 0 0 3 2 0 0] 34 2 410 ##(Smalltalk.PushButton) 34 20 nil 2272 34 2 8 1140924416 1 2512 nil nil nil 7 nil nil nil 2512 nil 8 4294907733 898 nil 8 'Новая точка' 1 1 nil nil false nil nil nil 1410 138 144 34 2 1474 #createAt:extent: 34 2 514 15 21 514 181 51 2512 1474 #text: 34 1 8 'Новая точка' 2512 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 10 0 0 0 97 0 0 0 35 0 0 0] 8 #() 1632 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 2272 34 2 8 1140924416 1 2832 nil nil nil 7 nil nil nil 2832 nil 8 4294907733 898 nil 8 'Очистить' 1 1 nil nil false nil nil nil 1410 138 144 34 2 1474 #createAt:extent: 34 2 514 25 81 514 171 51 2832 1474 #text: 34 1 8 'Очистить' 2832 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 40 0 0 0 97 0 0 0 65 0 0 0] 8 #() 1632 nil 29 1632 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 2192 34 2 8 1140850688 131073 3152 nil nil nil 7 nil nil nil 3152 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ChartView) 34 29 nil 3152 34 2 8 1149239296 1 3280 590662 2 ##(Smalltalk.ListModel) 138 144 816 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 196934 1 ##(Smalltalk.RGB) 16908287 nil 7 nil nil nil 3280 nil nil nil nil nil nil 8 #(0 0 0 0) true nil 1 514 111 425 nil nil nil 3 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 41 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 1 1 514 1177 851 3280 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 76 2 0 0 169 1 0 0] 8 #() 1632 nil 27 7 false 170 192 816 nil 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 1 1 514 1569 851 3152 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 169 1 0 0] 34 2 3280 410 ##(Smalltalk.RichTextEdit) 34 18 nil 3152 34 2 8 1149309252 1 3856 nil 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 10 984134 2 ##(Smalltalk.CommandMenuItem) 1 898 #chooseSelectionFont 8 '&Font...' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 4002 1 898 #bePlain 8 '&Plain' 1 1 nil nil nil 4002 1 898 #toggleBold 8 '&Bold' 1 1 nil nil nil 4002 1 898 #toggleItalic 8 '&Italic' 1 1 nil nil nil 4002 1 898 #toggleUnderlined 8 '&Underlined' 1 1 nil nil nil 4066 4097 3954 nil true 34 3 4002 1025 898 #alignParagraphLeft 8 '&Left' 1 1 nil nil nil 4002 1025 898 #alignParagraphCenter 8 '&Centre' 1 1 nil nil nil 4002 1025 898 #alignParagraphRight 8 '&Right' 1 1 nil nil nil 8 '&Align' nil 1 nil nil nil nil nil 4066 4097 4002 1 898 #chooseSelectionColor 8 '&Colour...' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 3856 nil 8 1769636347 852486 ##(Smalltalk.NullConverter) nil nil 9 nil 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 32 0 173 1] 1410 138 144 34 4 1474 #createAt:extent: 34 2 514 1177 1 514 393 851 3856 1474 #contextMenu: 34 1 3968 3856 1474 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 3856 1474 #resetCharFormat 816 3856 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 2 0 0 0 0 0 0 16 3 0 0 169 1 0 0] 8 #() 1632 nil 27 1632 nil 27 170 192 816 nil 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 1 51 514 1569 1031 2192 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 28 2 0 0] 34 2 3152 2272 1632 nil 27 170 192 816 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1410 138 144 34 3 1474 #createAt:extent: 34 2 514 3839 21 514 1601 1201 416 1474 #text: 34 1 8 'Вращающаяся дверь' 416 1474 #updateMenuBar 816 416 1570 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 159 10 0 0 98 2 0 0] 34 4 576 2192 1648 410 ##(Smalltalk.StyledContainer) 34 22 nil 416 34 2 8 1174405120 131073 5376 nil nil nil 7 nil nil nil 5376 nil 170 192 816 nil 138 144 8 #(#bottom #left #right #top) 590598 ##(Smalltalk.StyledPen) 786694 ##(Smalltalk.IndexedColor) 33554433 3 8 #(2 2) 1246214 ##(Smalltalk.StyledGradientBrush) 721158 ##(Smalltalk.SystemColor) 31 nil 514 1 3490 8 0 0 0 0 0 0 224 63 514 3 3490 8 0 0 0 0 0 0 224 63 8 #(0 0 0 0) true nil 1 1410 138 144 34 1 1474 #createAt:extent: 34 2 514 501 159 514 751 491 5376 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 250 0 0 0 79 0 0 0 113 2 0 0 68 1 0 0] 34 1 410 ##(Smalltalk.Slider) 34 18 nil 5376 34 2 8 1140916225 1 5872 721990 2 ##(Smalltalk.ValueHolder) nil false 1376774 ##(Smalltalk.PluggableSearchPolicy) 1890 #= 8 #() 1890 #hash 8 #() 1 nil nil 7 nil nil nil 5872 nil 8 4294904153 4594 nil nil 21 nil nil 1410 138 144 34 2 1474 #createAt:extent: 34 2 514 121 271 514 501 71 5872 1474 #range: 34 1 525062 ##(Smalltalk.Interval) 1 201 21 5872 1570 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 135 0 0 0 54 1 0 0 170 0 0 0] 8 #() 1632 nil 27 1632 nil 27 1632 nil 27)! !
!CompressManagerShell class categoriesFor: #icon!public! !
!CompressManagerShell class categoriesFor: #resource_Default_view!public!resources-views! !

ChartView guid: (GUID fromString: '{53002956-3458-404a-ac62-bdd46fc05d4c}')!
ChartView comment: ''!
!ChartView categoriesForClass!MVP-Resources-Misc! !
!ChartView methodsFor!

adjustRectangle: aRectangle
	self setZeroPoint: aRectangle extent.
	^super adjustRectangle: aRectangle!

brushPoint: aPointIndex
	| color1 color2 |
	color1 := self trendColorAsRGB.
	color2 := (self history at: aPointIndex) color.
	^GdiplusSolidBrush color: (color2 ifNil: [color1])!

cancelTrackingAt: aPoint 
	!

continueTrackingAt: aPoint from: aPreviousPoint
	"Private - Continue position tracking for the associated view at aPoint when
	the previous tracking position was at aPreviousPoint. Part of the <MouseTracker>
	target protocol. Answers the actual position achieved"

	self setBorderModel: aPoint.
	^aPoint!

cursor
	^Cursor cross!

dateOrigin	
	^dateOrigin!

dateOrigin: anObject	
	dateOrigin := anObject
	"Transcript	show: 'dateStart: ' , dateStart displayString; cr"!

defaultAxisColorAsRGB
	^ARGB stdColor: #darkGray!

defaultAxisPenWidth
	^2!

defaultMinHBand
 ^ 15!

defaultNumberHBand
 ^ 5!

defaultPointSize
	^8 @ 8!

drawAxis: aCanvas
	| offset step marginX |
	marginX := zeroPoint x // 2.
	aCanvas
		pen: (Pen color: Color darkGray);
		lineFrom: marginX @ zeroPoint y to: self width @ zeroPoint y;
		lineFrom: zeroPoint x @ 0 to: zeroPoint x @ self height;		
		text: '0' at: (marginX//2)@(zeroPoint y - 8).
	step := 1.
	
	[offset := (20 * self scaleY * step) rounded.
	step := step + 1.
	aCanvas
		lineFrom: (2.5*marginX) @ (zeroPoint y + offset) to: (self width-marginX) @ (zeroPoint y + offset);
		text: (offset//self scaleY ) displayString at: (marginX//2)@(zeroPoint y - offset-8);
		lineFrom: (2.5*marginX) @ (zeroPoint y - offset) to: (self width-marginX)  @ (zeroPoint y - offset);
		text: (-1*offset//self scaleY ) displayString at: (marginX//2)@(zeroPoint y + offset-8).
	zeroPoint y - offset > 0]
			whileTrue!

drawAxisAtGr: graphics
	| posY step marginX |
	marginX := zeroPoint x // 2.
	self
		drawAxisLineFrom: (1.7* marginX) rounded@ zeroPoint y
		to: (self width - (1* marginX)) rounded @ zeroPoint y
		on: graphics.
	self
		drawAxisLineFrom: zeroPoint x rounded @ 0
		to: zeroPoint x rounded @ self height
		on: graphics.
	"self	drawString: '0' at: (marginX // 4) @ (zeroPoint y - 8) on: graphics. step := topValue - hbandStep."
	"self halt."
	step := topValue.
	
	[posY := self scaleValueToPixel: step.
	"Transcript show: step displayString; cr."
	step >= botValue] whileTrue: 
				[self
					drawGridLineFrom: (1.7 * marginX) rounded @ posY
					to: (self width - marginX) @ posY
					on: graphics.
				self
					drawString: step rounded displayString
					at: (1.6 * marginX) @ (posY - 8)
					on: graphics.
				step := step - hbandStep]!

drawAxisLineFrom: fromPoint to: toPoint on: aGraphics
	| pen |
	pen := GdiplusPen color: self defaultAxisColorAsRGB width: self defaultAxisPenWidth.
	aGraphics
		drawLineFrom: fromPoint
		to: toPoint
		pen: pen!

drawCompressedModelAtGr: graphics
	| pen polygonPoints |
	selectedBorder ifNotNil: [^ self].
	polygonPoints := self history select: [:each | each status = #stored].
	polygonPoints := polygonPoints collect: [:each | self scaleDataPointToPixel: each].
	polygonPoints size > 1
		ifTrue: 
			[pen := GdiplusPen color: (ARGB alpha: 255 red: 34 green: 193 blue: 203) width: self penWidth.
			graphics
				smoothingMode: SmoothingModeAntiAlias;
				drawCurve: polygonPoints
					pen: pen
					tension: 0.01]!

drawDataPoint: aPoint on: aCanvas
	"Private - aPosition := (self calcTimestampToPxel: aDataPoint timestamp)@ (self calcValueToPixel: aDataPoint value)."

	"self drawDataPoint: aPosition stat."

	| solid |
	solid := aPoint x odd or: [aPoint y even].
	self
		drawDataPoint: aPoint
		on: aCanvas 
		state: (solid ifTrue: [#solid] ifFalse: [#transporent])!

drawDataPoint: aPoint on: aCanvas state: aState
	| pen brush |	
	pen := Pen color: self trendColor.
	brush := aState = #transporent
				ifTrue: [Color white brush]
				ifFalse: [aState = #solid ifTrue: [self trendColor brush] ifFalse: [Color green brush]].
	aCanvas
		pen: pen;
		brush: brush;
		ellipse: (Rectangle center: aPoint extent: self  pointSize )!

drawGridLineFrom: fromPoint to: toPoint on: aGraphics
	| pen |
	pen := GdiplusPen color: self defaultAxisColorAsRGB width: self defaultAxisPenWidth / 2.
	aGraphics
		drawLineFrom: fromPoint
		to: toPoint
		pen: pen!

drawLine:aPoint on: aCanvas 
	"Private - Draw the receiver on aCanvas"

	aCanvas
		pen: (Pen 
			withStyle: Win32Constants.PS_SOLID
			width: 2
			color: self trendColor).
	aCanvas lineTo: aPoint!

drawLineFrom: fromPoint to: toPoint pen: aGdiplusPen on: aGraphics
	"Private - Draw the receiver on GdiplusGraphics"

	"aCanvas  pen: (Pen withStyle: Win32Constants.PS_SOLID width: 2 color: self trendColor).
	aCanvas lineTo: aPoint"

	aGraphics
		drawLineFrom: fromPoint
		to: toPoint
		pen: aGdiplusPen!

drawModel: aCanvas
	aCanvas moveTo: (self scaleDataPointToPixel: self model first).
	self model do: 
			[:each |
			| aPoint |
			aPoint := self scaleDataPointToPixel: each.
			self drawDataPoint: aPoint on: aCanvas.
			self drawLine: aPoint on: aCanvas]!

drawModelAtGr: graphics
	"Private -  рисование данных модели с GDI+"

	| pen polygonPoints |
	"polygonPoints := OrderedCollection new."
	polygonPoints := self history collect: [:each | self scaleDataPointToPixel: each].
	pen := GdiplusPen color: self trendColorAsRGB width: self penWidth.
	graphics
		smoothingMode: SmoothingModeAntiAlias;
		drawCurve: polygonPoints
			pen: pen
			tension: 0.1.
	polygonPoints inject: 1
		into: 
			[:i :aPoint |
			graphics fillEllipse: (Rectangle center: aPoint extent: self pointSize) brush: (self brushPoint: i).
			i + 1].
	self drawCompressedModelAtGr: graphics!

drawSelectedBorder: graphics
	"selectedRegion - Point,  где X левая граница, Y правая граница. Индексы в данных модели"
	| rect |
	selectedBorder ifNotNil: 
			[:value |
			"Transcript show: 'selectedRegion: ' , value displayString; cr."
			"self halt."
			rect := self drawSelectedRect.
			
			[graphics compositingQuality: CompositingQualityGammaCorrected.
			"Draw a multicolored background."
			graphics fillRectangle: rect
				brush: (GdiplusSolidBrush color: (ARGB alpha: 50 red: 255 green: 0 blue: 0)).
			graphics drawRectangle: rect
				pen: (GdiplusPen color: (ARGB alpha: 200 red: 255 green: 0 blue: 0))]
					ensure: [graphics compositingQuality: CompositingModeSourceCopy]].
	^self!

drawSelectedRect
	"selectedBorder - Point,  где X левая граница, Y правая граница. Индексы в данных модели"

	| x1 x2 result |
	result := nil.
	selectedBorder
		ifNotNil: 
			[:value |
			x1 := self scaleModelTimestampToPixel: value x.
			x2 := self scaleModelTimestampToPixel: value y.
			x2 - x1 < 3 ifTrue: [x2 := x1 + 3].
			result := Rectangle origin: x1 @ trendRect origin y corner: x2 @ trendRect corner y].
	^result!

drawString: aText at: aPoint on: aGraphics
	"aGraphics drawString: aText at: aPoint"
	"(Font name: 'Segoi UI' pointSize: 10) characterSet: 204"

	font := GdiplusFont
				name: 'Segoe UI'
				emSize: 10
				style: FontStyleBold
				unit: UnitPoint.
	aGraphics
		drawString: aText
		font: font
		at: aPoint
		format:  (GdiplusStringFormat  new alignment:  StringAlignmentFar)
		brush: nil!

drawTimeLabel: aGraphics
	| y align middle|
	self isShowTimeLabel ifFalse: [^self].
	font := self timeLabelFont.
	y := zeroPoint y - (font height * 2) - 2.
	middle := (self history  at: self history size /2 ) timestamp.
	align := [:param | GdiplusStringFormat new alignment: param].
	aGraphics
		drawString: (self timestampToTimeLabel: self history first timestamp)
		font: font 	at: zeroPoint x @ y
		format: (align value: StringAlignmentNear)	brush: nil.
	aGraphics
		drawString: (self timestampToTimeLabel: self history last timestamp)
		font: font at: trendRect width @ y
		format: (align value: StringAlignmentFar) brush: nil.
	aGraphics
		drawString: (self timestampToTimeLabel: middle)
		font: font at:  (self scaleTimestampToPxel: middle) @ y
		format: (align value: StringAlignmentCenter) brush: nil!

endTrackingAt: aPoint 
	"Private - End position tracking for the new position of the receiver.
	Part of the <MouseTracker> target protocol."!

eraseBackground: aCanvas
	"aCanvas erase"

	aCanvas erase: (Rectangle origin: 0 @ 0 corner: self width @ self height)
		color: (Color
				red: 226
				green: 226
				blue: 226)!

findNearestPointAt: aPoint
	" ближайщая точка к указанным координатам"

	| pointMin point delta deltaMin size i |
	size := self model size.
	size > 0 ifFalse: [^nil].
	"mills := (self calcPixelToTimesstamp: aPoint x) asMilliseconds."
	pointMin := self model first.
	deltaMin := aPoint dist: (self scaleDataPointToPixel: pointMin).
	i := 2.
	[i <= size] whileTrue: 
			[point := self model at: i.
			i := i + 1.
			delta := aPoint dist: (self scaleDataPointToPixel: point).
			"Transcript show: 'delta- ' , delta displayString , ' deltaMin- ' , deltaMin displayString; cr. "
			deltaMin > delta
				ifTrue: 
					[deltaMin := delta.
					pointMin := point]	"ifFalse: [^pointMin]"].
	deltaMin < 40 ifTrue: [^pointMin] ifFalse: [^nil]!

getTextAt: aDataPoint
	| stream |
	stream := String writeStream: 32.
	aDataPoint timestamp asIECStringOn: stream.
	stream nextPutAll:  String lineDelimiter.
	(aDataPoint value) printOn: stream decimalPlaces: 2.
	
	^stream contents
!

history
	^self model!

initialize	
	super initialize.
	Transcript show: self class printString , ' >> initialize '; cr.

	!

isShowTimeLabel
		^topValue-botValue> hbandStep
!

margintY
	^10!

margintZeroX
	^60!

onLeftButtonDoubleClicked: aMouseEvent
	| x2 x1 |
	x1 := self scaleModelTimestampToPixel: selectedBorder x.
	x2 := self scaleModelTimestampToPixel: selectedBorder y.
	self setBorderModel: (aMouseEvent position x - ((x2 - x1) / 2) -zeroPoint x) ceiling @ 0!

onLeftButtonPressed: aMouseEvent
	"Start tracking the mouse with the ultimate aim of resizing the views
	surrounding the receiver."

	selectedBorder ifNil: [^true].
	(self drawSelectedRect containsPoint: aMouseEvent position)
		ifTrue: 
			[self setMouseTrackerAt: aMouseEvent.
			^false]
		ifFalse: [^true]!

onModelChanged
	"Transcript show: self class displayString , ' >> onModelChanged'; cr."
	^super onModelChanged!

onMouseHovering: aMouseEvent
	self showSelectedPointAt: aMouseEvent  position.
	"Transcript show: 'onMouseHovering: ', (x@y) displayString;	cr."
	^super onMouseHovering: aMouseEvent!

onMouseMoved: aMouseEvent
	"hintBubble	ifNotNil: 	[:value | hintBubble close.	hintBubble := nil].	Transcript show: 'onMouseMoved'."
	"aMouseEvent window trackMouseHover: 1500"
	"self hasHotTracking ifTrue: [self trackMouseHover: -1]."
	"self trackMouseHover: 900."
	aMouseEvent window trackMouseHover: 900.
	^super onMouseMoved: aMouseEvent!

onPaintRequired: aPaintEvent
	"| canvas |"

	"Transcript show: self class displayString , '>> onPaintRequired:'; cr."
	super onPaintRequired: aPaintEvent!

onViewCreated
	"Transcript show: self class displayString , ' >> onViewCreated'; cr."

	hintBubble := nil.
	selectedPoint := nil.
	self setZeroPoint: self extent.
	scaleY := 1.
	scaleX := 0.5.
	topValue := 100.
	botValue := -1 * topValue.
	hbandStep := 20.
	selectedBorder := nil!

paintStyledEdgesOn: graphics
	super paintStyledEdgesOn: graphics.
	"Transcript show: self class displayString , ' >> paintStyledEdgesOn: graphics'; cr."
	self scale.
	"рисуем оси и текст с ними рядом"
	self drawAxisAtGr: graphics.
	"линии и точки данных"
	(self model isNil or: [self model isEmpty])
		ifFalse: 
			[self drawModelAtGr: graphics.
			self drawSelectedBorder: graphics.
			self drawTimeLabel: graphics].
	^self!

penWidth
	^penWidth ifNil: [penWidth := 2.5]
!

penWidth: anObject
	penWidth := anObject!

pointSize
	pointSize ifNil: [pointSize := self  defaultPointSize ].
	^pointSize!

pointSize: anObject
	pointSize := anObject!

scale
	| range manexp dv bdv |
	(self history isNil or: [self history isEmpty]) ifTrue: [^self].
	dateOrigin := self history first timestamp asSeconds.
	range := self valueMinMax.
	scaleX := (trendRect width - zeroPoint x)
				/ (self history last timestamp asSeconds - dateOrigin) asFloat.
	manexp := (range second - range first) manexp.
	"ndigits := manexp third > 0 ifTrue: [0] ifFalse: [manexp third * -1 + 1]. 	nbands := (manexp second * 10) asInteger + 1. "
	"nbands > 5 ifTrue: [ nbands := nbands // 2+1. dv := 0.2]. "
	
	dv := 0.05. "(self  presenter parentPresenter startSpin value / 1000.0) asFloat."
	bdv := dv * (10 raisedToInteger: manexp third).
	topValue := (range second / bdv) ceiling * bdv.
	botValue := (range first / bdv) floor * bdv.
	"self halt."
	hbandStep := topValue - botValue /  bdv/2." topValue / self defaultNumberHBand."	
	"знаки границ разные " 
	"topValue sign ~~  botValue sign ifTrue: [ 
		hbandStep >  (self defaultNumberHBand * 2) ifTrue: [bdv := bdv*2 ]
	] ifFalse: ["
	"self halt."
	hbandStep >  (self defaultNumberHBand )  ifTrue: [bdv := bdv * 2.0 "(hbandStep/ self defaultNumberHBand) rounded "].
		
	hbandStep := bdv.
	trendRect height / self defaultNumberHBand < self defaultMinHBand
		ifTrue: [hbandStep := topValue - botValue].
	Transcript
		show: 'calculateScale topValue: ' , topValue displayString , ' maxValue: '
					, range second displayString , ' hbandStep: '
					, hbandStep displayString; cr.
	"botValue := topValue.
	[botValue <= range first] whileFalse: 
			[label = String.printf(.*f,ndigits,v). hbands.push(this.createHBand(label))."
			"botValue := botValue - bdv]."
	scaleY := trendRect height / (topValue - botValue).
	zeroPoint y: (scaleY * topValue + self margintY) asInteger!

scaleDataPointToPixel: aDataPoint
	| pixel |
	pixel := (self scaleTimestampToPxel: aDataPoint timestamp)
				@ (self scaleValueToPixel: aDataPoint value).
	"Transcript show: 'Datapoint = ' , aDataPoint displayString , ' -> Pixel = ' , pixel displayString; cr."
	^pixel!

scaleModelTimestampToPixel: anModelIndex
	^self scaleTimestampToPxel: (self model at: anModelIndex) timestamp!

scalePixelToDataPoint: aPoint
	^(self scalePixelToTimesstamp: aPoint x) @ (self scalePixelToValue: aPoint y)!

scalePixelToTimesstamp: aX
	^TimeStamp fromSeconds: (aX - zeroPoint x) // self scaleX + self dateOrigin!

scalePixelToValue: anY
	^(zeroPoint y - (anY / self scaleY)) rounded!

scaleTimestampToPxel: aTimestamp
	"одна секунда - один пиксель"

	^((aTimestamp asSeconds - self dateOrigin) * self scaleX + zeroPoint x) rounded!

scaleValueToPixel: aValue
	"1.0 в числе равен 1 пикселю"

	^(zeroPoint y - (aValue * self scaleY)) rounded!

scaleX
	^scaleX!

scaleY
	^scaleY!

selectedBorder
	^selectedBorder!

selectedBorder: aPoint
	| range size |
	range := aPoint y - aPoint x.
	size := self history size.
	selectedBorder := aPoint.
	aPoint x < 1 ifTrue: [selectedBorder := 1 @ (1 + range)].
	aPoint y > size ifTrue: [selectedBorder := (size - range) @ size].
	"Transcript show: 'selectedRegion: aPoint: ' , aPoint printString , ' ' , selectedRegion printString; cr."
	self refreshContents!

setBorderModel: aPoint
	| x position |
	x := (aPoint x * self history size / trendRect width) rounded.
	position := x @ (x + selectedBorder y - selectedBorder x).
	Transcript
		show: 'new position: ' , aPoint displayString , ' offset: ' , x printString , ' border: '
					, selectedBorder printString;
		cr.
	self selectedBorder: position.
	self presenter trigger: #borderChanged: with: self selectedBorder!

setMouseTrackerAt: aMouseEvent
	| origin start |
	self setFocus.
	start := aMouseEvent screenPosition.
	origin := ((self scaleModelTimestampToPixel: selectedBorder x) - zeroPoint x)
				@ aMouseEvent position y.	"self mapPoint: zeroPoint to: View desktop."
	"Transcript show: ' start: ' , start displayString , ' origin: ' , origin displayString; cr."
	(MouseTracker forPresenter: self presenter startingAt: start)
		origin: origin;
		startTracking: self;
		beHorizontalOnly!

setZeroPoint: anExtent
	"одна секунда - один пиксель"

	trendRect := 0 @ self margintY corner: anExtent x @ (anExtent y - self margintY).
	zeroPoint := self margintZeroX @ (trendRect height / 2.0 + (self margintY * 1)) rounded.
	trendRect origin x: zeroPoint x
	"Transcript show: 'anExtent: ' , anExtent displayString , ' trendRect: ' , trendRect displayString
					, ' zeroPoint: ' , zeroPoint displayString; cr"!

showBubbleAt: aPosition  text: aText
	hintBubble	ifNil: 
			["Transcript show: 'hintBubble create x: ' , aX displayString , ' y: ' , aY displayString; cr."
			hintBubble := (MessageBubble new)
				caption: ' Выделенная точка:';
				font: ((Font name: 'Segoi UI' pointSize: 10) characterSet: 204);
				maxWidth: 200; willFade: false;	isBalloon: false;	timeout: 1100; yourself ].
	hintBubble tooltip isNil ifFalse: [hintBubble close].
	hintBubble
		position: aPosition "(self mapPoint: (aX + 10) @ (aY + 10) to: View desktop)";
		"text: (self getTextAt: aX @ aY);"
		text: aText ;
		open!

showSelectedPointAt: aPoint
	| datapoint pos canvas |
	datapoint := self findNearestPointAt: aPoint.
	"Transcript show: 'nearest: ' , datapoint displayString;	cr."
	datapoint isNil ifTrue: [^self].
	canvas := self canvas.
	selectedPoint isNil
		ifFalse: [self drawDataPoint: (self scaleDataPointToPixel: selectedPoint) on: canvas].
	selectedPoint := datapoint.
	pos := self scaleDataPointToPixel: selectedPoint.
	self
		drawDataPoint: pos
		on: canvas
		state: #selected.
	self showBubbleAt: (self mapPoint: pos to: View desktop) text: (self getTextAt: selectedPoint)!

startTrackingAt: aPoint
	"Private - Start position tracking for the associated view at aPoint. Part of the 
	<MouseTracker> target protocol. Answers the actual position achieved."!

timeLabelFont
	^GdiplusFont
		name: 'Segoe UI'
		emSize: 8
		style: FontStyleRegular
		unit: UnitPoint!

timestampToString: aTimestamp
	| aStream |
	"^aTimestamp displayString"
	aStream := WriteStream on: String new.
	aTimestamp asIECStringOn: aStream.
	^aStream contents!

timestampToTimeLabel: atimestamp
	| targetStream |
	targetStream := WriteStream on: String new.
	targetStream
		nextPutAll: (Locale default
					printDate: atimestamp date
					format: 'dd.MM.yy'
					flags: 0);
		nextPutAll: String lineDelimiter;
		nextPutAll: (Locale default
					printTime: atimestamp time
					format: 'HH:mm:ss'
					flags: 0).
	^targetStream contents!

trendColor
	^IndexedColor stdColor: #darkMagenta!

trendColorAsRGB
	trendColor
		ifNil: 
			[trendColor := ARGB
						stdColor: (ARGB stdColourTable keys asOrderedCollection at: (Random new next * 50) asInteger + 1)].
	^trendColor!

valueMinMax
	| min max |
	"find max/min element in collection"
	self history size < 1 ifTrue: [^Array with: nil with: nil].
	"max := min := self history first value."
	max := min := 0.
	self history do: 
			[:each | 
			min := min min: each value .
			max := max max: each value].
	"min := self history do: [:c | min := min min: c value].
	max := self history do: [:c | max := max max: c value]."
	"answer := self history inject: min into: [:a :c | a max: c value]."
	^Array with: min with: max! !
!ChartView categoriesFor: #adjustRectangle:!geometry!private! !
!ChartView categoriesFor: #brushPoint:!drawing!public! !
!ChartView categoriesFor: #cancelTrackingAt:!private!tracking! !
!ChartView categoriesFor: #continueTrackingAt:from:!private!tracking! !
!ChartView categoriesFor: #cursor!public! !
!ChartView categoriesFor: #dateOrigin!accessing!public! !
!ChartView categoriesFor: #dateOrigin:!accessing!public! !
!ChartView categoriesFor: #defaultAxisColorAsRGB!drawing!private! !
!ChartView categoriesFor: #defaultAxisPenWidth!accessing!public! !
!ChartView categoriesFor: #defaultMinHBand!public! !
!ChartView categoriesFor: #defaultNumberHBand!public! !
!ChartView categoriesFor: #defaultPointSize!geometry!private! !
!ChartView categoriesFor: #drawAxis:!private!unnecessary code! !
!ChartView categoriesFor: #drawAxisAtGr:!drawing!private! !
!ChartView categoriesFor: #drawAxisLineFrom:to:on:!drawing!private!unnecessary code! !
!ChartView categoriesFor: #drawCompressedModelAtGr:!accessing!private! !
!ChartView categoriesFor: #drawDataPoint:on:!private!unnecessary code! !
!ChartView categoriesFor: #drawDataPoint:on:state:!private!unnecessary code! !
!ChartView categoriesFor: #drawGridLineFrom:to:on:!private!unnecessary code! !
!ChartView categoriesFor: #drawLine:on:!private!unnecessary code! !
!ChartView categoriesFor: #drawLineFrom:to:pen:on:!private!unnecessary code! !
!ChartView categoriesFor: #drawModel:!private!unnecessary code! !
!ChartView categoriesFor: #drawModelAtGr:!accessing!drawing!private! !
!ChartView categoriesFor: #drawSelectedBorder:!accessing!public! !
!ChartView categoriesFor: #drawSelectedRect!accessing!public! !
!ChartView categoriesFor: #drawString:at:on:!public! !
!ChartView categoriesFor: #drawTimeLabel:!accessing!public! !
!ChartView categoriesFor: #endTrackingAt:!private!tracking! !
!ChartView categoriesFor: #eraseBackground:!private!unnecessary code! !
!ChartView categoriesFor: #findNearestPointAt:!helpers!private! !
!ChartView categoriesFor: #getTextAt:!helpers!private!unnecessary code! !
!ChartView categoriesFor: #history!event handling!public! !
!ChartView categoriesFor: #initialize!public! !
!ChartView categoriesFor: #isShowTimeLabel!public!testing! !
!ChartView categoriesFor: #margintY!geometry!private! !
!ChartView categoriesFor: #margintZeroX!geometry!private! !
!ChartView categoriesFor: #onLeftButtonDoubleClicked:!event handling!public!tracking! !
!ChartView categoriesFor: #onLeftButtonPressed:!event handling!public!tracking! !
!ChartView categoriesFor: #onModelChanged!event handling!public! !
!ChartView categoriesFor: #onMouseHovering:!event handling!public! !
!ChartView categoriesFor: #onMouseMoved:!event handling!public! !
!ChartView categoriesFor: #onPaintRequired:!private!unnecessary code! !
!ChartView categoriesFor: #onViewCreated!event handling!public! !
!ChartView categoriesFor: #paintStyledEdgesOn:!drawing!public! !
!ChartView categoriesFor: #penWidth!accessing!private! !
!ChartView categoriesFor: #penWidth:!accessing!private! !
!ChartView categoriesFor: #pointSize!accessing!private! !
!ChartView categoriesFor: #pointSize:!accessing!private! !
!ChartView categoriesFor: #scale!accessing!geometry!helpers!public! !
!ChartView categoriesFor: #scaleDataPointToPixel:!geometry!helpers!public! !
!ChartView categoriesFor: #scaleModelTimestampToPixel:!public! !
!ChartView categoriesFor: #scalePixelToDataPoint:!geometry!public! !
!ChartView categoriesFor: #scalePixelToTimesstamp:!geometry!helpers!private! !
!ChartView categoriesFor: #scalePixelToValue:!geometry!helpers!private! !
!ChartView categoriesFor: #scaleTimestampToPxel:!geometry!helpers!private! !
!ChartView categoriesFor: #scaleValueToPixel:!geometry!helpers!private! !
!ChartView categoriesFor: #scaleX!geometry!public! !
!ChartView categoriesFor: #scaleY!geometry!public! !
!ChartView categoriesFor: #selectedBorder!public! !
!ChartView categoriesFor: #selectedBorder:!public! !
!ChartView categoriesFor: #setBorderModel:!private!tracking! !
!ChartView categoriesFor: #setMouseTrackerAt:!private!tracking! !
!ChartView categoriesFor: #setZeroPoint:!geometry!private! !
!ChartView categoriesFor: #showBubbleAt:text:!helpers!private! !
!ChartView categoriesFor: #showSelectedPointAt:!helpers!private! !
!ChartView categoriesFor: #startTrackingAt:!private!tracking! !
!ChartView categoriesFor: #timeLabelFont!accessing!public! !
!ChartView categoriesFor: #timestampToString:!helpers!private!unnecessary code! !
!ChartView categoriesFor: #timestampToTimeLabel:!accessing!public! !
!ChartView categoriesFor: #trendColor!drawing!private! !
!ChartView categoriesFor: #trendColorAsRGB!drawing!private! !
!ChartView categoriesFor: #valueMinMax!helpers!public! !

!ChartView class methodsFor!

icon
	^##(ScribbleView) icon!

new
	"Answer a new, initialized, instance of the receiver."

	^super new initialize!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: ((Aspect name: #pointSize
					chooseFrom: #('nil' '1@1' '3@3' '5@5' '8@8' ))
					beImmutable;
					isNullable: true;
					yourself);
	add: ((Aspect name: #penWidth)
					isNullable: false;
					beImmutable;
					yourself).
		
	^aspects! !
!ChartView class categoriesFor: #icon!public! !
!ChartView class categoriesFor: #new!public! !
!ChartView class categoriesFor: #publishedAspectsOfInstances!public! !

"Binary Globals"!

