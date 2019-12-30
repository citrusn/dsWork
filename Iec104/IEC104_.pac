| package |
package := Package name: 'IEC104'.
package paxVersion: 1;
	basicComment: 'Пакет для реализации протокола обмена МЭК 104'.

package basicPackageVersion: '0.002'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiA0IEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAABgAAAElFQzEwNFIAAAATAAAAQ29y
ZVxTRFNcaWVjMTA0LmV4ZZoAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAAFQAAAFJ1bnRp
bWVTZXNzaW9uTWFuYWdlcu+/JQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAA').

package classNames
	add: #Acpi;
	add: #ApduTag;
	add: #Asdu;
	add: #Chat2;
	add: #CP56Time2a;
	add: #DataPoint;
	add: #Mek104;
	add: #Qds;
	add: #TagHistory;
	yourself.

package methodNames
	add: #TimeStamp -> #asIECString;
	add: #TimeStamp -> #asIECStringOn:;
	add: 'Locale class' -> #russianLCID;
	yourself.

package globalNames
	add: #Iec104Constants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Samples\Sockets\Chat\Chat';
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Contributions\ITC Gorisek\Dialect Abstraction Layer';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\Object Arts\Dolphin\Sockets\Dolphin Sockets';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\Object Arts\Dolphin\IDE\Base\Internal Bitmaps and Icons';
	yourself).

package!

"Class Definitions"!

Object subclass: #Acpi
	instanceVariableNames: 'buffer'
	classVariableNames: 'NR NS UFrameCommand UFrameString'
	poolDictionaries: 'Iec104Constants'
	classInstanceVariableNames: ''!
Object subclass: #ApduTag
	instanceVariableNames: 'timestamp lenTag valueBlock timestampBlock qualityBlock value quality asdu ioa'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Asdu
	instanceVariableNames: 'buffer tags'
	classVariableNames: 'ASDUType COTType QOIType'
	poolDictionaries: 'Iec104Constants'
	classInstanceVariableNames: ''!
Object subclass: #DataPoint
	instanceVariableNames: 'value timestamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Qds
	instanceVariableNames: 'buffer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TagHistory
	instanceVariableNames: 'history tagName'
	classVariableNames: 'DataPointClass DefaultInstance'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CP56Time2a
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #Chat2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Chat subclass: #Mek104
	instanceVariableNames: 'calleePort sendTime backgroundProcess countStepGI tagsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Locale class methodsFor!

russianLCID
	^1049! !
!Locale class categoriesFor: #russianLCID!public! !

!TimeStamp methodsFor!

asIECString 
	|targetStream|
	targetStream := WriteStream on: String new.
	self asIECStringOn: targetStream.
	^targetStream contents!

asIECStringOn: aStream
	| micros |
	date asOdbcStringOn: aStream.
	aStream nextPut: $\x20.
	time asOdbcStringOn: aStream.
	micros := (time asMilliseconds \\ 1000 * 1) printString.
	aStream
		nextPut: $.;
		nextPutAll: ('000' copy
					replaceFrom: 4 - micros size
					to: 3
					with: micros
					startingAt: 1)! !
!TimeStamp categoriesFor: #asIECString!public! !
!TimeStamp categoriesFor: #asIECStringOn:!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #Iec104Constants put: (PoolConstantsDictionary named: #Iec104Constants)!
Iec104Constants at: 'ACPI_IFrame' put: 16r1!
Iec104Constants at: 'ACPI_SFrame' put: 16r2!
Iec104Constants at: 'ACPI_UFrame' put: 16r3!
Iec104Constants at: 'actStartDT' put: 16r4!
Iec104Constants at: 'actStopDT' put: 16r10!
Iec104Constants at: 'actTestFR' put: 16r40!
Iec104Constants at: 'conStartDT' put: 16r8!
Iec104Constants at: 'conStopDT' put: 16r20!
Iec104Constants at: 'conTestFR' put: 16r80!
Iec104Constants shrink!

"Classes"!

Acpi guid: (GUID fromString: '{e14ed812-d090-443f-9be0-e76e7cde6210}')!
Acpi comment: '	Start byte (0x68)   APCI  Application Protocol Control Information
	1 Length of the APDU (max. 253)   
	2 Control field 1   
	3 Control field 2   
	4 Control field 3   
	5 Control field 4 
•	I-формат для передачи данных телеметрии;
•	S-формат для передачи квитанций;
•	U-формат для передачи посылок установления связи и тестирования канала связи.

(Acpi from: #[104 04 67 00 00 00] ) sendUFrame
 
Надо будет завести машину состояний? Что делать с ответами, Игнорировать?'!
!Acpi categoriesForClass!Kernel-Objects! !
!Acpi methodsFor!

buffer
	^buffer!

buffer: anObject
	buffer := anObject!

bufferToHex: aStream
	"Private - 	Acpi from: #[104 04 67 00 00 00]"

	aStream
		nextPut: $[;
		nextPut: $\x20.
	buffer do: 
			[:each |
			aStream
				nextPutAll: (each printStringRadix: 16 showRadix: false);
				nextPut: $\x20].
	aStream nextPut: $]!

controlField1
	"Private - см. комментарии к классу"

	^buffer third!

controlField2
	"Private - см. комментарии к классу"

	^buffer fourth!

controlField3
	"Private - см. комментарии к классу"

	^buffer at: 5!

controlField4
	"Private - см. комментарии к классу"

	^buffer at: 6!

displayOn: aStream
	"Answer the name of the receiver prepended by 'a' or 'an' as appropriate"

	aStream
		nextPutAll: 'APDU len:';
		nextPutAll: self sizeAPDU printString.	"; nextPut: $\x20"
	self displayTypeFrameOn: aStream.
	self class isShowBuffer
		ifTrue: 
			[aStream nextPutAll: String lineDelimiter.
			self bufferToHex: aStream]!

displayTypeFrameOn: aStream
	| typeFrame |
	typeFrame := self getTypeACPI.
	ACPI_IFrame = typeFrame	ifTrue: 
			[aStream
				nextPutAll: ' I-frame ';
				nextPutAll: self getDescrI].
	ACPI_SFrame = typeFrame	ifTrue: 
			[aStream
				nextPutAll: ' S-frame ';
				nextPutAll: self getDescrS].
	ACPI_UFrame = typeFrame	ifTrue: 
			[aStream
				nextPutAll: ' U-frame ';
				nextPutAll: self getDescrU]!

getDescrI
	"Private - отправляем тип команды в I frame

	Measured scaled with time response, Length 23  NS:0 NR:0
	ASDU: 35 <Measured scaled Information with time> SQ:1 Count 1
	COT: 3 <Spontaneous> Sector 1
	[2] 2018-05-24 16:27:44.023 62 "
	"I-Format byte\bit 7 6 5 4 3 2 1  0 
0 Send sequence number N(S) LSB      0 
1 Send sequence number N(S) MSB 
2 Receive sequence number N(R) LSB 0 
3 Receive sequence number N(R) MSB "

	^'NS:' , (self controlField2 * 256 + self controlField1 >> 1) displayString , 
	 ' NR:', (self controlField4 * 256 + self controlField3 >> 1) displayString!

getDescrS
	"Private - отправляем тип команды в S frame"
	"S-Format byte\bit 
7 6 5 4 3 2 1 0 
0      0       0 1 
1 	0 
2 Receive sequence number N(R) LSB 0 
3 Receive sequence number N(R) MSB "

	^' NR:', ((self controlField4 * 256 + self controlField3) >> 1) displayString
!

getDescrU
	"Private - отправляем описание команды U frame"

	| uCommand |
	uCommand := self getUCmd.
	^self class uFrameString at: uCommand!

getResponseU
	" команда ответа U frame пакета для принятой команды. 
	В моем случае это будет пока только ответ на команду TESTFR ACT request.
	Сейчас это только мастер МЭК 104"

	| uCommand |
	uCommand := self getUCmd.
	uCommand = actStartDT ifTrue:  [^self error: 'Команда ' , (self class uFrameString at: actStartDT) , ' для мастера не применима'].

	uCommand = conStartDT ifTrue: [^ nil].	"нормальная ситуация. действий не требуется"

	uCommand = actStopDT ifTrue: 	[^self error: 'Команда ' , (self class uFrameString at: actStopDT) , ' для мастера не применима'].

	uCommand = conStopDT ifTrue: [^ nil].	"нормальная ситуация. действий не требуется"

	uCommand = actTestFR ifTrue: [^   self class uFrameCommand at: conTestFR].	"отвечаем на команду TESTFR ACT"

	uCommand = conTestFR ifTrue: [^self error: 'Команда ' , (self class uFrameString at: conTestFR) , ' для мастера не применима']!

getTypeACPI
	| control1 |
	"U-Format byte\bit 
         7 6        5 4          3 2 1 0 
0 TESTFR STOPDT STARTDT 1 1 
1 	0 
2 	0 
3 	0 "
	control1 := self controlField1.
	"((buffer at:6) = 0 and: [(buffer at:5) = 0 and: [(buffer at:4) = 0 and: "
	(control1 bitAnd: 3) = 3 ifTrue: [^ACPI_UFrame].

	"S-Format byte\bit 
7 6 5 4 3 2 1 0 
0      0       0 1 
1 	0 
2 Receive sequence number N(R) LSB 0 
3 Receive sequence number N(R) MSB "
	"((buffer third bitAnd: 1) = 0 and: [buffer second = 0 and:"
	control1 = 1 ifTrue: [^ACPI_SFrame].

	"I-Format byte\bit 7 6 5 4 3 2 1  0 
0 Send sequence number N(S) LSB      0 
1 Send sequence number N(S) MSB 
2 Receive sequence number N(R) LSB 0 
3 Receive sequence number N(R) MSB "
	"((buffer third bitAnd: 1) = 0 and:"
	(control1 bitAnd: 1) = 0 ifTrue: [^ACPI_IFrame].
	^self error: 'Неизвестный тип фрейма ACPI'

!

getUCmd
	"Private - первые два бита -тип фрейма"

	^self controlField1 bitAnd: 2r11111100
	"или ^self controlField1 - 3"!

isHaveASDU
	^self sizeASDU > 0!

printOn: aStream
	"Answer the name of the receiver as a developer would wish to see it ."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $)!

sizeAPCI
	" размер заголовка apci"

	^6!

sizeAPDU
	"размер APDU - второй байт в буфере"

	^self buffer second!

sizeASDU
	"размер ASDU - второй байт в буфере-минус размер заголовка APCI"

	^self buffer second - 4! !
!Acpi categoriesFor: #buffer!accessing!public! !
!Acpi categoriesFor: #buffer:!accessing!private! !
!Acpi categoriesFor: #bufferToHex:!private! !
!Acpi categoriesFor: #controlField1!private! !
!Acpi categoriesFor: #controlField2!private! !
!Acpi categoriesFor: #controlField3!private! !
!Acpi categoriesFor: #controlField4!private! !
!Acpi categoriesFor: #displayOn:!public! !
!Acpi categoriesFor: #displayTypeFrameOn:!private! !
!Acpi categoriesFor: #getDescrI!private! !
!Acpi categoriesFor: #getDescrS!private! !
!Acpi categoriesFor: #getDescrU!private! !
!Acpi categoriesFor: #getResponseU!public! !
!Acpi categoriesFor: #getTypeACPI!public! !
!Acpi categoriesFor: #getUCmd!private! !
!Acpi categoriesFor: #isHaveASDU!public! !
!Acpi categoriesFor: #printOn:!public! !
!Acpi categoriesFor: #sizeAPCI!public! !
!Acpi categoriesFor: #sizeAPDU!public! !
!Acpi categoriesFor: #sizeASDU!public! !

!Acpi class methodsFor!

clearCounters
	NS := 0.
	NR := 0!

CountersIntoBuffer: aByteBuffer
	self NRIntoBuffer: aByteBuffer.
	self NSIntoBuffer: aByteBuffer!

fromArray: aByteArray
	(aByteArray size > 5 and: 
			[aByteArray first = 104
				and: [aByteArray second between: 4 and: 253	"and: [aByteArray size = (aByteArray second + 2)]"]])
		ifFalse: [self error: 'APDU некорректен)'].
	^super new
		buffer: (ByteArray withAll: aByteArray);
		yourself!

fromStream: aReadStream
	| msg len |
	[aReadStream next = 16r68] whileFalse.	"ждем начало заголовка пакета"
	len := aReadStream next.	" определяем длину всего пакета"
	msg := ByteArray new: len + 2.	"Заголовол и длина"
	aReadStream next: len into: msg startingAt: 3.	"Читаем полный пакет "
	msg at: 1 put: 16r68.
	msg at: 2 put: len.
	^Acpi fromArray: msg!

getGIRequest
"[68 0E 00 00 00 00 64 01 06 00 01 00 00 00 00 14] GI 100"
	| msg b |
	b := #[104 14 00 00 00 00 100 01 06 00 01 00 00 00 00 20].
	msg := ByteArray new: b size.
	msg
		replaceFrom: 1
		to: b size
		with: b
		startingAt: 1.
	self CountersIntoBuffer: msg.
	self incNS.	"увеличиваем счетчик отправленных пакетов"
	^self fromArray: msg
	"[68 0E 00 00 00 00 64 01 06 00 01 00 00 00 00 14]
Interrogation request, Length 14  NS:3 NR:16
ASDU: 100 <Interrogation Command> Count 1
COT: 6 <Activation> Sector 1
[0] QOI: 20 <Station Interrogation> "!

getSRequest
	| msg b |
	b := #[104 04 01 00 00 00].
	msg := ByteArray new: b size.
	msg replaceFrom: 1 to: b size with: b	startingAt: 1.
	self NRIntoBuffer: msg.
	^self fromArray: msg!

getStartDT: anAct
	" act = true команда запроса, иначе con- команда ответа	
	bit 	4 	3        
	     con  	act"
	"^act ifTrue: [#[104 04 07 00 00 00]] ifFalse: [#[104 04 11 00 00 00]]"

	^anAct ifTrue: [UFrameCommand at: actStartDT] ifFalse: [UFrameCommand at: conStartDT]
!

getStartDTAct
	^self getStartDT: true!

getStopDT: anAct
	" act = true команда запроса, иначе con- команда ответа
	bit 	6 	5        
	     con  	act"
	"^act ifTrue: [#[104 04 19 00 00 00]] ifFalse: [#[104 04 35 00 00 00]]"

	^anAct ifTrue: [UFrameCommand at: actStopDT] ifFalse: [UFrameCommand at: conStopDT]!

getStopDTAct
	^self getStopDT: true!

getTestFR: anAct
	" act = true команда запроса, иначе con- команда ответа
	bit 	7 	6        
	     con  	act"
	"^act ifTrue: [#[104 04 67 00 00 00]] ifFalse: [#[104 04 131 00 00 00]]"

	^anAct ifTrue: [UFrameCommand at: actTestFR] ifFalse: [UFrameCommand at: conTestFR]!

getTestFRAct
	^self getStopDT: true!

icon
	^Mek104 icon!

incNR
	NR := NR + 2
	"младший бит не используется, поэтому счетчик в два раза больше считает"
!

incNS
	NS := NS + 2
	"младший бит не используется, поэтому счетчик в два раза больше считает"
	!

initialize
	"Private - Initialise the map of standard ACPI names to indices.
		self class initialize.	"

	UFrameCommand ifNil: [UFrameCommand := IdentityDictionary new].
	UFrameCommand
		at: actStartDT put: #[104 04 07 00 00 00];
		at: conStartDT put: #[104 04 11 00 00 00];
		at: actStopDT put: #[104 04 19 00 00 00];
		at: conStopDT put: #[104 04 35 00 00 00];
		at: actTestFR put: #[104 04 67 00 00 00];
		at: conTestFR put: #[104 04 131 00 00 00].
	UFrameString ifNil: [UFrameString := IdentityDictionary new].
	UFrameString
		at: actStartDT put: 'STARTDT ACT request';
		at: conStartDT put: 'STARTDT CON response';
		at: actStopDT put: 'STOPDT ACT request';
		at: conStopDT put: 'STOPDT CON response';
		at: actTestFR put: 'TESTFR ACT request';
		at: conTestFR put: 'TESTFR CON response'!

isShowBuffer
	"Private - определяет отображать ли содержимое буфера в строке"
	^ 1 = 0!

new
	"Private - Color's are immutable and must be instantiated through one of the specific instance creation
	methods."

	^self shouldNotImplement!

NRIntoBuffer: aByteBuffer
	aByteBuffer wordAtOffset: 4 put: NR	"Счетчик отправленных пакетов. Занимает два байта"!

NSIntoBuffer: aByteBuffer
	aByteBuffer wordAtOffset: 2 put: NS.	"Счетчик отправленных пакетов. Занимает два байта"
	!

uFrameCommand
	^UFrameCommand!

uFrameString
	^UFrameString!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	UFrameCommand := nil.
	UFrameString := nil.! !
!Acpi class categoriesFor: #clearCounters!public! !
!Acpi class categoriesFor: #CountersIntoBuffer:!private! !
!Acpi class categoriesFor: #fromArray:!public! !
!Acpi class categoriesFor: #fromStream:!public! !
!Acpi class categoriesFor: #getGIRequest!event handlers!public! !
!Acpi class categoriesFor: #getSRequest!event handlers!public! !
!Acpi class categoriesFor: #getStartDT:!event handlers!public! !
!Acpi class categoriesFor: #getStartDTAct!event handlers!public! !
!Acpi class categoriesFor: #getStopDT:!event handlers!public! !
!Acpi class categoriesFor: #getStopDTAct!event handlers!public! !
!Acpi class categoriesFor: #getTestFR:!event handlers!public! !
!Acpi class categoriesFor: #getTestFRAct!event handlers!public! !
!Acpi class categoriesFor: #icon!public! !
!Acpi class categoriesFor: #incNR!private! !
!Acpi class categoriesFor: #incNS!private! !
!Acpi class categoriesFor: #initialize!private! !
!Acpi class categoriesFor: #isShowBuffer!private! !
!Acpi class categoriesFor: #new!private! !
!Acpi class categoriesFor: #NRIntoBuffer:!private! !
!Acpi class categoriesFor: #NSIntoBuffer:!private! !
!Acpi class categoriesFor: #uFrameCommand!private! !
!Acpi class categoriesFor: #uFrameString!private! !
!Acpi class categoriesFor: #uninitialize!private! !

ApduTag guid: (GUID fromString: '{9d43d9bd-5107-4a67-8112-34bf08348e73}')!
ApduTag comment: 'Класс, представляющий тег в стандарте МЭК104.
rate - скорость изменения зачения
counter- счетчик поступивших значений
buffer - место хранения сырых данных протокола

Measured float with time response, Length 25  NS:0 NR:0
ASDU: 36 <Measured float Information with time> SQ:1 Count 1
COT: 3 <Spontaneous> Sector 1 [000/000/007] 2018-07-17 11:57:08.865 49.900002 
<- 11:57:08.963 [68 19 00 00 00 00  -apci  ()  
6 байт - 		36 - M_ME_TF_1  ASDU: 36 <Measured float Information with time>	1 байт
7 байт - 		81 -  SQ:1 Count 1 									   	2 байт
8 байт - 		03 - T   |P/N  |Cause of transmission  COT: 3 <Spontaneous>  	   	3 байт
9 байт - 		00 - Originator address (ORG)   							    	4 байт
10, 11 байт - 	01 00 -  Sector 1  ASDU address fields   					  	5-6 байт
12-14 байт - 	07 00 00 - objAddr IOA [000/000/007] 					  	7-9 байт
15-18 байт -	9A 99 47 42 -value R32 49.900002						  	10-13 байт --- начало тега	
19 байт - 		00 - QDS											  	14 байт
20-26 байт 	A1 22 39 0B 11 07 12 - CP56Time2a 						 	15-21 байт (CP56Time2a new) bytes: #[16rA1 16r22 16r39 16r0B 16r11 16r07 16r12]
t := ApduTag new: #[16r24 81 03 00 01 00 07 00 00 16r9A 16r99 16r47 16r42 00 16rA1 16r22 16r39 16r0B 16r11 07 16r12].
t
'!
!ApduTag categoriesForClass!Kernel-Objects! !
!ApduTag methodsFor!

<= anApduTag
	^self tag <= anApduTag tag!

ASDU
	^asdu!

blockASDU100: aByteArray
	timestamp := TimeStamp current.
	lenTag := 1!

blockASDU11: aByteArray
	value := aByteArray swordAtOffset: 9.	"SVA contains a 16-bit value in the range <-32 768..32 767> 10-11 байт 10-11 байт"
	quality := Qds new: (aByteArray at: 12).	" 00 - QDS	12 байт"
	timestamp := TimeStamp current.
	lenTag := 3!

blockASDU13: aByteArray
	value := (aByteArray floatAtOffset: 9) asScaledDecimal: 2.	"9A 99 47 42 -value R32 49.900002 10-13 байт"
	quality := Qds new: (aByteArray at: 14).	" 00 - QDS	14 байт"
	timestamp := TimeStamp current.
	lenTag := 5!

blockASDU35: aByteArray
	value := aByteArray swordAtOffset: 9.	"SVA contains a 16-bit value in the range <-32 768..32 767> 10-11 байт"
	quality := Qds new: (aByteArray at: 12).	" 00 - QDS	12 байт"
	timestamp := CP56Time2a from: (aByteArray copyFrom: 13).  "(CP56Time2a new) bytes: #[16rA1 16r22 16r39 16r0B 16r11 16r07 16r12] CP56Time2a 13-19 байт "			
	lenTag := 10!

blockASDU36: aByteArray
	value := (aByteArray floatAtOffset: 9) asScaledDecimal: 2.	"9A 99 47 42 -value R32 49.900002 10-13 байт"
	quality := Qds new: (aByteArray at: 14).	" 00 - QDS	14 байт"
	timestamp := CP56Time2a from: (aByteArray copyFrom: 15).	"(CP56Time2a new) bytes: #[16rA1 16r22 16r39 16r0B 16r11 16r07 16r12] CP56Time2a 15-21 байт"
	lenTag := 12!

displayOn: aStream
	aStream
		nextPutAll: 'IOA: ' , self tagname , ' value: ' , self value displayString , ' quality: '
				, self quality displayString , ' timestamp: '
				, self timestamp displayString!

initialize: aByteArray
	"lenTag := 0.	
	valueBlock := self notYetImplemented.
	qualityBlock := self notYetImplemented.
	timestampBlock := self notYetImplemented."

	timestamp := self notYetImplemented.
	value := self notYetImplemented.
	quality := self notYetImplemented.
	ioa := Array
				with: (aByteArray at: 7)
				with: (aByteArray at: 8)
				with: (aByteArray at: 9).
	asdu := aByteArray first.	"Тип ASDU"
	(#(11 13 35 36 100) includes: asdu)
		ifTrue: [ self perform: #blockASDU36:  with: aByteArray]
"self perform: ('blockASDU' , asdu displayString, ':') with: aByteArray]"	"обрабатываемые ASDU"!

lenASDU1
	^1!

lenASDU3
	^1!

lenASDU30
	^8!

lenASDU31
	^8!

lenTag
	"self perform: ('lenASDU' , self ASDU displayString) asSymbol"

	^lenTag!

nameASDU
	"название Типа ASDU"

	^(Asdu ASDUType at: self ASDU) first!

notYetImplemented
	^['<-->']!

printOn: aStream
	"Answer the name of the receiver as a developer would wish to see it ."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

quality
	"quality ifNil: [quality := qualityBlock value]."
	^quality!

quality: anObject
	^Error notYetImplemented!

tag
	"07 00 00 - objAddr IOA [000/000/007] 7-9 байт"

	^ioa at: 7 + ((ioa at: 8) * 256) + ((ioa at: 9) * 65536)!

tagname
	"07 00 00 - objAddr IOA [000/000/007] 7-9 байт"

	^(ioa third) displayString , '/' , (ioa second ) displayString , '/' , (ioa first ) displayString!

tagNext: anOffset
	"07 00 00 - objAddr IOA [000/000/007] 7-9 байт"

	| t c |
	c := 0.
	t := (ioa first ) + anOffset.
	t < 255
		ifTrue: [ioa first put: t]
		ifFalse: 
			[ioa first put: t - 255.
			c := 1].
	t := (ioa second ) + c.
	t < 255
		ifTrue: [ioa second put: t]
		ifFalse: 
			[ioa second put: t - 255.
			c := 1].
	t := (ioa third ) + c.
	ioa third put: t!

timestamp
	 "timestamp ifNil: [timestamp := timestampBlock value]."
	^timestamp!

timestamp: anObject
	^Error notYetImplemented!

value
	"value ifNil: [value := valueBlock value]."
	^value!

value: anObject
	^Error notYetImplemented! !
!ApduTag categoriesFor: #<=!public! !
!ApduTag categoriesFor: #ASDU!accessing!public! !
!ApduTag categoriesFor: #blockASDU100:!helpers!private! !
!ApduTag categoriesFor: #blockASDU11:!helpers!private! !
!ApduTag categoriesFor: #blockASDU13:!helpers!private! !
!ApduTag categoriesFor: #blockASDU35:!helpers!private! !
!ApduTag categoriesFor: #blockASDU36:!helpers!private! !
!ApduTag categoriesFor: #displayOn:!public! !
!ApduTag categoriesFor: #initialize:!public! !
!ApduTag categoriesFor: #lenASDU1!helpers!private! !
!ApduTag categoriesFor: #lenASDU3!helpers!private! !
!ApduTag categoriesFor: #lenASDU30!helpers!private! !
!ApduTag categoriesFor: #lenASDU31!helpers!private! !
!ApduTag categoriesFor: #lenTag!public! !
!ApduTag categoriesFor: #nameASDU!accessing!public! !
!ApduTag categoriesFor: #notYetImplemented!private! !
!ApduTag categoriesFor: #printOn:!public! !
!ApduTag categoriesFor: #quality!accessing!public! !
!ApduTag categoriesFor: #quality:!accessing!private! !
!ApduTag categoriesFor: #tag!accessing!public! !
!ApduTag categoriesFor: #tagname!accessing!public! !
!ApduTag categoriesFor: #tagNext:!accessing!public! !
!ApduTag categoriesFor: #timestamp!accessing!public! !
!ApduTag categoriesFor: #timestamp:!accessing!private! !
!ApduTag categoriesFor: #value!accessing!public! !
!ApduTag categoriesFor: #value:!accessing!private! !

!ApduTag class methodsFor!

icon
	^Mek104 icon!

new: aByteArray
	^super new
		initialize: aByteArray;
		yourself!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect string: #tagname);
		add: (Aspect integer: #ASDU);
		add: (Aspect string: #nameASDU);
		add: (Aspect time: #timestamp);
		add: (Aspect float: #value);
		add: (Aspect string: #quality).
	"add: (Aspect integer: #rate);
		add: (Aspect integer: #count);"
	^aspects! !
!ApduTag class categoriesFor: #icon!public! !
!ApduTag class categoriesFor: #new:!public! !
!ApduTag class categoriesFor: #publishedAspectsOfInstances!public! !

Asdu guid: (GUID fromString: '{69bbd8aa-8b01-4e30-837d-4530ae2f7e72}')!
Asdu comment: '[68 17 00 00 00 00] - acpi 
[ 23 81  03 00 01 00 02 00 00 3E 00 00 F7 AB  1B 10 18 05 12] - asdu hex
(Asdu from: #[35 129 03 00 01 00 02 00  0 62 00 0 247 121 27 16 24 05 18] ) displayString
byte/bit    
	7    6       5
6 	Type identification 
7 	SQ | Number of objects   
8 	T   |P/N  |Cause of transmission (COT)   
9 	Originator address (ORG)   
10 	ASDU address fields   
11 	(2 octets)   
12 	Information object address fields (IOA)   - если SQ >1, то  адрес только один
13 	(3 octets)   
14     
15 	Object information -  если SQ >1, а здесь информация повторяется по каждому тегу без адреса IOA
'!
!Asdu categoriesForClass!Kernel-Objects! !
!Asdu methodsFor!

ASDU
	"Тип ASDU"

	^buffer first!

buffer
	^buffer!

buffer: anObject
	buffer := anObject!

bufferToHex: aStream
	"Private - 	Asdu from: #[23 81 03 00 01 00 02 00 00 3E 00 00 F7 AB 1B 10 18 05 12]	"

	aStream nextPutAll: '[ '.
	buffer do: 
			[:each |
			aStream
				nextPutAll: (each printStringRadix: 10 showRadix: false);
				nextPut: $\x20].
	aStream nextPut: $]!

COT
	"Cause of transmission (COT)   "

	^buffer third bitAnd: 63!

countOfTags
	^buffer second bitAnd: 127!

createTags: aByteArray
	"создаем тег по необходимости"

	| t |
	t := SortedCollection new.
	self countOfTags < 2
		ifTrue: [t add: (ApduTag new: aByteArray)]
		ifFalse: [t add: (ApduTag new: aByteArray)].
	^t!

displayOn: aStream
	"Measured scaled with time response, Length 23  NS:0 NR:0
ASDU: 35 <Measured scaled Information with time> SQ:1 Count 1
COT: 3 <Spontaneous> Sector 1
[2] 2018-05-24 16:27:44.023 62	"

	| id typeIdentDescr typeIdentASDU cotDescr cot |
	id := self ASDU.
	typeIdentDescr := (self class ASDUType at: id) second.
	typeIdentASDU := (self class ASDUType at: id) first.
	cot := self COT.
	cotDescr := self class COTType at: cot.
	aStream
		nextPutAll: typeIdentASDU , (' SQ: ' , self SQ displayString)
					, (' Count: ' , self countOfTags displayString) , String lineDelimiter;
		nextPutAll: 'ASDU: ' , id displayString , ' <' , typeIdentDescr , '>' , String lineDelimiter;
		nextPutAll: 'T: ' , self T displayString , ' P/N: ' , self PN displayString , ' COT: '
					, cot displayString , ' <'
					, cotDescr , '>'.
	self class isShowBuffer ifTrue: [self bufferToHex: aStream]!

initialize
	Acpi incNR.	"считаем пришедшие пакеты только с ASDU"
	tags := self createTags: buffer!

ORG
	"Originator address (ORG)   "

	^buffer at: 9!

PN
	"P/N (бит 7) = 0, положительное подтверждение
	P/N (бит 7) = 1, отрицательное подтверждение"

	^(buffer second bitAnd: 2r01000000) = 2r01000000!

printOn: aStream
	"Answer the name of the receiver as a developer would wish to see it ."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

SQ
	^(buffer second bitAnd: 128) = 128!

T
	"T (бит 8) = 0,  рабочая передача
	T (бит 8) = 1,  тестовая передача"

	^(buffer third bitAnd: 128) = 128!

tags
 ^ tags! !
!Asdu categoriesFor: #ASDU!public! !
!Asdu categoriesFor: #buffer!accessing!private! !
!Asdu categoriesFor: #buffer:!accessing!private! !
!Asdu categoriesFor: #bufferToHex:!private! !
!Asdu categoriesFor: #COT!accessing!public! !
!Asdu categoriesFor: #countOfTags!accessing!public! !
!Asdu categoriesFor: #createTags:!private! !
!Asdu categoriesFor: #displayOn:!public! !
!Asdu categoriesFor: #initialize!public! !
!Asdu categoriesFor: #ORG!public! !
!Asdu categoriesFor: #PN!accessing!public! !
!Asdu categoriesFor: #printOn:!public! !
!Asdu categoriesFor: #SQ!accessing!public! !
!Asdu categoriesFor: #T!accessing!public! !
!Asdu categoriesFor: #tags!public! !

!Asdu class methodsFor!

ASDUType
	^ASDUType!

COTType
	^COTType!

icon 
	
	^Mek104  icon!

initialize
	"Private - Initialise the map of ASDU type identifier to indices.
		self class initialize. 	"

	self initializeASDU.
	self initializeCOT.
	self initializeQOI!

initializeASDU
	ASDUType ifNil: [ASDUType := IdentityDictionary new].
	ASDUType
		at: 000 put: #('ASDU_TYPEUNDEF' 'not allowed');
		at: 001 put: #('M_SP_NA_1' 'Single-point information');
		at: 002 put: #('M_SP_TA_1' 'Single-point information with time tag');
		at: 003 put: #('M_DP_NA_1' 'Double-point information');
		at: 004 put: #('M_DP_TA_1' 'Double-point information with time tag');
		at: 005 put: #('M_ST_NA_1' 'Step position information');
		at: 006 put: #('M_ST_TA_1' 'Step position information with time tag');
		at: 007 put: #('M_BO_NA_1' 'Bitstring of 32 bit');
		at: 008 put: #('M_BO_TA_1' 'Bitstring of 32 bit with time tag');
		at: 009 put: #('M_ME_NA_1' 'Measured value, normalised value');
		at: 010 put: #('M_ME_TA_1' 'Measured value, normalized value with time tag');
		at: 011 put: #('M_ME_NB_1' 'Measured value, scaled value');
		at: 012 put: #('M_ME_TB_1' 'Measured value, scaled value wit time tag');
		at: 013 put: #('M_ME_NC_1' 'Measured value, short floating point number');
		at: 014 put: #('M_ME_TC_1' 'Measured value, short floating point number with time tag');
		at: 015 put: #('M_IT_NA_1' 'Integrated totals');
		at: 016 put: #('M_IT_TA_1' 'Integrated totals with time tag');
		at: 017 put: #('M_EP_TA_1' 'Event of protection equipment with time tag');
		at: 018 put: #('M_EP_TB_1' 'Packed start events of protection equipment with time tag');
		at: 019
			put: #('M_EP_TC_1' 'Packed output circuit information of protection equipment with time tag');
		at: 020 put: #('M_PS_NA_1' 'Packed single point information with status change detection');
		at: 021 put: #('M_ME_ND_1' 'Measured value, normalized value without quality descriptor');
		at: 030 put: #('M_SP_TB_1' 'Single-point information with time tag');
		at: 031 put: #('M_DP_TB_1' 'Double-point information with time tag');
		at: 032 put: #('M_ST_TB_1' 'Step position information with time tag');
		at: 033 put: #('M_BO_TB_1' 'Bitstring of 32 bit with time tag');
		at: 034 put: #('M_ME_TD_1' 'Measured value, normalised value with time tag');
		at: 035 put: #('M_ME_TE_1' 'Measured value, scaled value with time tag');
		at: 036 put: #('M_ME_TF_1' 'Measured value, short floating point number with time tag');
		at: 037 put: #('M_IT_TB_1' 'Integrated totals with time tag');
		at: 038 put: #('M_EP_TD_1' 'Event of protection equipment with time tag');
		at: 039 put: #('M_EP_TE_1' 'Packed start events of protection equipment with time tag');
		at: 040
			put: #('M_EP_TF_1' 'Packed output circuit information of protection equipment with time tag');
		at: 045 put: #('C_SC_NA_1' 'Single command');
		at: 046 put: #('C_DC_NA_1' 'Double command');
		at: 047 put: #('C_RC_NA_1' 'Regulating step command');
		at: 048 put: #('C_SE_NA_1' 'Set-point Command, normalised value');
		at: 049 put: #('C_SE_NB_1' 'Set-point Command, scaled value');
		at: 050 put: #('C_SE_NC_1' 'Set-point Command, short floating point number');
		at: 051 put: #('C_BO_NA_1' 'Bitstring 32 bit command');
		at: 058 put: #('C_SC_TA_1' 'Single command with time tag');
		at: 059 put: #('C_DC_TA_1' 'Double command with time tag');
		at: 060 put: #('C_RC_TA_1' 'Regulating step command with time tag');
		at: 061 put: #('C_SE_TA_1' 'Measured value, normalised value command with time tag');
		at: 062 put: #('C_SE_TB_1' 'Measured value, scaled value command with time tag');
		at: 063 put: #('C_SE_TC_1' 'Measured value, short floating point number command with time tag');
		at: 064 put: #('C_BO_TA_1' 'Bitstring of 32 bit command with time tag CP56Time2a');
		at: 070 put: #('M_EI_NA_1' 'End of Initialisation');
		at: 100 put: #('C_IC_NA_1' 'Interrogation command');
		at: 101 put: #('C_CI_NA_1' 'Counter interrogation command');
		at: 102 put: #('C_RD_NA_1' 'Read Command');
		at: 103 put: #('C_CS_NA_1' 'Clock synchronisation command');
		at: 104 put: #('C_TS_NA_1' 'Test command');
		at: 105 put: #('C_RP_NA_1' 'Reset process command');
		at: 106 put: #('C_CD_NA_1' 'C_CD_NA_1 Delay acquisition command');
		at: 107 put: #('C_TS_TA_1' 'Test command with time tag');
		at: 110 put: #('P_ME_NA_1' 'Parameter of measured values, normalized value');
		at: 111 put: #('P_ME_NB_1' 'Parameter of measured values, scaled value');
		at: 112 put: #('P_ME_NC_1' 'Parameter of measured values, short floating point number');
		at: 113 put: #('P_AC_NA_1' 'Parameter activation');
		at: 120 put: #('F_FR_NA_1' 'File ready');
		at: 121 put: #('F_SR_NA_1' 'Section ready');
		at: 122 put: #('F_SC_NA_1' 'Call directory, select file, call file, call section');
		at: 123 put: #('F_LS_NA_1' 'Last section, last segment');
		at: 124 put: #('F_FA_NA_1' 'ACK file, ACK section');
		at: 125 put: #('F_SG_NA_1' 'Segment');
		at: 126 put: #('F_DR_TA_1' 'Directory')!

initializeCOT
	COTType ifNil: [COTType := IdentityDictionary new].
	COTType
		at: 0 put: 'Not used';
		at: 1 put: 'Cyclic data';
		at: 2 put: 'Background request';
		at: 3 put: 'Spontaneous';
		at: 4 put: 'End of initialisation';
		at: 5 put: 'Read-Request';
		at: 6 put: 'Activation';
		at: 7 put: 'Activation confirmation';
		at: 8 put: 'Command abort';
		at: 9 put: 'Command abort confirmation';
		at: 10 put: 'Activation termination';
		at: 11 put: 'Return because of remote command';
		at: 12 put: 'Return because local command';
		at: 13 put: 'File access';
		at: 20 put: 'Station interrogation (general)';
		at: 21 put: 'INRO1';
		at: 22 put: 'INRO2';
		at: 23 put: 'INRO3';
		at: 24 put: 'INRO4';
		at: 25 put: 'INRO5';
		at: 26 put: 'INRO6';
		at: 27 put: 'INRO7';
		at: 28 put: 'INRO8';
		at: 29 put: 'INRO9';
		at: 30 put: 'INRO10';
		at: 31 put: 'INRO11';
		at: 32 put: 'INRO12';
		at: 33 put: 'INRO13';
		at: 34 put: 'INRO14';
		at: 35 put: 'INRO15';
		at: 36 put: 'INRO16';
		at: 37 put: 'REQCOGEN';
		at: 38 put: 'REQCO1';
		at: 39 put: 'REQCO2';
		at: 40 put: 'REQCO3';
		at: 41 put: 'REQCO4';
		at: 44 put: 'UNKNOWN_TYPE';
		at: 45 put: 'UNKNOWN_CAUSE';
		at: 46 put: 'UNKNOWN_ASDU_ADDRESS';
		at: 47 put: 'UNKNOWN_OBJECT_ADDRESS'!

initializeQOI
	QOIType ifNil: [QOIType := IdentityDictionary new].
	QOIType
		at: 0 put: 'UNUSED';
		at: 20 put: 'INROGEN';
		at: 21 put: 'INRO1';
		at: 22 put: 'INRO2';
		at: 23 put: 'INRO3';
		at: 24 put: 'INRO4';
		at: 25 put: 'INRO5';
		at: 26 put: 'INRO6';
		at: 27 put: 'INRO7';
		at: 28 put: 'INRO8';
		at: 29 put: 'INRO9';
		at: 30 put: 'INRO10';
		at: 31 put: 'INRO11';
		at: 32 put: 'INRO12';
		at: 33 put: 'INRO13';
		at: 34 put: 'INRO14';
		at: 35 put: 'INRO15';
		at: 36 put: 'INRO16'!

isShowBuffer
	"Private - определяет отображать ли содержимое буфера в строке"
	^ 1 = 1!

new: aByteArray
	"(aByteArray size = 6 and: [aByteArray first = 104 and: [aByteArray second between: 4 and: 253]]) ifFalse: [self error: 'APDU некорректен)']."

	^super new
		buffer: aByteArray;
		initialize;
		yourself!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."
	"self  uninitialize. "
	ASDUType := nil. 
	COTType:= nil.
	QOIType := nil
	! !
!Asdu class categoriesFor: #ASDUType!public! !
!Asdu class categoriesFor: #COTType!public! !
!Asdu class categoriesFor: #icon!public! !
!Asdu class categoriesFor: #initialize!private! !
!Asdu class categoriesFor: #initializeASDU!private! !
!Asdu class categoriesFor: #initializeCOT!public! !
!Asdu class categoriesFor: #initializeQOI!public! !
!Asdu class categoriesFor: #isShowBuffer!private! !
!Asdu class categoriesFor: #new:!public! !
!Asdu class categoriesFor: #uninitialize!private! !

DataPoint guid: (GUID fromString: '{f86d3855-3d22-4654-89f8-fc31bd95a6cb}')!
DataPoint comment: ''!
!DataPoint categoriesForClass!Kernel-Objects! !
!DataPoint methodsFor!

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
	target nextPutAll: '; ', String lineDelimiter.
	(value + 0.005) printOn: target decimalPlaces: 2.
	target nextPutAll: ' ]'!

initialize
	super initialize.
!

printOn: aStream
	"Append a textual representation of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		display: self;
		nextPut: $)!

timestamp
	^timestamp!

timestamp: anObject
	timestamp := anObject!

value
	^value!

value: anObject
	value := anObject! !
!DataPoint categoriesFor: #copyFrom:!public! !
!DataPoint categoriesFor: #displayOn:!public! !
!DataPoint categoriesFor: #initialize!public! !
!DataPoint categoriesFor: #printOn:!public! !
!DataPoint categoriesFor: #timestamp!accessing!public! !
!DataPoint categoriesFor: #timestamp:!accessing!public! !
!DataPoint categoriesFor: #value!accessing!public! !
!DataPoint categoriesFor: #value:!accessing!public! !

!DataPoint class methodsFor!

icon
	^Icon fromId: 'UndefinedObject.ico'!

new
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^self new: TimeStamp current value: 0.
		!

new: aTimeStamp value: aValue
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^super new
		initialize;
		timestamp: aTimeStamp;
		value: aValue;
		yourself! !
!DataPoint class categoriesFor: #icon!public! !
!DataPoint class categoriesFor: #new!public! !
!DataPoint class categoriesFor: #new:value:!public! !

Qds guid: (GUID fromString: '{185af862-b9bd-4688-a0bf-9701bbfea8a5}')!
Qds comment: '	  7  6   5   4 3  2 1   0   
         IV NT SB BL 0 0 0 SPI - SIQ = Single-point information with quality descriptor 
				   DPI - DIQ = Double-point information with quality descriptor 
				    OV      
OV Overflow quality flag:	BL Blocked quality flag:	SB Substituted quality flag:	NT Topical quality flag:	IV Invalid quality flag:
<0> = no overflow;		<0> = not blocked;		<0> = not substituted;		<0> = topical;			<0> = valid;
<1> = overflow;			<1> = blocked;			<1> = substituted;			<1> = not topical;		<1> = invalid;

CY Carry flag: 	CA Adjusted flag:				EI Elapsed flag:
<0> = no carry;	<0> = Counter was not adjusted;	<0> = Elapsed time valid;
<1> = carry;	<1> = Counter was adjusted;		<1> = Elapsed time not valid;
Qds from: 250'!
!Qds categoriesForClass!Kernel-Objects! !
!Qds methodsFor!

blocked
	"BL
Blocked quality flag:
##<0> = not blocked;
##<1> = blocked;"

	^(buffer bitAnd: 16r10) > 0!

buffer
	^buffer!

buffer: anObject
	buffer := anObject!

displayOn: aStream
	"Answer the name of the receiver prepended by 'a' or 'an' as appropriate"
	"IV NT SB BL "

	aStream nextPut: ${.
	self invalid ifTrue: [aStream nextPutAll: 'IV '].
	self not_topical ifTrue: [aStream nextPutAll: 'NT '].
	self substituted ifTrue: [aStream nextPutAll: 'SB '].
	self blocked ifTrue: [aStream nextPutAll: 'BL '].
	self overflow ifTrue: [aStream nextPutAll: 'OV'].
	aStream nextPut: $}!

invalid
	"IV
Invalid quality flag:
##<0> = valid;
##<1> = invalid;"

	^(buffer bitAnd: 16r80) > 0!

not_topical
	"NT
Topical quality flag:
##<0> = topical;
##<1> = not topical;"

	^(buffer bitAnd: 16r40) > 0!

overflow
	"OV
Overflow quality flag:
##<0> = no overflow;
##<1> = overflow;"

	^(buffer bitAnd: 16r01) > 0!

printOn: aStream
	"Answer the name of the receiver as a developer would wish to see it ."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

substituted
"	SB
Substituted quality flag:
##<0> = not substituted;
##<1> = substituted;"

	^ (buffer bitAnd:  16r20) > 0! !
!Qds categoriesFor: #blocked!accessing!public! !
!Qds categoriesFor: #buffer!accessing!private! !
!Qds categoriesFor: #buffer:!accessing!private! !
!Qds categoriesFor: #displayOn:!public! !
!Qds categoriesFor: #invalid!accessing!public! !
!Qds categoriesFor: #not_topical!accessing!public! !
!Qds categoriesFor: #overflow!accessing!public! !
!Qds categoriesFor: #printOn:!public! !
!Qds categoriesFor: #substituted!accessing!public! !

!Qds class methodsFor!

icon 
	
	^Mek104  icon!

new
	"Private - Color's are immutable and must be instantiated through one of the specific instance creation
	methods."

	^self shouldNotImplement!

new: aByte
	^super new
		buffer: aByte;
		yourself! !
!Qds class categoriesFor: #icon!public! !
!Qds class categoriesFor: #new!private! !
!Qds class categoriesFor: #new:!public! !

TagHistory guid: (GUID fromString: '{2805fdd7-76fa-46ba-9e2b-9c810ca5e3d7}')!
TagHistory comment: 'Класс для хранения тега с именем tagName и историей изменения history c точками класса DataPoint'!
!TagHistory categoriesForClass!Kernel-Objects! !
!TagHistory methodsFor!

displayOn: aStream
	"Append, to aStream, a String whose characters are a description of the receiver as a user
	would want to see it."

	aStream
		nextPutAll: ', ';
		display: self history size;
		nextPutAll: ' tags'
	"display: self owner;"!

history
	"Answer the <collection> of tapes held by the receiver.

	Illustrated Patterns
	Accessor Method
	"

	^history!

history: anObject
	history := anObject!

historyAdd: aDataPoint

	self history add: aDataPoint.
	^aDataPoint!

historyAdd: aTimestamp value: aValue
	| point |
	point := DataPointClass new: aTimestamp value: aValue.
	history add: point.
	^point!

initialize
	"Private - Initialize the receiver to contain default values.

	Illustrated Patterns
	Instance Initialization
	"
	tagName := ''.
	self history: OrderedCollection new
	!

printOn: aStream
	"Append, to aStream, a String whose characters are a description of the receiver as a developer
	would want to see it."

	self basicPrintOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self tagName, ' size: ';
		display: self history size;
		nextPut: $)
!

saveToCSV: aFileName
	| file |
	file := File open: aFileName mode: #truncate check: false share: #read.
	self history
		do: [:each | file write: each timestamp asIECString , ', ' , each value displayString , String lineDelimiter].
	file close!

tagName
	^tagName!

tagName: anObject
	tagName := anObject! !
!TagHistory categoriesFor: #displayOn:!public! !
!TagHistory categoriesFor: #history!public! !
!TagHistory categoriesFor: #history:!accessing!public! !
!TagHistory categoriesFor: #historyAdd:!public! !
!TagHistory categoriesFor: #historyAdd:value:!public! !
!TagHistory categoriesFor: #initialize!private! !
!TagHistory categoriesFor: #printOn:!public! !
!TagHistory categoriesFor: #saveToCSV:!public! !
!TagHistory categoriesFor: #tagName!accessing!public! !
!TagHistory categoriesFor: #tagName:!accessing!public! !

!TagHistory class methodsFor!

default
	"Private - Answer the default instance of the receiver."
^ Error notYetImplemented.
	DefaultInstance isNil ifTrue: [
		DefaultInstance := self new ].
	^DefaultInstance !

getHistoryTag: aTag start: aDateEnd end: aDateStart
	| sql rs c |
	sql := 'select m.value, TO_DATE (TO_CHAR (m.value_date , ''DD.MM.YYYY HH24:MI:SS''), ''DD.MM.YYYY HH24:MI:SS'') AS value_date, m.tagname from astue.measures m  where m.tagname ='''
				, aTag , ''' and cast(m.value_date as date) between to_date('''
				, aDateStart , ''') and to_date('''
				, aDateEnd , ''')'.
	"Transcript show: sql; cr."
	c := self defaultConnectionDb.
	rs := c query: sql.
	^[rs collect: [:each | DataPointClass new: (each atIndex: 2) value: ((each at: 'VALUE') asFloat roundTo: 0.01) ]]
		ensure: [c close]!

icon

	"Generated from:
	self createIconMethod: #icon ofSize: 48@48 fromFile: 'C:\Object Arts\Dev\Dolphin6\Object Arts\Samples\MVP\Video Library\VideoLibrary.png'.
	"
	^InternalIcon fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 6 0 0 0 87 2 249 135 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 16 17 73 68 65 84 104 67 237 89 9 115 148 87 118 157 84 170 146 74 170 82 149 26 131 217 145 192 6 139 85 128 118 181 212 146 90 234 214 174 214 190 75 221 173 86 107 107 73 72 66 43 235 0 18 2 45 8 48 54 130 145 141 49 140 199 108 97 177 49 152 33 54 102 95 108 6 24 48 251 238 56 255 98 78 238 185 77 51 194 224 154 196 153 177 61 149 168 234 213 123 253 125 95 127 125 207 61 247 158 123 223 211 47 126 241 255 127 127 3 30 232 238 238 246 141 51 199 195 18 159 136 210 82 219 229 3 7 14 140 250 27 48 251 79 38 154 98 45 48 153 226 16 29 109 66 100 100 20 76 177 102 196 196 196 34 33 33 9 121 121 121 135 110 223 190 253 143 63 107 64 52 56 210 24 131 168 168 24 152 98 226 212 120 99 148 73 1 197 154 44 178 150 235 2 208 106 205 128 211 233 236 1 240 119 63 27 64 86 171 245 162 217 18 143 104 49 50 70 140 140 139 77 128 209 24 141 40 49 158 159 35 34 163 213 120 174 13 17 79 217 145 53 159 49 155 205 168 173 173 141 252 73 193 196 138 247 105 116 70 70 22 146 147 147 97 77 207 84 207 199 75 62 112 86 22 226 44 136 136 48 122 194 76 216 33 56 178 197 193 235 113 113 113 8 11 11 67 126 126 254 31 127 212 112 91 179 102 77 148 217 146 132 166 5 45 240 155 54 13 227 198 141 199 216 177 227 145 150 150 142 226 18 135 128 202 70 170 172 233 125 5 42 137 30 249 148 29 2 137 120 154 47 156 195 194 35 4 92 140 176 100 84 80 137 137 137 112 187 221 247 254 170 236 208 136 5 141 141 200 205 201 70 181 179 12 239 109 29 194 230 193 141 24 24 232 71 103 231 10 180 181 181 162 190 174 30 165 197 37 200 47 40 66 94 65 33 146 83 210 20 144 2 137 49 169 193 145 145 145 194 70 52 194 195 13 122 207 40 107 21 3 147 136 2 239 27 163 144 46 204 58 28 246 29 127 81 64 89 89 57 40 16 227 135 6 55 96 255 254 127 195 161 67 7 241 209 71 7 176 107 207 78 236 248 96 59 54 111 25 68 95 95 15 154 155 23 160 168 168 4 69 79 129 20 20 22 35 205 154 134 236 236 92 152 45 9 207 0 153 98 61 74 22 21 19 45 66 64 69 139 148 240 52 235 53 230 12 89 140 17 150 162 163 163 81 88 88 136 234 234 106 231 15 6 36 30 249 163 195 102 195 150 183 223 196 137 47 62 199 137 19 199 113 234 212 9 124 242 201 199 56 118 236 40 78 159 62 137 227 199 63 195 111 118 108 69 73 73 41 178 115 242 80 168 32 74 117 38 35 92 231 229 23 34 39 55 95 66 205 138 180 244 116 13 179 56 201 25 99 116 148 39 228 132 9 26 78 182 169 102 102 185 166 57 20 29 35 215 37 159 162 228 190 48 101 149 239 87 84 84 248 253 183 1 165 166 101 98 233 210 101 56 115 230 12 174 92 185 130 107 215 174 225 218 245 27 184 115 239 46 238 220 189 143 219 119 238 225 203 175 190 194 201 83 103 64 143 151 148 218 197 96 187 174 9 128 131 107 2 225 32 136 116 1 64 64 233 34 8 156 19 18 147 17 47 181 36 220 32 76 152 19 20 4 243 133 5 147 33 168 245 70 195 209 3 150 242 205 176 75 73 73 145 119 23 220 223 188 121 243 63 191 20 80 115 115 115 123 166 132 207 249 139 23 113 239 193 19 49 248 33 110 222 186 131 235 95 223 212 153 198 127 125 243 38 30 61 122 136 237 59 182 163 212 230 80 111 23 151 216 116 77 48 202 192 48 38 248 153 10 230 5 200 217 59 146 146 83 117 29 159 152 160 74 70 6 200 18 223 199 207 100 70 175 9 136 8 163 128 21 85 163 68 147 161 151 2 136 49 153 145 146 106 197 141 155 119 113 239 209 55 98 240 3 220 18 16 156 31 62 250 15 28 250 228 168 204 79 112 251 238 29 216 29 78 13 31 155 189 76 7 1 240 26 25 40 38 168 98 15 35 52 102 56 59 94 150 188 76 21 202 115 42 4 194 12 141 107 104 104 84 70 56 200 8 65 112 77 102 56 40 221 204 157 231 0 164 89 51 97 145 164 35 181 177 113 9 184 250 245 45 92 186 122 29 55 239 62 192 157 7 143 113 243 206 125 220 186 125 31 187 246 30 196 229 171 87 240 224 225 99 56 202 202 159 13 26 206 225 5 193 217 11 202 195 80 169 176 82 164 160 8 136 131 215 117 148 20 35 35 51 27 4 210 181 170 7 107 122 86 163 188 220 9 139 57 86 37 55 33 41 201 83 241 37 172 152 240 100 40 86 68 225 57 0 140 71 222 72 179 74 209 74 73 151 23 230 138 247 108 232 233 91 167 32 110 221 123 136 171 215 111 225 250 205 59 56 119 241 18 154 154 219 197 248 10 148 13 3 225 5 228 53 156 225 244 167 16 179 191 104 248 211 164 247 2 201 203 203 199 135 59 247 162 175 191 31 43 59 59 69 170 219 4 136 11 117 181 110 164 165 38 139 24 120 100 154 225 148 110 77 189 253 28 0 122 159 44 36 36 166 72 188 102 235 72 207 200 145 164 201 80 48 4 229 44 175 18 25 61 128 179 2 192 233 170 68 86 118 158 128 112 233 245 50 167 75 217 224 76 163 189 249 224 93 115 46 46 177 107 142 12 31 12 181 146 82 135 134 218 201 211 167 112 224 208 17 252 118 231 30 108 216 176 9 27 223 222 130 129 117 111 97 112 112 11 58 218 219 81 92 144 163 14 54 199 39 188 24 255 164 148 70 39 38 165 234 204 7 169 70 92 115 206 204 202 19 112 169 98 116 190 22 44 42 11 61 77 163 203 93 213 50 42 213 120 187 131 185 64 163 236 207 66 106 120 56 41 43 195 239 81 193 36 132 120 253 238 221 187 184 240 229 69 188 53 248 142 204 87 176 121 104 27 222 220 180 5 93 107 250 176 237 55 31 160 183 111 173 254 62 251 179 23 18 184 170 186 14 65 161 145 168 114 55 32 41 53 83 12 44 84 195 249 5 50 145 148 108 213 245 194 69 203 148 17 126 206 206 41 16 192 105 10 136 96 232 69 26 194 53 243 193 3 200 249 236 179 55 71 188 73 207 207 14 169 242 237 11 23 106 110 252 231 183 223 224 241 55 79 240 217 23 103 240 233 177 47 176 105 232 61 108 220 188 21 3 194 198 206 189 251 241 238 214 237 40 182 151 75 174 90 158 7 224 174 107 24 42 175 172 131 179 194 13 71 69 13 236 206 42 228 23 217 145 145 93 128 82 71 197 51 22 242 242 139 117 205 94 136 198 167 164 102 60 99 134 50 201 48 35 48 246 72 106 156 228 136 23 204 119 129 120 193 180 181 47 84 22 223 145 86 229 209 147 199 120 242 205 183 184 112 233 138 128 56 173 12 112 108 220 52 132 143 14 31 197 231 199 79 32 95 42 190 84 234 99 207 49 80 90 82 122 33 33 201 138 100 107 1 202 92 53 10 164 188 178 22 149 53 245 50 26 244 90 82 138 21 181 13 173 234 125 42 6 89 201 200 36 51 169 10 134 134 39 38 165 168 241 156 217 244 49 228 210 100 159 192 231 189 249 193 217 3 174 28 85 213 226 176 167 34 112 67 106 11 165 153 82 253 249 233 243 56 240 241 81 108 217 186 3 67 50 62 62 124 12 231 191 188 140 179 103 207 171 212 190 16 62 29 29 29 177 161 161 161 152 60 121 50 102 206 156 142 128 192 32 132 132 199 192 154 93 136 170 218 6 1 49 95 88 169 134 83 88 170 168 170 147 151 8 19 82 236 202 132 41 111 24 49 148 60 161 149 170 33 69 16 204 19 174 9 78 5 65 24 243 130 177 151 149 73 242 87 168 140 58 37 140 46 92 252 61 206 156 191 132 19 103 47 226 184 0 248 244 51 105 87 78 158 195 49 97 226 247 87 190 86 73 63 39 32 88 23 94 90 192 124 38 249 98 198 140 233 8 9 9 18 205 13 71 76 180 81 42 160 1 33 97 1 152 19 16 132 136 232 4 20 151 150 195 85 81 173 33 198 208 114 62 101 43 218 20 175 172 101 229 22 33 67 146 221 18 159 44 161 149 163 33 21 159 144 162 198 83 225 152 83 188 70 7 120 20 47 89 61 90 81 89 131 247 119 238 199 142 15 15 224 189 223 238 195 193 195 191 83 16 159 159 58 135 127 63 113 22 215 111 220 193 213 63 220 64 181 228 39 197 228 165 0 194 13 6 73 192 2 132 134 6 75 239 63 14 198 72 131 108 33 35 97 50 26 96 8 15 146 158 69 70 88 16 102 207 158 137 185 65 225 72 20 67 170 221 245 178 95 104 131 75 88 177 149 85 42 83 26 110 226 241 249 13 237 72 144 176 99 239 147 102 149 144 147 48 74 16 48 12 41 2 240 48 150 163 224 152 63 218 240 201 61 134 212 206 221 251 113 226 220 37 1 113 1 231 191 186 140 107 55 111 43 59 188 47 187 188 95 191 20 0 247 185 61 189 171 196 160 122 68 155 162 180 19 140 137 137 194 140 89 211 17 20 20 32 213 57 74 182 150 225 136 48 132 192 215 215 87 64 142 193 152 113 99 49 126 130 15 106 235 26 181 22 84 84 74 238 84 185 81 51 191 21 238 250 22 9 191 249 72 77 207 65 195 130 118 100 230 122 242 164 177 169 89 43 42 219 149 204 44 134 92 154 130 76 151 13 18 89 99 65 37 32 182 40 100 204 41 236 116 118 247 41 147 204 175 23 140 111 104 88 80 194 155 113 230 68 41 227 171 113 234 244 89 252 250 221 247 225 18 93 47 40 96 23 153 134 120 75 172 104 111 140 48 84 132 192 160 121 152 232 235 163 44 141 25 59 22 163 70 141 194 152 49 163 240 250 235 147 101 30 7 131 48 57 115 230 76 212 184 235 80 45 34 64 49 112 86 213 163 90 64 82 130 185 201 97 221 200 205 43 122 102 44 13 246 128 200 82 96 204 19 230 14 215 236 145 24 106 52 254 5 0 7 143 28 241 99 99 20 26 102 80 74 169 253 247 238 63 196 165 203 127 192 225 223 157 196 193 67 71 177 107 247 62 44 95 190 92 138 147 244 48 197 133 194 66 164 244 35 6 217 26 134 35 56 56 80 182 153 99 49 122 244 104 140 28 57 18 175 188 242 138 206 175 190 250 170 128 27 143 73 175 189 134 217 254 115 81 37 64 106 235 154 80 33 51 235 12 65 85 84 213 34 81 12 116 8 115 5 133 165 106 52 13 244 2 73 146 182 217 154 158 37 234 151 162 137 75 192 62 19 39 188 200 64 180 28 149 4 6 133 106 114 49 249 156 229 149 88 178 116 5 22 72 175 83 83 35 181 193 89 41 215 92 232 236 234 198 134 245 27 144 155 155 171 61 185 197 44 173 111 84 132 108 216 67 100 187 24 170 201 63 105 146 15 70 140 24 241 116 188 130 17 35 71 96 148 128 27 59 126 60 124 38 189 134 215 167 248 105 163 88 41 198 187 133 17 123 121 181 230 79 69 117 61 74 164 64 37 137 74 149 185 170 36 95 114 21 12 99 62 62 49 73 152 72 215 34 90 86 86 106 124 33 132 232 125 14 86 95 198 31 227 144 18 200 53 43 235 186 245 111 98 211 166 65 12 13 189 43 76 116 170 190 123 229 113 85 119 183 180 185 209 104 105 105 86 32 20 0 2 153 59 119 46 166 201 65 128 135 17 1 242 234 72 140 20 86 66 66 194 240 218 235 83 49 125 134 63 102 204 244 135 223 244 89 154 59 53 181 141 2 168 73 69 160 107 117 191 178 100 146 211 144 156 252 34 77 114 178 82 38 142 124 105 242 150 187 106 30 4 4 6 11 210 20 105 93 37 214 132 86 106 249 155 27 223 70 127 255 90 172 23 175 51 70 169 215 111 189 61 168 50 198 4 163 134 179 32 17 112 79 79 143 60 219 143 213 171 5 144 200 111 115 115 147 40 142 69 153 201 200 176 98 217 178 37 114 173 25 1 1 1 200 202 202 66 137 244 74 237 11 59 116 176 79 90 208 220 138 150 214 118 125 63 243 134 33 199 228 181 11 27 14 25 165 54 167 230 206 247 110 41 121 64 69 93 102 104 120 10 79 170 122 158 198 113 205 246 129 113 202 207 185 121 5 162 245 76 50 143 68 174 29 88 47 219 207 165 104 109 235 80 227 228 116 78 119 77 177 50 184 233 55 24 194 196 41 201 202 78 102 102 186 42 90 87 87 23 146 164 199 31 47 49 237 227 59 25 115 230 6 169 66 53 137 66 113 152 164 174 188 62 101 154 178 99 151 144 38 8 246 85 125 125 43 39 188 20 68 152 193 168 50 70 143 186 164 23 98 99 247 225 206 221 42 143 243 27 154 53 31 118 239 217 143 222 129 13 114 95 10 86 94 161 234 117 255 218 117 210 159 91 196 248 69 88 180 120 41 6 183 188 171 64 223 219 182 93 223 81 98 119 97 232 157 109 234 140 141 111 13 194 230 176 11 56 147 2 107 107 107 81 230 194 12 161 146 31 190 218 208 45 94 178 76 139 218 162 142 37 226 148 95 9 115 203 101 119 214 4 63 191 233 122 244 82 39 127 47 5 192 98 212 216 212 42 122 45 115 75 59 154 90 59 196 224 125 104 20 195 249 121 247 222 189 216 246 254 7 88 37 122 204 106 200 74 236 174 23 85 145 100 164 138 208 83 46 169 1 149 98 52 239 55 52 183 161 170 174 5 245 141 172 7 11 176 120 217 10 189 206 26 225 221 224 100 231 74 108 203 70 63 37 37 73 100 210 34 50 110 146 62 223 44 149 63 68 24 48 75 190 173 196 146 197 203 177 104 209 18 180 183 45 150 16 182 193 102 179 237 124 41 128 125 251 246 253 171 221 225 66 145 173 12 249 133 54 164 203 70 133 113 151 40 217 111 73 76 195 242 21 171 240 171 229 93 114 106 96 210 150 59 56 196 128 192 80 3 130 195 140 8 12 137 192 188 224 48 169 206 161 210 114 132 138 162 133 233 125 122 187 68 142 102 130 195 141 98 148 17 161 50 243 252 212 24 197 29 85 162 134 12 243 137 187 190 114 201 39 183 212 13 158 2 210 96 122 126 229 202 46 116 119 175 121 54 214 10 219 169 18 222 223 155 7 245 226 209 89 179 231 73 35 55 23 243 230 205 129 191 255 108 204 153 51 7 179 102 205 244 180 15 115 102 203 61 127 204 155 235 15 31 31 95 213 121 142 209 82 188 56 7 135 132 98 180 212 132 231 175 143 193 184 241 19 37 214 125 48 97 162 47 166 76 245 83 21 154 250 198 52 81 162 89 152 37 53 130 2 18 40 223 53 68 200 94 87 128 177 33 164 116 183 180 180 137 211 86 74 190 116 163 183 183 95 194 173 79 148 112 232 95 190 23 64 145 244 255 19 124 38 98 230 28 127 61 29 11 10 10 210 132 99 177 10 12 156 167 96 194 66 101 45 149 120 234 212 169 152 50 101 10 198 139 190 191 241 198 84 77 72 86 223 217 179 103 235 224 119 253 252 166 170 234 240 123 116 6 187 93 127 127 127 209 244 84 113 202 44 61 43 13 15 151 214 68 10 34 159 231 111 240 212 142 154 207 90 209 223 63 32 170 214 163 245 167 179 107 245 131 63 123 168 117 228 200 145 95 242 133 101 229 14 217 176 75 217 95 216 142 197 75 23 233 96 194 57 236 165 112 149 219 69 106 227 116 131 77 131 34 34 141 250 227 60 38 12 12 12 212 163 65 182 18 188 23 21 99 84 0 81 82 185 3 2 230 202 137 118 188 30 153 152 229 52 161 177 113 190 246 89 73 73 9 242 127 5 238 244 114 213 232 62 217 50 114 48 116 58 59 59 119 253 89 163 191 251 64 96 112 144 20 170 52 124 114 248 8 78 156 60 133 35 159 126 134 179 231 46 104 140 230 231 231 41 51 60 80 162 145 244 58 79 202 244 64 74 174 49 1 121 100 24 47 27 238 72 57 192 229 33 46 43 46 165 55 43 59 91 138 82 1 242 132 101 246 54 165 54 187 54 114 108 31 214 173 219 160 222 166 209 189 189 189 141 255 99 163 135 127 193 45 155 23 30 32 241 244 192 233 114 73 51 87 172 71 24 172 5 172 204 108 170 98 196 131 238 218 122 61 92 226 1 64 124 66 178 135 118 75 162 182 35 46 81 26 30 8 216 157 21 98 104 57 220 13 109 146 208 114 98 33 202 149 34 245 164 92 230 1 169 29 52 122 205 154 94 122 188 253 127 101 244 240 47 175 90 181 218 65 35 248 127 1 107 70 134 210 74 99 104 100 106 90 134 246 74 44 233 156 89 220 248 28 117 159 50 90 32 160 185 7 102 167 201 182 154 206 112 75 139 144 40 76 85 85 85 171 193 235 165 30 48 68 68 255 151 255 197 140 254 238 139 76 166 88 45 78 244 54 219 89 175 177 170 16 82 121 45 241 73 186 169 231 97 46 103 134 137 238 161 101 35 83 227 158 175 109 0 251 152 86 105 13 6 6 214 169 209 148 192 61 123 246 248 252 213 140 30 254 226 172 108 182 11 197 18 74 102 13 9 14 110 206 105 40 189 206 122 65 47 51 60 120 173 161 177 69 10 154 91 54 65 241 232 150 70 172 191 111 61 122 123 52 60 158 60 126 252 248 159 126 20 163 135 255 200 138 149 221 218 186 210 216 230 150 14 212 53 54 63 11 141 154 250 70 112 212 207 151 127 106 72 161 139 144 162 180 182 79 18 112 85 47 86 117 245 200 113 224 234 239 47 52 63 22 146 93 187 118 253 3 189 79 79 179 15 98 82 86 75 104 48 60 98 45 169 146 196 226 105 81 140 174 206 53 88 177 162 147 37 255 219 159 213 191 86 233 40 54 98 220 207 86 138 209 81 18 26 89 210 125 118 118 118 105 85 100 153 151 2 179 242 199 114 232 15 250 157 48 233 91 108 165 54 44 91 34 93 225 226 101 82 212 22 179 59 236 248 65 47 251 41 190 36 255 82 250 101 123 107 155 68 6 254 254 167 248 253 255 51 191 249 95 163 223 72 109 229 97 44 60 0 0 0 0 73 69 78 68 174 66 96 130]!

new
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^super new initialize! !
!TagHistory class categoriesFor: #default!private! !
!TagHistory class categoriesFor: #getHistoryTag:start:end:!accessing!public! !
!TagHistory class categoriesFor: #icon!public! !
!TagHistory class categoriesFor: #new!public! !

CP56Time2a guid: (GUID fromString: '{f7725e12-b982-4313-a878-6e5bbe4426a7}')!
CP56Time2a comment: 'TYPE T_CP56Time2a :
	STRUCT
		0, 1 Milliseconds : WORD; (* 0..59.999ms = 60sec = 1min *)
		2 IVResMinute    : BYTE; (* Bit 7 = IV (invalid time), Bit 6 = Res (spare bit), Bit 0..5 = Minutes (0..59min) *)
		3 SURes2Hour    : BYTE; (* Bit 7 = SU (1=summer time, 0=normal time), Bits 5..6 = Res2, Bits 0..4 = Hours (0..23) *)
		4 DOWDay         : BYTE; (* Bits 5..7 = Day of week (1..7, not used 0 !!!!!!), Bits 0..4 = Day of month (1..31)*)
		5 Res3Month      : BYTE; (* Bits 4..7 = Res3 (spare bits), Bits 0..3 = Month (1..12) *)
		6 Res4Year        : BYTE; (* Bit 7 = Res4, Bits 0..6 = Year (0..99) *)
	END_STRUCT
END_TYPE

Day of week (DOW): 1 = Monday, 7 = Sunday, 0 = not used;
SU: 1 = Summer time, 0 = normal time;

7B 19 06 12 16 08 12  2018-08-22 18:06:06.523 
cp := (CP56Time2a new) bytes: #[16r7B 16r19 06 16r12 16r16 08 16r12 ]

'!
!CP56Time2a categoriesForClass!External-Data-Structured! !
!CP56Time2a methodsFor!

asTimeStamp
	| aDate aTime |
	aTime := Time
				hours: self hours
				minutes: self minutes
				seconds: self seconds
				milliseconds: self milliseconds.
	aDate := Date
				newDay: self dayOfMonth
				monthIndex: self month
				year: self year.
	^TimeStamp date: aDate time: aTime!

dayOfMonth
	"день недели"

	^(bytes byteAtOffset: 4) & 31!

dayOfMonth: anInteger
	"день недели"

	bytes byteAtOffset: 4 put: ((bytes byteAtOffset: 4) & 16rE0) + (anInteger & 31)!

dayOfWeek
	"день недели"

	^((bytes byteAtOffset: 4) & 224) >> 5!

dayOfWeek: anInteger
	"день недели"

	"^((bytes byteAtOffset: 4) & 224) >> 5"
	"encodedValue[4] = (byte)((encodedValue[4] & 0x1f) | ((value & 0x07) << 5))"

	bytes byteAtOffset: 4 put: (bytes byteAtOffset: 4) & 16r1F | ((anInteger & 16r07) << 5)!

displayOn: aStream
	"2018-07-17 11:57:08.865 {SU IV}"

	aStream
		nextPutAll: 'CP56Time2a: ' , self year displayString , '-' , self month displayString , '-' , self dayOfMonth displayString;
		nextPutAll: ' ' , self hours displayString , ':' , self minutes displayString , ':' , self seconds displayString , '.' , self milliseconds displayString;
		nextPutAll: ' {' , (self isSummerTime ifTrue: ['SU'] ifFalse: [''])
					, (self isInvalidTime ifTrue: [' IV'] ifFalse: ['']) , '}'!

hours
	"число часов "

	^(bytes byteAtOffset: 3) & 31!

hours: anInteger
	"число часов "

	"encodedValue[3] = (byte)((encodedValue[3] & 0xe0) | (value & 0x1f));"
	bytes byteAtOffset: 3 put:  (bytes byteAtOffset: 3) & 240 | (anInteger & 31)

!

isInvalidTime
	" время с признаком Invalid"

	^((bytes byteAtOffset: 2) & 16r80) == 1!

isInvalidTime: aBoolean
	" время с признаком Invalid"

	aBoolean
		ifTrue: [bytes byteAtOffset: 2 put: ((bytes byteAtOffset: 2) bitOr: 16r80)]
		ifFalse: [bytes byteAtOffset: 2 put: ((bytes byteAtOffset: 2) bitAnd: 16r7F)]!

isSummerTime
	" время летнее"

	^((bytes byteAtOffset: 3) & 16r80) == 16r80!

isSummerTime: aBoolean
	" время летнее"

	aBoolean
		ifTrue: [bytes byteAtOffset: 3 put: ((bytes byteAtOffset: 3) bitOr: 16r80)]
		ifFalse: [bytes byteAtOffset: 3 put: ((bytes byteAtOffset: 3) bitAnd: 16r7F)]!

milliseconds
	"число  секунд.милисекунд"

	^(bytes wordAtOffset: 0) \\ 1000!

milliseconds: anInteger
	"число  секунд.милисекунд"
	"^(bytes wordAtOffset: 0) / 1000"

	| millies |
	millies := self seconds * 1000 + anInteger.
	bytes wordAtOffset: 0 put: millies

	"encodedValue[0] = (byte)(millies & 0xff); 
	encodedValue[1] = (byte)((millies / 0x100) & 0xff); "!

minutes
	"число минут "

	^(bytes byteAtOffset: 2) & 63!

minutes: anInteger
	"число минут "

	"encodedValue[2] = (byte)((encodedValue[2] & 0xc0) | (value & 0x3f));"

	bytes byteAtOffset: 2 put: (bytes byteAtOffset: 2) & 192 | (anInteger & 31)

!

month
	"месяц 1-12"

	^(bytes byteAtOffset: 5) & 15!

month: anInteger
	"месяц 1-12"

	bytes byteAtOffset: 5 put: ((bytes byteAtOffset: 5) & 16r0F) + (anInteger & 16r0F)!

printOn: aStream
	"Answer the name of the receiver as a developer would wish to see it ."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $)!

seconds
	"число  секунд"

	^(bytes wordAtOffset: 0) // 1000!

seconds: anInteger
	"число  секунд"

	"(bytes byteAtOffset: 5) put: ((bytes byteAtOffset: 5) & 16r0F) + (anInteger & 16r0F)"

	| msPart millies |
	msPart := self milliseconds \\ 1000.
	millies := anInteger * 1000 + msPart.
	bytes wordAtOffset: 0 put: millies

	"bytes byteAtOffset: 0 put: millies & 255.
	bytes byteAtOffset: 1 put: millies // 256 & 255"!

year
	"Получаем год из структуры."

	^((bytes byteAtOffset: 6) & 16r7F) + 2000!

year: anInteger
	"Получаем год из структуры."

	"(bytes byteAtOffset: 6) & 16r7F) + 2000"
	
	bytes byteAtOffset: 6 put: ((bytes byteAtOffset: 6) & 16r80) + ((anInteger - 2000) & 16r7F)! !
!CP56Time2a categoriesFor: #asTimeStamp!public! !
!CP56Time2a categoriesFor: #dayOfMonth!public! !
!CP56Time2a categoriesFor: #dayOfMonth:!public! !
!CP56Time2a categoriesFor: #dayOfWeek!public! !
!CP56Time2a categoriesFor: #dayOfWeek:!public! !
!CP56Time2a categoriesFor: #displayOn:!public! !
!CP56Time2a categoriesFor: #hours!public! !
!CP56Time2a categoriesFor: #hours:!public! !
!CP56Time2a categoriesFor: #isInvalidTime!public! !
!CP56Time2a categoriesFor: #isInvalidTime:!public! !
!CP56Time2a categoriesFor: #isSummerTime!public! !
!CP56Time2a categoriesFor: #isSummerTime:!public! !
!CP56Time2a categoriesFor: #milliseconds!public! !
!CP56Time2a categoriesFor: #milliseconds:!public! !
!CP56Time2a categoriesFor: #minutes!public! !
!CP56Time2a categoriesFor: #minutes:!public! !
!CP56Time2a categoriesFor: #month!public! !
!CP56Time2a categoriesFor: #month:!public! !
!CP56Time2a categoriesFor: #printOn:!public! !
!CP56Time2a categoriesFor: #seconds!public! !
!CP56Time2a categoriesFor: #seconds:!public! !
!CP56Time2a categoriesFor: #year!public! !
!CP56Time2a categoriesFor: #year:!public! !

!CP56Time2a class methodsFor!

defineFields
	" 
	Milliseconds     : WORD; (* 0..59.999ms = 60sec = 1min *)
	IVResMinute    : BYTE; (* Bit 7 = IV (invalid time), Bit 6 = Res (spare bit), Bit 0..5 = Minutes (0..59min) *)
	SURes2Hour     : BYTE; (* Bit 7 = SU (1=summer time, 0=normal time), Bits 5..6 = Res2, Bits 0..4 = Hours (0..23) *)
	DOWDay          : BYTE; (* Bits 5..7 = Day of week (1..7, not used 0 !!!!!!), Bits 0..4 = Day of month (1..31)*)
	Res3Month      : BYTE; (* Bits 4..7 = Res3 (spare bits), Bits 0..3 = Month (1..12) *)
	Res4Year         : BYTE; (* Bit 7 = Res4, Bits 0..6 = Year (0..99) *)
	"
	self
		defineField: #Milliseconds type: WORDField new;
		defineField: #IVResMinute type: BYTEField new;
		defineField: #SURes2Hour type: BYTEField new;
		defineField: #DOWDay type: BYTEField new;
		defineField: #Res3Month type: BYTEField new;
		defineField: #Res4Year type: BYTEField new.
	self byteSize: 7!

from: aByteArray
	^super new
		bytes: aByteArray;
		yourself!

fromTimestamp: aTimestamp
	" 
	self fromTimestamp: Timestamp now
	"
	^super new
		bytes: (ByteArray new: 7);
		year: aTimestamp date year;
		month: aTimestamp date monthIndex;
		dayOfMonth: aTimestamp date dayOfMonth;
		dayOfWeek: aTimestamp date weekdayIndex;
		hours: aTimestamp time hours;
		minutes: aTimestamp time minutes;
		seconds: aTimestamp time seconds;
		milliseconds: aTimestamp time milliseconds;
		yourself!

icon
	"Answer a suitable iconic representation for the receiver."

	^Date icon!

new
	Error notYetImplemented! !
!CP56Time2a class categoriesFor: #defineFields!public! !
!CP56Time2a class categoriesFor: #from:!public! !
!CP56Time2a class categoriesFor: #fromTimestamp:!public! !
!CP56Time2a class categoriesFor: #icon!public! !
!CP56Time2a class categoriesFor: #new!private! !

Chat2 guid: (GUID fromString: '{2e66bf40-394d-4926-a56c-7f520c7e26be}')!
Chat2 comment: ''!
!Chat2 categoriesForClass!MVP-Presenters! !
!Chat2 class methodsFor!

icon
	"Generated from:
	self createIconMethod: #icon ofSize: 48@48 fromFile: 'D:\Spc\Dolphin\Resources\iec104.png'.
	"
	^InternalIcon fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 6 0 0 0 87 2 249 135 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 2 75 73 68 65 84 104 67 237 152 189 74 196 64 20 133 125 8 159 195 231 240 49 236 125 0 65 43 193 66 11 107 193 126 11 17 20 193 66 155 109 196 66 4 113 91 101 65 80 65 214 78 27 187 184 103 55 99 78 238 222 153 220 49 193 76 36 7 110 145 220 159 153 111 126 119 179 148 117 92 61 64 219 234 1 218 214 63 2 24 220 102 217 242 102 97 120 182 136 115 124 182 178 155 7 71 232 106 172 215 130 81 223 234 1 132 26 97 139 5 208 106 72 59 186 155 133 22 0 167 247 229 0 60 87 233 230 169 156 179 178 151 101 235 199 139 182 117 158 39 24 84 85 15 239 224 91 0 120 156 148 147 241 92 165 201 103 57 231 238 57 119 252 82 135 215 69 45 116 116 251 34 119 144 222 62 230 32 121 255 210 1 64 199 220 232 194 118 46 115 71 88 233 0 156 140 138 58 0 1 144 65 205 2 104 107 22 203 194 34 140 184 171 131 60 163 154 5 208 204 218 25 196 185 28 43 244 84 233 204 64 18 0 117 246 64 231 1 120 15 24 79 32 40 29 0 140 186 171 211 218 41 84 7 96 252 94 190 7 140 203 40 29 0 136 247 1 96 180 155 248 243 107 30 55 122 153 61 54 11 160 157 66 48 235 166 148 183 177 86 207 249 113 241 77 85 15 0 163 193 13 250 12 13 91 229 126 235 132 234 194 55 122 157 133 215 3 128 208 24 231 105 22 113 170 252 104 248 160 215 130 81 223 10 128 142 170 7 104 91 61 64 219 170 6 144 127 246 247 135 185 35 160 181 65 57 199 153 85 171 7 139 185 158 143 12 97 0 173 80 8 192 242 149 162 106 0 224 215 242 162 0 228 168 179 133 58 128 207 39 90 142 52 128 250 164 197 195 162 0 184 35 214 37 36 71 110 227 44 119 76 37 125 88 98 154 120 233 201 217 143 6 64 1 72 46 11 31 0 55 232 114 89 236 215 62 116 201 129 146 237 70 1 176 172 0 28 195 163 239 132 119 28 35 197 179 142 54 254 20 192 18 131 119 28 131 28 39 134 243 205 124 178 0 50 215 247 62 89 0 222 27 188 244 58 1 128 78 185 103 185 177 59 1 32 223 89 12 121 164 102 0 32 142 177 158 66 242 157 197 196 76 52 7 192 199 96 213 61 224 252 73 205 128 28 77 142 11 249 124 146 237 70 237 1 30 173 144 241 79 2 217 160 207 180 91 88 83 45 0 94 14 33 147 191 105 44 75 66 44 1 175 254 124 6 88 49 177 62 213 2 232 144 122 128 118 149 101 223 15 56 138 81 188 43 202 2 0 0 0 0 73 69 78 68 174 66 96 130]!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1140850688 131073 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 7 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 560 nil 721158 ##(Smalltalk.SystemColor) 31 328198 ##(Smalltalk.Point) 1041 241 7 nil nil nil 560 852230 ##(Smalltalk.FramingLayout) 170 176 34 18 410 ##(Smalltalk.TextEdit) 34 16 nil 560 34 2 8 1140916224 1025 752 nil 482 512 nil 7 nil nil nil 752 nil 8 4294904179 852486 ##(Smalltalk.NullConverter) nil nil 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 658 231 11 658 191 51 752 946 #text: 34 1 8 '127.0.0.1' 752 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 115 0 0 0 5 0 0 0 210 0 0 0 30 0 0 0] 8 #() 658 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 231 1186 #fixedViewLeft 191 1186 #fixedParentTop 11 1186 #fixedViewTop 51 410 ##(Smalltalk.TextEdit) 34 16 nil 560 34 2 8 1140916224 1025 1264 nil 482 512 nil 7 nil nil nil 1264 nil 8 4294904179 850 nil nil 1 882 138 144 34 2 946 #createAt:extent: 34 2 658 561 11 658 151 51 1264 946 #text: 34 1 8 '2404' 1264 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 5 0 0 0 99 1 0 0 30 0 0 0] 8 #() 1136 nil 27 1154 1200 561 1216 151 1232 11 1248 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 1600 nil 482 512 nil 7 nil nil nil 1600 nil 8 4294904549 1180998 4 ##(Smalltalk.CommandDescription) #disconnect 8 '&Отключить' 1 1 nil nil false nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 961 165 658 161 51 1600 946 #text: 34 1 8 '&Отключить' 1600 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 1 0 0 82 0 0 0 48 2 0 0 107 0 0 0] 8 #() 1136 nil 29 1154 1186 #fixedParentRight -179 1216 161 1232 165 1248 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 1984 nil 482 512 nil 7 nil nil nil 1984 nil 8 4294904549 1698 #send 8 '&Отправить' 1 1 nil nil false nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 961 85 658 161 51 1984 946 #text: 34 1 8 '&Отправить' 1984 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 1 0 0 42 0 0 0 48 2 0 0 67 0 0 0] 8 #() 1136 nil 29 1154 1968 -179 1216 161 1232 85 1248 51 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 560 34 2 8 1143017796 1025 2336 nil 482 512 nil 7 nil nil nil 2336 nil 8 4294904179 850 nil nil 9 882 138 144 34 1 946 #createAt:extent: 34 2 658 231 81 658 681 131 2336 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 115 0 0 0 40 0 0 0 199 1 0 0 105 0 0 0] 8 #() 1136 nil 27 1154 1200 231 1968 -229 1232 81 1248 131 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 2624 nil 482 512 nil 7 nil nil nil 2624 nil 8 4294904549 1698 #connect 8 '&Подключить' 1 5 nil nil true nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 961 15 658 161 51 2624 946 #text: 34 1 8 '&Подключить' 2624 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 1 0 0 7 0 0 0 48 2 0 0 32 0 0 0] 8 #() 1136 nil 29 1154 1968 -179 1216 161 1232 15 1248 51 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 2976 nil nil nil 7 nil nil nil 2976 nil 8 4294904207 850 nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 29 91 658 155 51 2976 946 #text: 34 1 8 'Сообщение:' 2976 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 0 45 0 0 0 91 0 0 0 70 0 0 0] 8 #() 1136 nil 27 1154 1200 29 1216 155 1232 91 1248 51 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 3296 nil nil nil 7 nil nil nil 3296 nil 8 4294904207 850 nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 21 21 658 201 51 3296 946 #text: 34 1 8 'Адрес сервера' 3296 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 110 0 0 0 35 0 0 0] 8 #() 1136 nil 27 1154 1200 21 1216 201 1232 21 1248 51 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 3616 nil nil nil 7 nil nil nil 3616 nil 8 4294904207 850 nil nil nil 882 138 144 34 2 946 #createAt:extent: 34 2 658 491 11 658 71 51 3616 946 #text: 34 1 8 'Порт' 3616 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 5 0 0 0 24 1 0 0 30 0 0 0] 8 #() 1136 nil 27 1154 1200 491 1216 71 1232 11 1248 51 170 192 34 6 2336 8 'outgoingMessage' 752 8 'calleeAddress' 1264 8 'calleePort' 590342 ##(Smalltalk.Rectangle) 658 1 1 658 1 1 882 138 144 34 1 946 #createAt:extent: 34 2 658 1 1 658 1141 241 560 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 58 2 0 0 120 0 0 0] 34 9 2624 2336 1984 1600 3616 2976 3296 1264 752 1136 nil 27 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1140850956 1 4240 nil 482 512 nil 7 nil nil nil 4240 nil 8 4294903693 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 4240 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 4368 #defaultGetTextBlock 468040131 8 #[30 105 226 0 106] #displayString 4416 7 257 nil nil 1049926 1 ##(Smalltalk.IconImageManager) 8 'status' 34 1 4384 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 4240 nil nil 882 138 144 34 1 946 #createAt:extent: 34 2 658 1 601 658 1141 41 4240 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 44 1 0 0 58 2 0 0 64 1 0 0] 8 #() 1136 nil 27 nil nil 410 ##(Smalltalk.RichTextEdit) 34 18 nil 416 34 2 8 1142947908 1025 4736 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 196934 1 ##(Smalltalk.RGB) 25231361 nil 7 nil nil nil 4736 nil 8 1846837755 850 nil nil 3 nil 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 0 0 180 1] 882 138 144 34 4 946 #createAt:extent: 34 2 658 1 241 658 1141 361 4736 946 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fnil\fcharset0 Microsoft Sans Serif;}}
\viewkind4\uc1\pard\lang2057\f0\fs16 
\par }
' 4736 946 #isTextModified: 8 #(true) 4736 946 #resetCharFormat 8 #() 4736 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 120 0 0 0 58 2 0 0 44 1 0 0] 8 #() 1136 nil 27 170 192 34 2 4736 8 'messageDisplay' nil 882 138 144 34 1 946 #createAt:extent: 34 2 658 3839 21 658 1141 641 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 185 9 0 0 74 1 0 0] 34 3 560 4736 4240 1136 nil 27 )! !
!Chat2 class categoriesFor: #icon!public! !
!Chat2 class categoriesFor: #resource_Default_view!public!resources-views! !

Mek104 guid: (GUID fromString: '{91e56240-5c3a-4a83-8993-061a73732a8c}')!
Mek104 comment: '	Mek104 show.

This will connect to a TCP/IP port (given by #defaultPort) and start a background process to wait for incoming connections from another Chat window. Ideally the other Chat instance will be created on separate machine, if one is available. However, more than one Chat instance can be created on a single machine but in this case only the first will be able to receive incoming calls since this will be the one that grabs the connection port. 

To establish a Chat session, enter the target machine name or IP address and click Connect. When a connection has successfully been established a pair of threads are forked to manage sending and receiving of messages across the bidirectional socket. Outbound messages are communicated to the sender thread through a shared queue. The sender is responsible for dequeueing messages and forwarding to the remote peer, which it does by writing them to the socket''s buffered write stream. When the complete message has been written the stream is flushed so that the message is transmitted, an  operation which may block the sender thread.  The receiver thread is responsible for receiving complete messages from the socket, and enqueuing them for asynchronous display by the UI process. Note that there is no shared queue for the inbound messages, since the deferred action mechanism process by the InputState object provides this capability in a convenient way for the UI thread.

You can start up a Process Monitor to view the different threads involved in a Chat session and watch how the set changes depending on the session state. 

Messages are transmitted as objects using STB. The message object is just an Association, the key of which identifies the sender, and the value of which is the message text itself. This message format would potentially allow for multiple participants in the conversation, but implementing this is left as an exercise. Note that a validating in-filer is used to guard against executing code that might be transmitted by a malicious remote Smalltalker!!

Messages can then be typed at either end and sent using the Send button.  A conversation can only be held between two Chat sessions at any one time. To disconnect a session click Disconnect or close one of the Chat windows.

Instance Variables:
	calleeAddress		<TextPresenter> holding the IP address to call.
	outgoingMessage	<TextPresenter> for entering a message to send.
	messageDisplay		<RichTextEdit> displaying received messages.
	status			<ValueHolder> holding a <readableString> status message.
	connectSocket		<Socket2> used for connection with a remote Chat instance.
	acceptSocket		<ServerSocket2> used to wait for a connection.
	sendQueue		<SharedQueue> in which outgoing messages are queued for the sender thread.
	sendTime 			Время отправки последнего пакета
'!
!Mek104 categoriesForClass!MVP-Presenters! !
!Mek104 methodsFor!

acceptCalls
	"Private - Monitor the port for a connection request. If the port is already in use then we'll
	receive an error and can display an 'Unable to receive calls' status message."

	"acceptSocket :=   [ServerSocket2 port: self defaultPort backlog: 0] on: SocketError
				do: [:ex | nil]."
	self setAcceptStatus.
	"acceptSocket isNil ifFalse: [self forkAccepter]"!

addMessage
	messageDisplay clear.
	messageDisplay text: 'привет, мир'.
	1 to: 10000
		do: 
			[:i |
			messageDisplay replaceSelection: i displayString , String lineDelimiter
			"self halt."]!

addTags: aTagCollection
	"Create a new tags and add it to the receiver's library"

	aTagCollection do: [:each | tagsPresenter model add: each]!

appendMessage: anObject
	"Private - An incoming message of aString has been received so we append this to the message display"

	anObject isEmpty ifTrue: [^nil].
	"anArray do:  [:each | stream nextPutAll: (each printStringRadix: 16 showRadix: false); nextPutAll: '  ']."
	messageDisplay caretPosition: 0.
	messageDisplay	replaceSelection: String lineDelimiter, 
			SYSTEMTIME now displayString , ' ' , anObject displayString , String lineDelimiter	" (String with: $\r with: $\n)"!

backgroundPauseMS
	^1000!

backgroundStep
	"Выполняется задача в фоне. Каждый период backgroundPauseMS"

	| now |
	sendTime isNil ifTrue: [^true].
	now := Timestamp current.
	now totalSeconds - sendTime totalSeconds > self defaultSRequestSeconds
		ifTrue: [self enqueueOutboundMessage: Acpi getSRequest].
	countStepGI = 0
		ifTrue: 
			[self gi.
			self setPeriodGI]
		ifFalse: [countStepGI := countStepGI - 1]!

calleePort
	"Answer the default port to use for chatting."
	
	^calleePort value
!

clearMessage
	"очистка окна обмена"

	messageDisplay clear.
	"Transcript show: 'clearMessage'."
	"messageDisplay text: ''"!

closeConnection
	"Private - Forcibly any connection with another chat window.
	Answer whether the connection was actually closed (i.e. 
	the answer will only be true if the connection was previously
	open)."
	socket isNil ifTrue: [^false].
	socket close.
	socket := nil.
	"Send a dummy message to wake up the send thread so that it terminates - the sender might
	not notice that the socket is closed until the next time it tries to send because it is
	blocked on the SharedQueue, not blocked on the socket (unlike the read process, which is
	blocked in the socket recv())"
	
	sendQueue nextPut: nil. 		
	"sds"
	self appendMessage:  '== Отключено'.
	backgroundProcess terminate.  "выключаем фоновый процесс"
	backgroundProcess  := nil.
	sendTime := nil.
	"sds"
	sendQueue := nil.
	^true!

connect

	| sock |	
	sock := Socket2 port: self calleePort address: self calleeAddress.	
	self forkConnector: sock.	
!

createComponents
	"Create the presenters contained by the receiver"
	
	super createComponents.
	"calleeAddress := self add: TextPresenter new name: 'calleeAddress'.	
	outgoingMessage := self add: TextPresenter new name: 'outgoingMessage'.
	status := #initializing asValue."

	calleeAddress value: '127.0.0.1'.
	calleePort := self add: TextPresenter new name: 'calleePort'.
	calleePort value: self defaultPort.

	tagsPresenter := self add: ListPresenter new name: 'tags'.
!

defaultPort

	^2404!

defaultSRequestSeconds
	^20!

enqueueOutboundMessage: anAcpi
	"Private - Enqueue the outbound message represented by the <Assocation> argument 
	to the output queue for subsequent processing by the sender thread.The key of the
	<Association> is the name of the sender, and the value is the message body."

	sendQueue nextPut: anAcpi buffer.
	sendTime := Timestamp current. "дата отправки последнего пакета"
	"[self messageReceived: anAssociation ] postToMessageQueue"
	[self appendMessage: '<- ' , anAcpi displayString] postToMessageQueue!

getASDU: anACPI
	^Asdu new: (anACPI buffer copyFrom: 7)!

gi
	" отправляем команду общего опроса"

	"msg := self userName -> outgoingMessage value."
	self enqueueOutboundMessage: Acpi getGIRequest.

!

model: aOrderedCollection
	"Set the model associated with the receiver to be aVideoLibrary."

	super model: aOrderedCollection.
	"ownerPresenter model: (self model aspectValue: #owner)."
	tagsPresenter model: (ListModel on: aOrderedCollection)!

onViewCreated
	super onViewCreated.
	self halt.!

onViewOpened
	"Private - The receiver is ready to go so start accepting calls."

	super onViewOpened.
	"messageDisplay textLimit: 500000."
	self topShell extent: 640 @ 480.

	"statusItem := self view viewNamed: 'key name' ifNone: [^self].
	statusModel := ValueHolder new.
	statusItem model: statusModel"!

process: anACPI from: aReadStream
	| buf |
	anACPI isHaveASDU
		ifTrue: 
			["пришел пакет с полезной информацией"
			buf := self getASDU: anACPI.
			self enqueueInboundMessage: String lineDelimiter , buf displayString.	"вывод ASDU"
			buf tags isNil
				ifFalse: 
					[self enqueueInboundMessage: String lineDelimiter , buf tags displayString , String lineDelimiter.
					self addTags: buf tags]]
		ifFalse: 
			[buf := anACPI getResponseU.	"команда ответа на пришедший пакет"
			buf isNil ifFalse: [self enqueueOutboundMessage: (Acpi fromArray: buf)]]	"вывод тега"!

queryCommand: query
	"Private - Enters details about a potential command for the receiver into the 
	<CommandQuery>,  query."

	| cmd |
	cmd := query commandSymbol.
	cmd == #gi
		ifTrue: 
			[query isEnabled: self isConnected.
			^true].
	^super queryCommand: query!

receiveMessagesOn: aReadStream
	
	["| filer | filer := aReadStream."
	
	[| acpi |
	acpi := Acpi fromStream: aReadStream.	"Полный пакет"
	self enqueueInboundMessage: '-> ' , acpi displayString.
	self process: acpi from: aReadStream]
			repeat]
			on: Stream endOfStreamSignal
			do: 
				[:ex |
				"Transcript show: 'receiveMessagesOn EX-', ex printString ; cr."
				]!

send
	"Send the current outgoing message to the remote socket"

	| anAcpi |
	"msg := self userName -> outgoingMessage value."
	anAcpi := Acpi fromArray: Acpi getStartDTAct.
	self enqueueOutboundMessage: anAcpi.
	outgoingMessage
		clear;
		setFocus!

sendCommand
	"отправка команды из контекстного меню"
	sendQueue nextPut: nil!

senderMain
	"Private - Main loop of sender thread. Note how we use an STBOutFiler to simplify
	transimssion of an arbitrary message format - of course this will only work if the peer is
	also written in Dolphin, but it is secure (unlike the old Chat) because we use a validating
	in-filer to guard against executing code in any malicious messages that might be sent."

	[| filer |
	filer := socket writeStream.		
	
	[| msg |
		msg := sendQueue next.		
		msg isNil ifTrue: [ self error: 'Queue is empty' ].
		filer nextPutAll: msg.			
		filer flush]" The stream is buffered, so we have to flush it or the message won't get transmitted immediately"
	repeat]
	on:  Error "SocketError"
	do: [:ex | self enqueueInboundMessage: 'senderMain EX-', ex printString.]!

setPeriodGI
	countStepGI := 300!

socketConnected: aSocket2
	super socketConnected: aSocket2.
	self appendMessage: '== Подключено'.
	Acpi clearCounters.	"обнуляем счетчики NS NR"
	tagsPresenter model removeAll.
	self setPeriodGI.
	backgroundProcess := [
			[Processor sleep: self backgroundPauseMS.
			self backgroundStep] repeat]
				fork!

statusImage
	^self connect
		ifTrue: [(Icon fromId: 'LiveUpdate.ico') imageIndex]
		ifFalse: [(Icon fromId: 'PowerSwitch.ico') imageIndex]!

validateUserInterface
	"Set the state of the receiver accordingly. This method is sent at judicious times
	in an attempt to ensure that the receivers state is up to date. "

	super validateUserInterface.
	calleePort view isEnabled: self isIdle.	

	! !
!Mek104 categoriesFor: #acceptCalls!private! !
!Mek104 categoriesFor: #addMessage!public! !
!Mek104 categoriesFor: #addTags:!adding!public! !
!Mek104 categoriesFor: #appendMessage:!private! !
!Mek104 categoriesFor: #backgroundPauseMS!accessing!public! !
!Mek104 categoriesFor: #backgroundStep!accessing!public! !
!Mek104 categoriesFor: #calleePort!accessing!public! !
!Mek104 categoriesFor: #clearMessage!public! !
!Mek104 categoriesFor: #closeConnection!private! !
!Mek104 categoriesFor: #connect!commands!public! !
!Mek104 categoriesFor: #createComponents!public! !
!Mek104 categoriesFor: #defaultPort!public! !
!Mek104 categoriesFor: #defaultSRequestSeconds!accessing!public! !
!Mek104 categoriesFor: #enqueueOutboundMessage:!private! !
!Mek104 categoriesFor: #getASDU:!public! !
!Mek104 categoriesFor: #gi!public! !
!Mek104 categoriesFor: #model:!public! !
!Mek104 categoriesFor: #onViewCreated!public! !
!Mek104 categoriesFor: #onViewOpened!private! !
!Mek104 categoriesFor: #process:from:!public! !
!Mek104 categoriesFor: #queryCommand:!private! !
!Mek104 categoriesFor: #receiveMessagesOn:!public! !
!Mek104 categoriesFor: #send!commands!public! !
!Mek104 categoriesFor: #sendCommand!public! !
!Mek104 categoriesFor: #senderMain!private! !
!Mek104 categoriesFor: #setPeriodGI!public! !
!Mek104 categoriesFor: #socketConnected:!public! !
!Mek104 categoriesFor: #statusImage!public! !
!Mek104 categoriesFor: #validateUserInterface!public! !

!Mek104 class methodsFor!

defaultModel
	"Answer a default model to be assigned to the receiver when it
	is initialized."

	^ OrderedCollection new!

icon
	"Generated from:
	self createIconMethod: #icon ofSize: 48@48 fromFile: 'D:\Spc\Dolphin\Resources\iec104.png'.
	"
	^InternalIcon fromBytes: #[137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 6 0 0 0 87 2 249 135 0 0 0 1 115 82 71 66 0 174 206 28 233 0 0 0 4 103 65 77 65 0 0 177 143 11 252 97 5 0 0 0 9 112 72 89 115 0 0 14 195 0 0 14 195 1 199 111 168 100 0 0 2 75 73 68 65 84 104 67 237 152 189 74 196 64 20 133 125 8 159 195 231 240 49 236 125 0 65 43 193 66 11 107 193 126 11 17 20 193 66 155 109 196 66 4 113 91 101 65 80 65 214 78 27 187 184 103 55 99 78 238 222 153 220 49 193 76 36 7 110 145 220 159 153 111 126 119 179 148 117 92 61 64 219 234 1 218 214 63 2 24 220 102 217 242 102 97 120 182 136 115 124 182 178 155 7 71 232 106 172 215 130 81 223 234 1 132 26 97 139 5 208 106 72 59 186 155 133 22 0 167 247 229 0 60 87 233 230 169 156 179 178 151 101 235 199 139 182 117 158 39 24 84 85 15 239 224 91 0 120 156 148 147 241 92 165 201 103 57 231 238 57 119 252 82 135 215 69 45 116 116 251 34 119 144 222 62 230 32 121 255 210 1 64 199 220 232 194 118 46 115 71 88 233 0 156 140 138 58 0 1 144 65 205 2 104 107 22 203 194 34 140 184 171 131 60 163 154 5 208 204 218 25 196 185 28 43 244 84 233 204 64 18 0 117 246 64 231 1 120 15 24 79 32 40 29 0 140 186 171 211 218 41 84 7 96 252 94 190 7 140 203 40 29 0 136 247 1 96 180 155 248 243 107 30 55 122 153 61 54 11 160 157 66 48 235 166 148 183 177 86 207 249 113 241 77 85 15 0 163 193 13 250 12 13 91 229 126 235 132 234 194 55 122 157 133 215 3 128 208 24 231 105 22 113 170 252 104 248 160 215 130 81 223 10 128 142 170 7 104 91 61 64 219 170 6 144 127 246 247 135 185 35 160 181 65 57 199 153 85 171 7 139 185 158 143 12 97 0 173 80 8 192 242 149 162 106 0 224 215 242 162 0 228 168 179 133 58 128 207 39 90 142 52 128 250 164 197 195 162 0 184 35 214 37 36 71 110 227 44 119 76 37 125 88 98 154 120 233 201 217 143 6 64 1 72 46 11 31 0 55 232 114 89 236 215 62 116 201 129 146 237 70 1 176 172 0 28 195 163 239 132 119 28 35 197 179 142 54 254 20 192 18 131 119 28 131 28 39 134 243 205 124 178 0 50 215 247 62 89 0 222 27 188 244 58 1 128 78 185 103 185 177 59 1 32 223 89 12 121 164 102 0 32 142 177 158 66 242 157 197 196 76 52 7 192 199 96 213 61 224 252 73 205 128 28 77 142 11 249 124 146 237 70 237 1 30 173 144 241 79 2 217 160 207 180 91 88 83 45 0 94 14 33 147 191 105 44 75 66 44 1 175 254 124 6 88 49 177 62 213 2 232 144 122 128 118 149 101 223 15 56 138 81 188 43 202 2 0 0 0 0 73 69 78 68 174 66 96 130]!

initialize
	"Private - Initialize the receiver's class variables
		self initialize
	"
	
	Statii := IdentityDictionary new.
	Statii
		at: #initializing put: 'Инициализация...';
		at: #listening put: 'Ожидание входящих...';
		at: #waiting put: 'Ожидание действий...';
		at: #connecting put: 'Соединение...';
		at: #connected put: 'Соединено';
		shrink!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 560 nil 721158 ##(Smalltalk.SystemColor) 31 328198 ##(Smalltalk.Point) 1041 241 5 nil nil nil 560 852230 ##(Smalltalk.FramingLayout) 170 176 34 20 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 752 nil nil nil 5 nil nil nil 752 nil 8 4294903005 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 658 29 91 658 155 51 752 930 #text: 34 1 8 'Сообщение:' 752 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 0 45 0 0 0 91 0 0 0 70 0 0 0] 8 #() 658 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 29 1170 #fixedViewLeft 155 1170 #fixedParentTop 91 1170 #fixedViewTop 51 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 1248 nil nil nil 5 nil nil nil 1248 nil 8 4294903005 834 nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 21 21 658 201 51 1248 930 #text: 34 1 8 'Адрес сервера' 1248 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 110 0 0 0 35 0 0 0] 8 #() 1120 nil 27 1138 1184 21 1200 201 1216 21 1232 51 410 ##(Smalltalk.TextEdit) 34 16 nil 560 34 2 8 1140916224 1025 1568 nil 482 512 nil 5 nil nil nil 1568 nil 8 4294903333 834 nil nil 1 866 138 144 34 2 930 #createAt:extent: 34 2 658 561 11 658 151 51 1568 930 #text: 34 1 8 '2404' 1568 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 5 0 0 0 99 1 0 0 30 0 0 0] 8 #() 1120 nil 27 1138 1184 561 1200 151 1216 11 1232 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 1904 nil 482 512 nil 5 nil nil nil 1904 nil 8 4294902069 1180998 4 ##(Smalltalk.CommandDescription) #connect 8 '&Подключить' 1 5 nil nil true nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 891 81 658 161 51 1904 930 #text: 34 1 8 '&Подключить' 1904 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 1 0 0 40 0 0 0 13 2 0 0 65 0 0 0] 8 #() 1120 nil 29 1138 1170 #fixedParentRight -329 1200 161 1216 81 1232 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 2288 nil 482 512 nil 5 nil nil nil 2288 nil 8 4294902069 2002 #gi 8 '&GI' 1 1 nil nil false nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 1051 161 658 161 51 2288 930 #text: 34 1 8 '&GI' 2288 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 2 0 0 80 0 0 0 93 2 0 0 105 0 0 0] 8 #() 1120 nil 29 1138 2272 -169 1200 161 1216 161 1232 51 410 ##(Smalltalk.StaticText) 34 16 nil 560 34 2 8 1140850944 1 2640 nil nil nil 5 nil nil nil 2640 nil 8 4294903005 834 nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 491 11 658 71 51 2640 930 #text: 34 1 8 'Порт' 2640 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 5 0 0 0 24 1 0 0 30 0 0 0] 8 #() 1120 nil 27 1138 1184 491 1200 71 1216 11 1232 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 2960 nil 482 512 nil 5 nil nil nil 2960 nil 8 4294902069 2002 #disconnect 8 '&Отключить' 1 1 nil nil false nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 891 161 658 161 51 2960 930 #text: 34 1 8 '&Отключить' 2960 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 1 0 0 80 0 0 0 13 2 0 0 105 0 0 0] 8 #() 1120 nil 29 1138 2272 -329 1200 161 1216 161 1232 51 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 560 34 2 8 1143017796 1025 3312 nil 482 512 nil 5 nil nil nil 3312 nil 8 4294903333 834 nil nil 9 866 138 144 34 1 930 #createAt:extent: 34 2 658 231 81 658 651 131 3312 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 115 0 0 0 40 0 0 0 184 1 0 0 105 0 0 0] 8 #() 1120 nil 27 1138 1184 231 2272 -339 1216 81 1232 131 410 ##(Smalltalk.TextEdit) 34 16 nil 560 34 2 8 1140916224 1025 3600 nil 482 512 nil 5 nil nil nil 3600 nil 8 4294903333 834 nil nil 1 866 138 144 34 2 930 #createAt:extent: 34 2 658 231 11 658 191 51 3600 930 #text: 34 1 8 '127.0.0.1' 3600 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 115 0 0 0 5 0 0 0 210 0 0 0 30 0 0 0] 8 #() 1120 nil 27 1138 1184 231 1200 191 1216 11 1232 51 410 ##(Smalltalk.PushButton) 34 20 nil 560 34 2 8 1140924416 1 3936 nil 482 512 nil 5 nil nil nil 3936 nil 8 4294902069 2002 #send 8 '&StartDT' 1 1 nil nil false nil nil nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 1051 81 658 161 51 3936 930 #text: 34 1 8 '&StartDT' 3936 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 2 0 0 40 0 0 0 93 2 0 0 65 0 0 0] 8 #() 1120 nil 29 1138 2272 -169 1200 161 1216 81 1232 51 170 192 34 6 1568 8 'calleePort' 3312 8 'outgoingMessage' 3600 8 'calleeAddress' 590342 ##(Smalltalk.Rectangle) 658 1 1 658 1 1 866 138 144 34 1 930 #createAt:extent: 34 2 658 1 1 658 1221 241 560 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 98 2 0 0 120 0 0 0] 34 10 1904 3312 3936 2960 2640 752 1248 1568 3600 2288 1120 nil 27 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1140850700 1 4592 nil 482 512 nil 5 nil nil nil 4592 nil 8 4294903009 170 192 34 6 853766 ##(Smalltalk.StatusBarItem) 513 -1 4592 nil ##(Smalltalk.BasicListAbstract) 930 #statusImage 8 #() ##(Smalltalk.Mek104) nil 8 'icon' 4722 1 -1 4592 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 4720 #defaultGetImageBlock 417085891 8 #[30 105 226 0 106] #iconImageIndex 4880 7 257 nil nil 8 'picture' 4722 1 -1 4592 nil 4866 0 nil 4898 2 3 4720 #defaultGetTextBlock 468040131 8 #[30 105 226 0 106] #displayString 4976 7 257 nil 4866 0 nil 1180966 ##(Smalltalk.CompiledExpression) 8 1 4864 8 'doIt' 8 '[:st |   ''Соединено'' = st 
	ifTrue: [ (Icon fromId: ''LiveUpdate.ico'')    imageIndex ] 
	ifFalse: [ (Icon fromId: ''PowerSwitch.ico'' ) imageIndex ] ]' 8 #[36 105 29 17 132 122 46 31 179 162 106 46 35 179 162 106] 8 'Соединено' 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'LiveUpdate.ico' #fromId: #imageIndex #ifTrue:ifFalse: 8 'PowerSwitch.ico' 5024 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) 8 'status' 34 1 4960 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 4592 nil nil 866 138 144 34 1 930 #createAt:extent: 34 2 658 1 601 658 1221 41 4592 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 44 1 0 0 98 2 0 0 64 1 0 0] 8 #() 1120 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 5456 nil nil nil 5 nil nil nil 5456 530 1 1 410 ##(Smalltalk.StatusBar) 34 18 nil 5456 34 2 8 1417677068 1 5536 nil 482 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 658 193 193 nil 5536 nil 8 4294903009 170 192 8 #() 34 1 4722 513 -1 5536 nil 8 'sdfsd' 4818 #iconImageIndex 8 #() 5216 5266 513 1 5536 nil nil 866 138 144 34 1 930 #createAt:extent: 34 2 658 1 1 658 1221 45 5536 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 98 2 0 0 22 0 0 0] 8 #() 1120 nil 27 nil nil nil 410 ##(Smalltalk.CardContainer) 34 16 nil 5456 34 2 8 1409286144 131073 6032 nil 482 8 4278190080 nil 5 nil nil nil 6032 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Log' 410 ##(Smalltalk.RichTextEdit) 34 18 nil 6032 34 2 8 1143017796 1025 6240 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 482 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 2002 #addMessage 8 'Добавить' 1 1 nil nil nil 6450 1 2002 #clearMessage 8 'Очистить' 1 1 nil nil nil 6450 1 2002 #sendCommand 8 'Команда' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 6240 nil 8 1842840059 834 nil nil 11 nil 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 48 0 160 1] 866 138 144 34 4 930 #createAt:extent: 34 2 658 9 55 658 1205 255 6240 930 #contextMenu: 34 1 6416 6240 930 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1251\deff0{\fonttbl{\f0\fswiss\fprq2\fcharset204 Trebuchet MS;}}
{\colortbl ;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang1049\f0\fs20 
\par }
' 6240 930 #resetCharFormat 5760 6240 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 94 2 0 0 154 0 0 0] 8 #() 1120 nil 27 6194 8 'Tags' 410 ##(Smalltalk.ListView) 34 30 nil 6032 34 2 8 1140920653 1025 7024 590662 2 ##(Smalltalk.ListModel) 138 144 5760 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 nil 5 nil nil nil 7024 nil 8 4294902861 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 5216 nil nil nil nil nil nil 138 144 34 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Тег (IOA)' 221 #left ##(Smalltalk.BasicListAbstract) 4818 #<= 4768 4818 #tagname 4768 nil 7024 930 #statusImage 4768 ##(Smalltalk.ApduTag) 1 4866 0 nil 5042 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:x | ''привет мир'']' 8 #[30 105 29 106] 8 'привет мир' 7344 7 257 nil nil 7250 8 'ASDU' 91 #left ##(Smalltalk.BasicListAbstract) 4818 #<= 8 #() 4818 #ASDU 8 #() nil 7024 nil 1 nil nil 7250 8 'Значение' 201 #left ##(Smalltalk.BasicListAbstract) 4818 #<= 4768 4818 #value 4768 nil 7024 nil 1 nil nil 7250 8 'Метка времени' 401 #left ##(Smalltalk.BasicListAbstract) 4818 #<= 7488 4818 #timestamp 4768 nil 7024 nil 1 nil nil 7250 8 'Качество' 221 #left 4818 #displayString 8 #() 4818 #<= 7712 4818 #quality 7712 nil 7024 nil 1 nil nil #report 5760 nil 131169 658 129 129 34 4 nil nil 658 1 1 nil 866 138 144 34 2 930 #createAt:extent: 34 2 658 9 55 658 1205 255 7024 930 #text: 34 1 8 'Тег (IOA)' 7024 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 94 2 0 0 154 0 0 0] 8 #() 1120 nil 27 7024 170 192 34 4 6240 8 'messageDisplay' 7024 8 'tags' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6032 34 2 8 1140916800 1 8080 7090 138 144 34 2 6224 7008 nil 7152 nil nil 1 nil nil nil 8080 nil 8 4294903015 4866 0 nil 4898 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 8208 7 257 nil 4866 0 nil 4898 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 8256 7 257 nil 5216 nil nil nil nil nil #noIcons nil nil nil nil nil 866 138 144 34 3 930 #createAt:extent: 34 2 658 1 1 658 1221 317 8080 930 #basicSelectionsByIndex: 34 1 8 #(2) 8080 930 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 8080 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 98 2 0 0 158 0 0 0] 8 #() 1120 nil 27 866 138 144 34 1 930 #createAt:extent: 34 2 658 1 45 658 1221 317 6032 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 98 2 0 0 180 0 0 0] 34 3 6240 7024 8080 1120 nil 27 170 192 5760 nil 866 138 144 34 1 930 #createAt:extent: 34 2 658 1 241 658 1221 361 5456 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 120 0 0 0 98 2 0 0 44 1 0 0] 34 2 6032 5536 1120 nil 27 170 192 5760 nil 866 138 144 34 1 930 #createAt:extent: 34 2 658 3839 21 658 1221 641 416 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 225 9 0 0 74 1 0 0] 34 3 560 5456 4592 1120 nil 27)! !
!Mek104 class categoriesFor: #defaultModel!public! !
!Mek104 class categoriesFor: #icon!constants!public! !
!Mek104 class categoriesFor: #initialize!private! !
!Mek104 class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

