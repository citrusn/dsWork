| package |
package := Package name: 'wave'.
package paxVersion: 1;
	basicComment: 'PILOT_FREQ = 807.2 	
SYNC1_FREQ = 2623.7 	
SYNC2_FREQ = 2381.0 	
BIT0_FREQ    = 2046.8  	
BIT1_FREQ    = 1023.4 	'.


package classNames
	add: #Wave;
	add: #WAVE_HEADER;
	add: #ZX_DATA_HEADER;
	add: #ZX_PROGRAM_HEADER;
	add: #ZX_SCREEN_HEADER;
	yourself.

package methodNames
	add: 'File class' -> #readBytes:count:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\ImagePPM\ppm';
	yourself).

package!

"Class Definitions"!

Object subclass: #Wave
	instanceVariableNames: 'waveHeader data fileName signal intervals screen'
	classVariableNames: 'BIT0_FREQ BIT1_FREQ PILOT_FREQ SYNC1_FREQ SYNC2_FREQ'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #WAVE_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ZX_DATA_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ZX_PROGRAM_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ZX_SCREEN_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!File class methodsFor!

readBytes: aString count: anInteger
	"Open the file at the specified path, read and answer all of the binary data it contains.
	The file is guaranteed to be closed, even if an exception occurs."

	| file |
	file := self
				open: aString
				mode: #read
				check: true
				share: #read.
	^
	[| buffer |
	buffer := ByteArray newFixed: anInteger.
	file read: buffer count: anInteger.
	buffer]
			ensure: [file close]! !
!File class categoriesFor: #readBytes:count:!file operations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Wave guid: (GUID fromString: '{9091e678-c267-4de4-a119-ff2e20d345ee}')!
Wave comment: 'wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\3DDeathChase(1983)(Micromega).wav''
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\Chaos.wav''
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\Frogger-Deanysoft.wav''. 
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\HarrierAttack!!(1983)(DurellSoftware).wav''. 
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\Hobbit,Thev1.2(1982)(MelbourneHouse).wav'' 
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\Manic_Miner.wav'' 
wav := Wave from: ''D:\Spc\Dolphin\Core\SDS\WaveFile\Space_Raiders.wav''
wav processIntervals2 
wav screen
wav waveHeader
wav signal  
wav data 
(wav intervals  copyFrom: 36567 to: 45000)
wav intervals  copyFrom: 147150 to: 150000
TrendPresenter showOn: (wav signal copyFrom:  9950 to: 10660 )
10660 конец первого блока
TrendPresenter showOn: (wav data copyFrom: 120000 to: 143000) 

44100 /3600.0 = 12 часов 15 минут одна секунда в масштабе 44_100 кГц
'!
!Wave categoriesForClass!Kernel-Objects! !
!Wave methodsFor!

buildByte: anIndex
	"Private "

	| byte |
	byte := 0.
	0 to: 7
		do: 
			[:i |
			byte := byte << 1.
			(self checkBit1: (intervals at: anIndex + (i << 1)) and: (intervals at: anIndex + ((i << 1) + 1)))
				ifTrue: [byte := byte + 1]].
	^byte!

checkBit0: prev and: cur
	^prev + cur between: BIT0_FREQ - 2 and: BIT0_FREQ + 2!

checkBit1: prev and: cur
	^prev + cur between: BIT1_FREQ - 3 and: BIT1_FREQ + 3!

checkPilot: prev and: cur
	^prev + cur between: PILOT_FREQ - 3 and: PILOT_FREQ + 3!

checkScreenBlock: zxHeader
	"self halt."
	zxHeader class name = #ZX_SCREEN_HEADER
		ifTrue: [(zxHeader datalength = 6912 "and: [zxHeader startaddr = 16384]" ) ifTrue: [^true]].
	^false!

checkSync: prev and: cur
	^prev + cur between: SYNC1_FREQ - 2 and: SYNC1_FREQ + 1!

data
	^data!

data: anObject
	data := anObject!

durationSeconds
	^waveHeader subchunk2Size // (waveHeader bitsPerSample / 8) 
		// waveHeader numChannels // waveHeader sampleRate!

fileName
	^fileName!

fileName: anFileName
	| buf |
	fileName := anFileName.
	buf := File readAllBytes: fileName.
	waveHeader := WAVE_HEADER fromBytes: (buf copyFrom: 1 to: 44).
	signal := OrderedCollection new.
	self setDataPoint: buf from: 44!

fillByteBlock: aByteArray count: anInteger from: anOffset start: aStartInteger
	"  заполняет aByteArray байтами  со смещения anOffset числом anInteger из буфера интервалов, начиная с указателя aStartInteger"

	| byte i offset |
	i := aStartInteger.
	offset := anOffset.
	anInteger timesRepeat: 
			[byte := self buildByte: i.
			aByteArray at: offset put: byte.
			offset := offset + 1.
			i := i + 16].
	"указатель на следующий байт"
	^i!

findSync: aByteValue start: anIndex
	"find sync pulse. Return end sync position"

	anIndex to: self intervals size - 1
		do: 
			[:idx |
			(self checkSync: (intervals at: idx) and: (intervals at: idx + 1))
				ifTrue: 
					[Transcript
						show: 'Sync find at: ';
						show: idx displayString;
						cr.
					^idx + 2]].
	^0!

freq
	^waveHeader sampleRate!

getIntervals
	| p1 p2 |
	signal isEmpty ifTrue: 
			[MessageBox confirm: 'Signal is empty!!'.
			^self].
	intervals := OrderedCollection new.
	p1 := signal first.
	2 to: signal size do: [:i |
			p2 := signal at: i.
			p2 x = p1 x ifTrue: [intervals add: p2 y - p1 y].
			p1 := p2]
	"signal do: [:each || prev |
			prev isNil ifTrue: [prev := each]
					ifFalse: 
						[prev x = each x ifTrue: [intervals add: each y - prev y].
						prev = each]]"!

getLengthBlock: aByteArray
	aByteArray first = 255 ifTrue: [^ZX_DATA_HEADER byteSize].
	aByteArray first = 0
		ifTrue: 
			[aByteArray second = 0 ifTrue: [^ZX_PROGRAM_HEADER byteSize].
			aByteArray second = 3 ifTrue: [^ZX_SCREEN_HEADER byteSize]].
	^0!

getTypeBlock: aByteArray
	aByteArray first = 255 ifTrue: [^ZX_DATA_HEADER fromBytes: aByteArray ].
	aByteArray first = 0
		ifTrue: 
			[aByteArray second = 0 ifTrue: [^ZX_PROGRAM_HEADER  fromBytes: aByteArray ].
			aByteArray second = 3 ifTrue: [^ZX_SCREEN_HEADER fromBytes: aByteArray]].
	^0!

intervals
	intervals ifNil: [self getIntervals].
	^intervals!

processIntervals
	| prev state cur |
	Transcript show: 'Wait pilot....'; cr.
	state := 0.
	data := nil. data := OrderedCollection new.
	prev := self intervals first. 
	2 to: 5670 do: [:i || c r|
		cur :=self intervals at: i. 
		state = 0 ifTrue: [(self checkPilot: prev and: cur) ifTrue: [state := 1. c := 0. Transcript show: 'Pilot found at: ' , i displayString; cr]].
		state = 1 ifTrue: [(self checkPilot: prev and: cur) 
				ifTrue: [c := c + 1]
				ifFalse: [c > 1000 ifTrue:  [state := 2. c := 0. Transcript show: 'Pilot ended at: ' , i displayString; cr]
							ifFalse: [state := 0]]].
		state = 2 ifTrue: [(self checkSync: prev and: cur) 
					ifTrue: [state := 3. c := 0.  r:=0. Transcript show: 'Syncro ended at: ' , i displayString;	cr]
					ifFalse: [c := c + 1. c > 3 ifTrue: [state := 0]]].
		state = 3 ifTrue: [ (self checkBit1: prev and: cur) ifTrue: [r:=r << 1 + 1.  c := c+1 ].
					(self checkBit0: prev and: cur) ifTrue: [c := c+1]. 
					c=8 ifTrue: [self halt. data add: r. r :=0. c:=0 ]].
		prev := cur].
	state = 0 ifTrue: [Transcript show: 'Pilot not found in data....'; cr].
	state = 1 ifTrue: [Transcript show: 'Pilot not ended in data....'; cr].
	state = 2 ifTrue: [Transcript show: 'Synchro not found in data....'; cr].
	state = 3 ifTrue: [Transcript show: 'Data reading: ', data size displayString ; cr].!

processIntervals2
	| i r offset |
	Transcript	show: 'Find sync....';	cr.
	data := nil. data := ByteArray new: 50.
	i :=1. 
	[ i> 0 and: [i< self  intervals size] ] whileTrue: [
		i := self findSync: (SYNC1_FREQ / 2.0) rounded start: i. 
		i = 0 ifTrue: [ ^data]. 
		offset := 1.   
		i:=self fillByteBlock: data count: 2 from: offset start: i.  offset := offset  + 2.
		r := (self getLengthBlock: data)-2.
		i:=self fillByteBlock: data count: r from: offset start: i. 
		r:=self getTypeBlock: data.
		(self checkScreenBlock: r) ifTrue: [screen := ByteArray new: 6912.  i:=self fillByteBlock: screen count: 6912 from: 1 start: (36567+2+16).   self saveScreenToPpm: screen ].
		Transcript show: r displayString; cr
	].
	!

saveScreenToPpm: aByteArray
	"  Ppm  fromPPMFile:  'D:\Spc\Dolphin\Core\SDS\waveFile\empty.ppm'  "

	| ppm |
	ppm := PpmFile fromZxScrBlock: aByteArray.
	ppm saveToFile: "'D:\Spc\Dolphin\Core\SDS\waveFile\' ," fileName , '.ppm'!

screen
	^screen!

screen: anObject
	screen := anObject!

setDataPoint: aBuffer from: anInteger	
	signal removeAll.
	1 to: waveHeader subchunk2Size do: 
			[:i || newPoint prevPoint point |
			point := aBuffer at: i + anInteger.
			newPoint :=  (point > 128 ifTrue: [1.0] ifFalse: [0.0]) @i. 
			signal isEmpty
				ifTrue:  [signal add: newPoint]
				ifFalse: 
					[prevPoint x ~= newPoint x
						ifTrue: [signal	add: prevPoint;	add: newPoint ]	].
			prevPoint := newPoint].			
	Transcript	show: 'signal: ' , signal size displayString ; cr!

signal
	^signal!

signal: anObject
	signal := anObject!

waveHeader
	^waveHeader! !
!Wave categoriesFor: #buildByte:!private! !
!Wave categoriesFor: #checkBit0:and:!private! !
!Wave categoriesFor: #checkBit1:and:!private! !
!Wave categoriesFor: #checkPilot:and:!private! !
!Wave categoriesFor: #checkScreenBlock:!accessing!public! !
!Wave categoriesFor: #checkSync:and:!private! !
!Wave categoriesFor: #data!accessing!public! !
!Wave categoriesFor: #data:!accessing!public! !
!Wave categoriesFor: #durationSeconds!public! !
!Wave categoriesFor: #fileName!accessing!public! !
!Wave categoriesFor: #fileName:!accessing!public! !
!Wave categoriesFor: #fillByteBlock:count:from:start:!public! !
!Wave categoriesFor: #findSync:start:!public! !
!Wave categoriesFor: #freq!public! !
!Wave categoriesFor: #getIntervals!public! !
!Wave categoriesFor: #getLengthBlock:!accessing!public! !
!Wave categoriesFor: #getTypeBlock:!accessing!public! !
!Wave categoriesFor: #intervals!accessing!public! !
!Wave categoriesFor: #processIntervals!public! !
!Wave categoriesFor: #processIntervals2!public! !
!Wave categoriesFor: #saveScreenToPpm:!public! !
!Wave categoriesFor: #screen!accessing!private! !
!Wave categoriesFor: #screen:!accessing!private! !
!Wave categoriesFor: #setDataPoint:from:!private! !
!Wave categoriesFor: #signal!accessing!private! !
!Wave categoriesFor: #signal:!accessing!private! !
!Wave categoriesFor: #waveHeader!accessing!public! !

!Wave class methodsFor!

from: aFileName

	^ self new fileName: aFileName.!

initialize
	"44100 частота дискритезации wav файла self initialize"
	super initialize.
	PILOT_FREQ := 44100.0 / 807.2 roundTo: 0.1.
	SYNC1_FREQ := 44100.0 / 2623.7 roundTo: 0.1.
	SYNC2_FREQ := 44100.0 / 2381 roundTo: 0.1.
	BIT0_FREQ := 44100.0 / 2046.8 roundTo: 0.1.
	BIT1_FREQ := 44100.0 / 1023.4 roundTo: 0.1! !
!Wave class categoriesFor: #from:!public! !
!Wave class categoriesFor: #initialize!public! !

WAVE_HEADER guid: (GUID fromString: '{f33882f5-fa79-4767-9273-f7085fedf3e4}')!
WAVE_HEADER comment: 'WAVE_HEADER compileDefinition.

buf := File readBytes: ''D:\Spc\Dolphin\Core\SDS\WaveFile\HarrierAttack!!(1983)(DurellSoftware).wav'' count:44.
wave := WAVE_HEADER fromBytes: buf. 

0..3 (4 байта) chunkId Содержит символы “RIFF” в ASCII кодировке (0x52494646 в big-endian представлении). Является началом RIFF-цепочки. 
4..7 (4 байта) chunkSize Это оставшийся размер цепочки, начиная с этой позиции. Иначе говоря, это размер файла – 8, то есть, исключены поля chunkId и chunkSize. 
8..11 (4 байта) format Содержит символы “WAVE” (0x57415645 в big-endian представлении) 
12..15 (4 байта) subchunk1Id Содержит символы “fmt “ (0x666d7420 в big-endian представлении) 
16..19 (4 байта) subchunk1Size 16 для формата PCM. Это оставшийся размер подцепочки, начиная с этой позиции. 
20..21 (2 байта) audioFormat Аудио формат, полный список можно получить здесь. Для PCM = 1 (то есть, Линейное квантование). Значения, отличающиеся от 1, обозначают некоторый формат сжатия. 
22..23 (2 байта) numChannels Количество каналов. Моно = 1, Стерео = 2 и т.д. 
24..27 (4 байта) sampleRate Частота дискретизации. 8000 Гц, 44100 Гц и т.д. 
28..31 (4 байта) byteRate Количество байт, переданных за секунду воспроизведения. 
32..33 (2 байта) blockAlign Количество байт для одного сэмпла, включая все каналы. 
34..35 (2 байта) bitsPerSample Количество бит в сэмпле. Так называемая “глубина” или точность звучания. 8 бит, 16 бит и т.д. 
36..39 (4 байта) subchunk2Id Содержит символы “data” (0x64617461 в big-endian представлении) 
40..43 (4 байта) subchunk2Size Количество байт в области данных. 
44.. data Непосредственно WAV-данные 
'!
!WAVE_HEADER categoriesForClass!External-Data-Structured! !
!WAVE_HEADER methodsFor!

audioFormat
	"Answer the <Integer> value of the receiver's 'audioFormat' field."

	^bytes wordAtOffset: ##(self offsetOf: #audioFormat)!

audioFormat: anInteger
	"Set the receiver's 'audioFormat' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #audioFormat) put: anInteger!

bitsPerSample
	"Answer the <Integer> value of the receiver's 'bitsPerSample' field."

	^bytes wordAtOffset: ##(self offsetOf: #bitsPerSample)!

bitsPerSample: anInteger
	"Set the receiver's 'bitsPerSample' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #bitsPerSample) put: anInteger!

blockAlign
	"Answer the <Integer> value of the receiver's 'blockAlign' field."

	^bytes wordAtOffset: ##(self offsetOf: #blockAlign)!

blockAlign: anInteger
	"Set the receiver's 'blockAlign' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #blockAlign) put: anInteger!

byteRate
	"Answer the <Integer> value of the receiver's 'byteRate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #byteRate)!

byteRate: anInteger
	"Set the receiver's 'byteRate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #byteRate) put: anInteger!

chunkId
	"Answer the <ByteArray> value of the receiver's 'chunkId' field."

	^String fromAddress: bytes yourAddress length: 4!

chunkId: aByteArray
	"Set the receiver's 'chunkId' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(4 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: 1
		to: size
		startingAt: 1!

chunkSize
	"Answer the <Integer> value of the receiver's 'chunkSize' field."

	^bytes dwordAtOffset: ##(self offsetOf: #chunkSize)!

chunkSize: anInteger
	"Set the receiver's 'chunkSize' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #chunkSize) put: anInteger!

format
	"Answer the <ByteArray> value of the receiver's 'format' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #format) length: 4!

format: aByteArray
	"Set the receiver's 'format' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(4 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: ##((self offsetOf: #format) + 1)
		to: ##(self offsetOf: #format) + size
		startingAt: 1!

numChannels
	"Answer the <Integer> value of the receiver's 'numChannels' field."

	^bytes wordAtOffset: ##(self offsetOf: #numChannels)!

numChannels: anInteger
	"Set the receiver's 'numChannels' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #numChannels) put: anInteger!

sampleRate
	"Answer the <Integer> value of the receiver's 'sampleRate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #sampleRate)!

sampleRate: anInteger
	"Set the receiver's 'sampleRate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #sampleRate) put: anInteger!

subchunk1Id
	"Answer the <ByteArray> value of the receiver's 'subchunk1Id' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #subchunk1Id) length: 4!

subchunk1Id: aByteArray
	"Set the receiver's 'subchunk1Id' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(4 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: ##((self offsetOf: #subchunk1Id) + 1)
		to: ##(self offsetOf: #subchunk1Id) + size
		startingAt: 1!

subchunk1Size
	"Answer the <Integer> value of the receiver's 'subchunk1Size' field."

	^bytes dwordAtOffset: ##(self offsetOf: #subchunk1Size)!

subchunk1Size: anInteger
	"Set the receiver's 'subchunk1Size' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #subchunk1Size) put: anInteger!

subchunk2Id
	"Answer the <ByteArray> value of the receiver's 'subchunk2Id' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #subchunk2Id) length: 4!

subchunk2Id: aByteArray
	"Set the receiver's 'subchunk2Id' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(4 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: ##((self offsetOf: #subchunk2Id) + 1)
		to: ##(self offsetOf: #subchunk2Id) + size
		startingAt: 1!

subchunk2Size
	"Answer the <Integer> value of the receiver's 'subchunk2Size' field."

	^bytes dwordAtOffset: ##(self offsetOf: #subchunk2Size)!

subchunk2Size: anInteger
	"Set the receiver's 'subchunk2Size' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #subchunk2Size) put: anInteger! !
!WAVE_HEADER categoriesFor: #audioFormat!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #audioFormat:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #bitsPerSample!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #bitsPerSample:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #blockAlign!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #blockAlign:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #byteRate!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #byteRate:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #chunkId!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #chunkId:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #chunkSize!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #chunkSize:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #format!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #format:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #numChannels!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #numChannels:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #sampleRate!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #sampleRate:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk1Id!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk1Id:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk1Size!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk1Size:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk2Id!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk2Id:!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk2Size!**compiled accessors**!public! !
!WAVE_HEADER categoriesFor: #subchunk2Size:!**compiled accessors**!public! !

!WAVE_HEADER class methodsFor!

defineFields
	self
		defineField: #chunkId type: (ArrayField type: ByteArray length: 4) offset: 0 ;
		defineField: #chunkSize type: DWORDField new;
		defineField: #format type: (ArrayField type: ByteArray length: 4);
		defineField: #subchunk1Id type: (ArrayField type: ByteArray length: 4);
		defineField: #subchunk1Size type: DWORDField new;
		defineField: #audioFormat type: WORDField new;
		defineField: #numChannels type: WORDField new;
		defineField: #sampleRate type: DWORDField new;
		defineField: #byteRate type: DWORDField new;
		defineField: #blockAlign type: WORDField new;
		defineField: #bitsPerSample type: WORDField new;
		defineField: #subchunk2Id type: (ArrayField type: ByteArray length: 4);
		defineField: #subchunk2Size type: DWORDField new.
	"defineField: #chunkId type: (StringField length: 4);"
	"defineField: #format type: (StringField length: 4) ;"
	"defineField: #subchunk1Id type: (StringField length: 4);"
	"defineField: #subchunk2Id type: (StringField length: 4) ;"
	"44.. data Непосредственно WAV-данные "
	self byteSize: 44! !
!WAVE_HEADER class categoriesFor: #defineFields!public! !

ZX_DATA_HEADER guid: (GUID fromString: '{4e10ac85-f721-4267-8d9a-50be7df4d93c}')!
ZX_DATA_HEADER comment: ''!
!ZX_DATA_HEADER categoriesForClass!Unclassified! !
!ZX_DATA_HEADER class methodsFor!

defineFields
	self
		defineField: #flag type: BYTEField new;
		defineField: #data type: BYTEField new!

packing
	" размер структуры до байта "

	^1! !
!ZX_DATA_HEADER class categoriesFor: #defineFields!public! !
!ZX_DATA_HEADER class categoriesFor: #packing!public! !

ZX_PROGRAM_HEADER guid: (GUID fromString: '{4e8e57c0-aaec-422f-953b-d8853ba74b04}')!
ZX_PROGRAM_HEADER comment: 'self compileDefinition
self byteSize
case #1:  program header or  program autostart header - for storing BASIC programs
Offset: 	Field type: 	Length: 	Description: 	Additional information: 
0  		byte 			1 		flag 			always 0. Byte indicating a standard ROM loading header 
1  		byte			1 		data type 		always 0: Byte indicating a program header 
2  		char 			10 		file name 		loading name of the program. filled with spaces (CHR$(32)) 
12 		word			2 		[data length] 	length of the following data (after the header) = length of BASIC program + variables 
14  		word			2 		autostart 		line LINE parameter of SAVE command. Value 32768 means "no auto-loading"; 0..9999 are valid line numbers.  
16  		word 		2 		[program length] length of BASIC program; remaining bytes ([data length] - [program length]) = offset of variables  
18  		byte 			1 		checksum byte 	simply all bytes (including flag byte) XORed  
'!
!ZX_PROGRAM_HEADER categoriesForClass!External-Data-Structured! !
!ZX_PROGRAM_HEADER methodsFor!

autoline
	"Answer the <Integer> value of the receiver's 'autoline' field."

	^bytes wordAtOffset: ##(self offsetOf: #autoline)!

autoline: anInteger
	"Set the receiver's 'autoline' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #autoline) put: anInteger!

chksum
	"Answer the <Integer> value of the receiver's 'chksum' field."

	^bytes byteAtOffset: ##(self offsetOf: #chksum)!

chksum: anInteger
	"Set the receiver's 'chksum' field to the value of the argument, anInteger"

	bytes byteAtOffset: ##(self offsetOf: #chksum) put: anInteger!

datalength
	"Answer the <Integer> value of the receiver's 'datalength' field."

	^bytes wordAtOffset: ##(self offsetOf: #datalength)!

datalength: anInteger
	"Set the receiver's 'datalength' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #datalength) put: anInteger!

filename
	"Answer the <ByteArray> value of the receiver's 'filename' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #filename) length: 10!

filename: aByteArray
	"Set the receiver's 'filename' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(10 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: ##((self offsetOf: #filename) + 1)
		to: ##(self offsetOf: #filename) + size
		startingAt: 1!

flag
	"Answer the <Integer> value of the receiver's 'flag' field."

	^bytes byteAtOffset: 0!

flag: anInteger
	"Set the receiver's 'flag' field to the value of the argument, anInteger"

	bytes byteAtOffset: 0 put: anInteger!

proglength
	"Answer the <Integer> value of the receiver's 'proglength' field."

	^bytes wordAtOffset: ##(self offsetOf: #proglength)!

proglength: anInteger
	"Set the receiver's 'proglength' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #proglength) put: anInteger!

type
	"Answer the <Integer> value of the receiver's 'type' field."

	^bytes byteAtOffset: ##(self offsetOf: #type)!

type: anInteger
	"Set the receiver's 'type' field to the value of the argument, anInteger"

	bytes byteAtOffset: ##(self offsetOf: #type) put: anInteger! !
!ZX_PROGRAM_HEADER categoriesFor: #autoline!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #autoline:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #chksum!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #chksum:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #datalength!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #datalength:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #filename!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #filename:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #flag!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #flag:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #proglength!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #proglength:!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #type!**compiled accessors**!public! !
!ZX_PROGRAM_HEADER categoriesFor: #type:!**compiled accessors**!public! !

!ZX_PROGRAM_HEADER class methodsFor!

defineFields
	self
		defineField: #flag type: BYTEField new;
		defineField: #type type: BYTEField new;
		defineField: #filename type: (ArrayField type: ByteArray length: 10);		
		defineField: #datalength type: WORDField new;
		defineField: #autoline type: WORDField new;
		defineField: #proglength type: WORDField new;
		defineField: #chksum type: BYTEField new
		!

packing
	" размер структуры до байта "

	^1! !
!ZX_PROGRAM_HEADER class categoriesFor: #defineFields!public! !
!ZX_PROGRAM_HEADER class categoriesFor: #packing!public! !

ZX_SCREEN_HEADER guid: (GUID fromString: '{3a016e4c-30b9-4aad-8163-171d27b88dc1}')!
ZX_SCREEN_HEADER comment: 'self compileDefinition
self byteSize
case #4:  byte header or  SCREEN$ header - for storing machine code or screens
Offset: 	Field type: 	Length: 	Description: 	Additional information: 
0  		byte			1 		flag byte 		always 0. Byte indicating a standard ROM loading header 
1  		byte 			1 		data type 		always 3: Byte indicating a byte header 
2  		char 			10 		file name 		loading name of the program. filled with spaces (CHR$(32)) 
12  		world		2 		[data length] 	length of the following data (after the header) in case of a  SCREEN$ header = 6912 
14  		world		2 		start address 	in case of a  SCREEN$ header = 16384  
16  		world 		2 		unused 		= 32768  
17  		byte 			1 		checksum 		byte simply all bytes (including flag byte) XORed '!
!ZX_SCREEN_HEADER categoriesForClass!External-Data-Structured! !
!ZX_SCREEN_HEADER methodsFor!

chksum
	"Answer the <Integer> value of the receiver's 'chksum' field."

	^bytes byteAtOffset: ##(self offsetOf: #chksum)!

chksum: anInteger
	"Set the receiver's 'chksum' field to the value of the argument, anInteger"

	bytes byteAtOffset: ##(self offsetOf: #chksum) put: anInteger!

datalength
	"Answer the <Integer> value of the receiver's 'datalength' field."

	^bytes wordAtOffset: ##(self offsetOf: #datalength)!

datalength: anInteger
	"Set the receiver's 'datalength' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #datalength) put: anInteger!

filename
	"Answer the <ByteArray> value of the receiver's 'filename' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #filename) length: 10!

filename: aByteArray
	"Set the receiver's 'filename' field to the value of the argument, aByteArray"

	| size |
	size := aByteArray byteSize min: ##(10 * ByteArray elementSize).
	aByteArray
		replaceBytesOf: bytes
		from: ##((self offsetOf: #filename) + 1)
		to: ##(self offsetOf: #filename) + size
		startingAt: 1!

flag
	"Answer the <Integer> value of the receiver's 'flag' field."

	^bytes byteAtOffset: 0!

flag: anInteger
	"Set the receiver's 'flag' field to the value of the argument, anInteger"

	bytes byteAtOffset: 0 put: anInteger!

startaddr
	"Answer the <Integer> value of the receiver's 'startaddr' field."

	^bytes wordAtOffset: ##(self offsetOf: #startaddr)!

startaddr: anInteger
	"Set the receiver's 'startaddr' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #startaddr) put: anInteger!

type
	"Answer the <Integer> value of the receiver's 'type' field."

	^bytes byteAtOffset: ##(self offsetOf: #type)!

type: anInteger
	"Set the receiver's 'type' field to the value of the argument, anInteger"

	bytes byteAtOffset: ##(self offsetOf: #type) put: anInteger!

unused
	"Answer the <Integer> value of the receiver's 'unused' field."

	^bytes wordAtOffset: ##(self offsetOf: #unused)!

unused: anInteger
	"Set the receiver's 'unused' field to the value of the argument, anInteger"

	bytes wordAtOffset: ##(self offsetOf: #unused) put: anInteger! !
!ZX_SCREEN_HEADER categoriesFor: #chksum!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #chksum:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #datalength!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #datalength:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #filename!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #filename:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #flag!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #flag:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #startaddr!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #startaddr:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #type!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #type:!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #unused!**compiled accessors**!public! !
!ZX_SCREEN_HEADER categoriesFor: #unused:!**compiled accessors**!public! !

!ZX_SCREEN_HEADER class methodsFor!

defineFields
	self
		defineField: #flag type: BYTEField new;
		defineField: #type type: BYTEField new;
		defineField: #filename type: (ArrayField type: ByteArray length: 10);		
		defineField: #datalength type: WORDField new;
		defineField: #startaddr type: WORDField new;
		defineField: #unused type: WORDField new;
		defineField: #chksum type: BYTEField new
		
!

packing
	" размер структуры до байта "

	^1! !
!ZX_SCREEN_HEADER class categoriesFor: #defineFields!public! !
!ZX_SCREEN_HEADER class categoriesFor: #packing!public! !

"Binary Globals"!

