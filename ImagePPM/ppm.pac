| package |
package := Package name: 'ppm'.
package paxVersion: 1;
	basicComment: '
img:= Bitmap fromPpm:  ''D:\Spc\Dolphin\Core\SDS\ImagePpm\boxes_1.ppm'' .
ImagePresenter showOn: img'.

package basicPackageVersion: '0.001'.


package classNames
	add: #PpmFile;
	yourself.

package methodNames
	add: 'Bitmap class' -> #fromPpm:;
	add: 'Bitmap class' -> #fromPpmFile:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	"add: '..\..\Contributions\rko\Swazoo\Sport6_';"
	yourself).

package!

"Class Definitions"!

Object subclass: #PpmFile
	instanceVariableNames: 'width height lineDelimeter pixelData colors format comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Bitmap class methodsFor!

fromPpm: aPpm
	"  self  fromPPMFile:  'D:\Spc\Dolphin\Core\SDS\ImagePpm\west_1.ppm'  "

	| buffer  result canvas n |	
	buffer := aPpm pixelData.
	result := self displayCompatibleWithExtent: aPpm width @ aPpm height.
	n := 1.
	canvas := result canvas.
	0 to: aPpm height - 1 do: 	[:y |
			0 to: aPpm width - 1	do: 	[:x |
					canvas pixelAt: x @ y put: (buffer at: n).
					n := n + 1]].
	^result!

fromPpmFile: aFileName
	"  self  fromPPMFile:  'D:\Spc\Dolphin\Core\SDS\ImagePpm\west_1.ppm'  "

	| ppm |
	ppm := PpmFile fromFile: aFileName.
	^  self fromPpm: ppm! !
!Bitmap class categoriesFor: #fromPpm:!public! !
!Bitmap class categoriesFor: #fromPpmFile:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

PpmFile guid: (GUID fromString: '{1e927b36-baad-43ab-8b4a-79a91c39154c}')!
PpmFile comment: 'ppm := PpmFile fromFile:  ''D:\Spc\Dolphin\Core\SDS\ImagePPM\house_2.ppm'' 
ppm saveToFile:  ''D:\Spc\Dolphin\Core\SDS\wavefile\empty.ppm'' 

ppm := PpmFile fromZxScrFile:  ''D:\Spc\Dolphin\Core\SDS\ImagePPM\SpaceRaiders.scr'' 
img:= Bitmap fromPpm:  ppm
ImagePresenter showOn: img

img:= Bitmap fromPpmFile:  ''D:\Spc\Dolphin\Core\SDS\wavefile\Space_Raiders.wav.ppm''.
img:= Bitmap fromPpmFile:  ''D:\Spc\Dolphin\Core\SDS\wavefile\Chaos.wav.ppm''.
ImagePresenter showOn: img
'!
!PpmFile categoriesForClass!Kernel-Objects! !
!PpmFile methodsFor!

colorFromZxAttr: aByte
	| b c |
	"self halt."
	"Transcript show: aByte displayString ; cr."
	b := aByte & 7. c:= Color yellow .
	b = 0 ifTrue: [c := Color black].
	b = 1 ifTrue: [c := Color blue].
	b = 2 ifTrue: [c := Color red].
	b = 3 ifTrue: [c := Color magenta].
	b = 4 ifTrue: [c := Color green].
	b = 5 ifTrue: [c := Color cyan].
	b = 6 ifTrue: [c := Color yellow].
	b = 7 ifTrue: [c := Color white].
	^b < 8 ifTrue: [c intensity: 0.75] ifFalse: [c]!

colors
	^colors!

colors: aNumberColors
	aNumberColors = 255 ifFalse: [^InvalidFormat new].
	colors := aNumberColors!

comment
	^comment!

comment: anObject
	comment := anObject!

fillPixelArray: aBuffer
	| n |
	pixelData := Array new: aBuffer size / 3.	" три байта на один пиксель"
	n := 1.
	1 to: aBuffer size  by: 3 do: 
			[:i |
			pixelData at: n
				put: (ColorRef
						fromInteger: (aBuffer at: i) + ((aBuffer at: i + 1) bitShift: 8) + ((aBuffer at: i + 2) bitShift: 16)).
			n := n + 1]!

format
	^format!

format: anObject
	anObject = 'P6' ifFalse: [^InvalidFormat new].
	format := anObject!

height
	^height!

height: anObject
	height := anObject!

initialize	
	pixelData := nil.
	lineDelimeter := $\n.
	format:= 'P6'.
	width:= 256.
	height:= 192.
	colors:= 255.
	comment:= 'Empty PPM File'!

lineDelimeter
	^lineDelimeter!

lineDelimeter: anObject
	lineDelimeter := anObject!

loadFromStream: aStream
	| s size|
	s:= aStream underlyingStream next: 2.
	self format: s.
	lineDelimeter := aStream next.
	s:=aStream upTo: lineDelimeter. s first = $# ifTrue: [comment := s. s := aStream upTo: lineDelimeter]. " комментарий и только одна строка"
	size := s splitString: ' '.
	width := Integer fromString: size first.
	height := Integer fromString: size second.
	s := aStream upTo: lineDelimeter.	
	self colors: (Integer fromString: s).
	aStream underlyingStream beBinary.
	self fillPixelArray: aStream upToEnd.!

pixelData
	^pixelData!

pixelData: anObject
	pixelData := anObject!

pixelDataFromZxBlock: aByteArray
	| attr | 
	attr := [:x1 :y1 || r | r := aByteArray at: 6145 + (y1//8*32) + (x1//8). (  (r & 64 >>3)+(r & 7)) @ ((r & 120) >> 3)].
	pixelData := Array new: 256 * 192.
	
	0 to: 191 do:  [:y |
			0 to: 255 do:  [:x |
					| c i ink paper  a |
					c := aByteArray at: (y // 64 * 2048) + (y & 7 * 256) + (y & 56 >> 3 * 32) + (x//8) + 1.
					a:= (attr value: x value: y). 
					ink :=  self colorFromZxAttr: a x.
					paper := self colorFromZxAttr: a y. 
					i := y * 256 + x + 1. 
					pixelData at: i put: ( c & (1<<(7 - (x\\8))) > 0 ifTrue: [ink] ifFalse: [paper]) ]].
!

saveToFile: aFileName
	| ppm |
	ppm := SpFileStream writingToFilename:  (SpFilename named: aFileName).
	^self saveToStream: ppm !

saveToStream: aStream
	"P6
# CREATOR: The GIMP's PNM Filter Version 1.0
1080 720
255" 
	aStream underlyingStream
		nextPutAll: self format; nextPutAll: self lineDelimeter;
		nextPut: $#; nextPutAll: self comment ; nextPutAll: self lineDelimeter;	
		nextPutAll: self width displayString; nextPutAll: ' '; nextPutAll: self height displayString; nextPutAll: self lineDelimeter;	
		nextPutAll: self colors displayString; nextPutAll: self lineDelimeter;
		beBinary.
	
	self pixelData do: [:each | aStream underlyingStream nextPut: each red ;  nextPut:  each  green ; nextPut: each blue].	
	aStream underlyingStream close!

width
	^width!

width: anObject
	width := anObject! !
!PpmFile categoriesFor: #colorFromZxAttr:!public! !
!PpmFile categoriesFor: #colors!public! !
!PpmFile categoriesFor: #colors:!accessing!private! !
!PpmFile categoriesFor: #comment!accessing!private! !
!PpmFile categoriesFor: #comment:!accessing!private! !
!PpmFile categoriesFor: #fillPixelArray:!accessing!public! !
!PpmFile categoriesFor: #format!accessing!public! !
!PpmFile categoriesFor: #format:!accessing!private! !
!PpmFile categoriesFor: #height!accessing!public! !
!PpmFile categoriesFor: #height:!accessing!private! !
!PpmFile categoriesFor: #initialize!public! !
!PpmFile categoriesFor: #lineDelimeter!accessing!private! !
!PpmFile categoriesFor: #lineDelimeter:!accessing!private! !
!PpmFile categoriesFor: #loadFromStream:!public! !
!PpmFile categoriesFor: #pixelData!accessing!public! !
!PpmFile categoriesFor: #pixelData:!accessing!private! !
!PpmFile categoriesFor: #pixelDataFromZxBlock:!accessing!public! !
!PpmFile categoriesFor: #saveToFile:!public! !
!PpmFile categoriesFor: #saveToStream:!public! !
!PpmFile categoriesFor: #width!accessing!public! !
!PpmFile categoriesFor: #width:!accessing!private! !

!PpmFile class methodsFor!

fromFile: aFileName
	| ppm |
	ppm := SpFilename named: aFileName.
	^self fromStream: ppm readStream!

fromStream: aStream
	^self new
		loadFromStream: aStream;
		yourself!

fromZxScrBlock: aByteArray
	^self new
		format: 'P6';
		width: 256;
		height: 192;
		colors: 255;
		comment: ' Image from Zx Spectrum screen';
		pixelDataFromZxBlock: aByteArray;
		yourself!

fromZxScrFile: aFileName
	| stream aByteArray |
	stream := (SpFilename named: aFileName) readStream.
	stream underlyingStream beBinary.
	aByteArray := stream upToEnd.
	^self fromZxScrBlock: aByteArray!

new
	^super new initialize! !
!PpmFile class categoriesFor: #fromFile:!public! !
!PpmFile class categoriesFor: #fromStream:!public! !
!PpmFile class categoriesFor: #fromZxScrBlock:!public! !
!PpmFile class categoriesFor: #fromZxScrFile:!public! !
!PpmFile class categoriesFor: #new!public! !

"Binary Globals"!

