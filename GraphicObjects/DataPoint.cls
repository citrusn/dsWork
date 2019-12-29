"Filed out from Dolphin Smalltalk 7"!

Object subclass: #DataPoint
	instanceVariableNames: 'value timestamp status'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

	target nextPutAll: '[ ' , timestamp displayString , ' ;' , value displayString , ' ]'
	"FormatDateTime('hh:MM:ss', X) + ' ; ' + FormatFloat('0', Y)+ ' ]'"!

printOn: aStream
	"Append a textual representation of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		display: self;
		nextPut: $)!

status
	^status!

status: anObject
	status := anObject!

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
!DataPoint categoriesFor: #printOn:!public! !
!DataPoint categoriesFor: #status!accessing!public! !
!DataPoint categoriesFor: #status:!accessing!public! !
!DataPoint categoriesFor: #timestamp!accessing!public! !
!DataPoint categoriesFor: #timestamp:!accessing!public! !
!DataPoint categoriesFor: #value!accessing!public! !
!DataPoint categoriesFor: #value:!accessing!public! !

!DataPoint class methodsFor!

icon
	^ManagerShell icon!

new: aTimeStamp value: aValue
	"Answer a new, initialised, instance of the receiver.

	Illustrated Patterns:
	Instance Initialization
	"

	^super new
		initialize;
		timestamp: aTimeStamp;
		value: aValue! !
!DataPoint class categoriesFor: #icon!public! !
!DataPoint class categoriesFor: #new:value:!public! !

