| package |
package := Package name: 'LAS Ini'.
package paxVersion: 1;
	basicComment: 'Copyright © Louis Sumberg, 2004.
Public Domain Freeware
lsumberg@mindspring.com

This package provides an easy way to save and restore the state of an application''s views and options using “ini” text files. For example, you can easily save and restore an application’s size and position, listview column sizes, splitter proportions, fonts, and other settings.

Online documentation is at www.mindspring.com/~lsumberg/dolphin

MyApp>>iniWrite: anIni
	super iniWrite: anIni.
	anIni
		writeKey: ''Filename'' value: self filename;
		writeKey: ''IsAutoPrompt'' value: self isAutoPrompt;
		writeKey: ''Threshold'' value: self threshold

MyApp>>iniRead: anIni
	super iniRead: anIni.
	self filename: (anIni readString: ''Filename'').
	self isAutoPrompt: (anIni readBoolean: ''IsAutoPrompt'' default: true).
	self threshold: (anIni readNumber: ''Threshold'' default: 1000)'.

package basicPackageVersion: '1.0.22'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIEYPEAAEAAAATGFzSW1hZ2VTdHJpcHBlcgAAAAAAAAAAUgAAAAcAAABMQVMgSW5pUgAA
AAAAAACaAAAAAAAAAFIAAAAQAAAARG9scGhpbiBNVlAgQmFzZVIAAAAVAAAAUnVudGltZVNlc3Np
b25NYW5hZ2Vy778lAAAAAAAGAw8AVmVyc2lvblJlc291cmNlAAAAAAYBEABWU19GSVhFREZJTEVJ
TkZPAAAAAHIAAAA0AAAAvQTv/gAAAQAAAAEAAgAAAAAAAQACAAAAPwAAAAAAAAAEAAAAAgAAAAAA
AAAAAAAAAAAAAOoAAAAAAAAA8AAAAGIAAAACAAAAUgAAAAgAAAAwNDA5MDRiMOoAAAAAAAAA8AAA
AGIAAAAYAAAAUgAAAA4AAABQcm9kdWN0VmVyc2lvblIAAAAKAAAAMSwgMCwgMCwgMlIAAAALAAAA
Q29tcGFueU5hbWVSAAAAAAAAAFIAAAAMAAAAUHJpdmF0ZUJ1aWxkUgAAAAAAAABSAAAADAAAAFNw
ZWNpYWxCdWlsZFIAAAAAAAAAUgAAAA8AAABGaWxlRGVzY3JpcHRpb25SAAAAGwAAAERvbHBoaW4g
WFAgVG9HbyBBcHBsaWNhdGlvblIAAAAPAAAATGVnYWxUcmFkZW1hcmtzUgAAADEAAABEb2xwaGlu
IGlzIGEgdHJhZGVtYXJrIG9mIENHSSBHcm91cCAoRXVyb3BlKSBMdGQuUgAAAAwAAABJbnRlcm5h
bE5hbWVSAAAAAAAAAFIAAAAQAAAAT3JpZ2luYWxGaWxlbmFtZVIAAAAAAAAAUgAAAA4AAABMZWdh
bENvcHlyaWdodFIAAAArAAAAUG9ydGlvbnMgQ29weXJpZ2h0IKkgT2JqZWN0IEFydHMgMTk5Ny0y
MDAzLlIAAAAIAAAAQ29tbWVudHNSAAAAHAAAAFBvd2VyZWQgYnkgRG9scGhpbiBTbWFsbHRhbGtS
AAAACwAAAEZpbGVWZXJzaW9uUgAAAAoAAAAxLCAwLCAwLCAyUgAAAAsAAABQcm9kdWN0TmFtZVIA
AAAdAAAAQSBEb2xwaGluIFhQIFRvR28gQXBwbGljYXRpb27KAAAAAAAAANAAAABiAAAAAQAAAAYC
CgBEV09SREFycmF5AAAAAHIAAAAEAAAACQSwBAMAAAAAAAAAwAEAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAA==').

package classNames
	add: #Ini;
	yourself.

package methodNames
	add: #CardContainer -> #iniRead:;
	add: #CardContainer -> #iniWrite:;
	add: #DesktopView -> #iniWrite:;
	add: #Font -> #iniWrite:key:;
	add: #KernelLibrary -> #getPrivateProfileString:key:default:buffer:bufferSize:filename:;
	add: #KernelLibrary -> #writePrivateProfileString:key:value:filename:;
	add: #ListView -> #iniRead:;
	add: #ListView -> #iniWrite:;
	add: #Object -> #iniWrite:key:;
	add: #Shell -> #iniRead:;
	add: #Shell -> #iniWrite:;
	add: #ShellView -> #iniRead:;
	add: #ShellView -> #iniWrite:;
	add: #TreeView -> #iniRead:;
	add: #TreeView -> #iniWrite:;
	add: #View -> #iniRead:;
	add: #View -> #iniWrite:;
	add: #WINDOWPLACEMENT -> #iniWrite:key:;
	add: 'Font class' -> #iniRead:key:default:;
	add: 'Point class' -> #fromString:;
	add: 'Point class' -> #readFrom:;
	add: 'WINDOWPLACEMENT class' -> #iniRead:key:default:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'LAS Runtime';
	yourself).

package!

"Class Definitions"!

Object subclass: #Ini
	instanceVariableNames: 'filename section'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!CardContainer methodsFor!

iniRead: anIni
	"Restore the receiver's state from anIni file."

	super iniRead: anIni.
	tabs selectionByIndex: (anIni readNumber: self name, ' selected tab' default: 1) ifAbsent: []!

iniWrite: anIni
	"Save the receiver's state to anIni file."

	super iniWrite: anIni.
	anIni writeKey: self name, ' selected tab' value: tabs selectionByIndex! !
!CardContainer categoriesFor: #iniRead:!filing!public! !
!CardContainer categoriesFor: #iniWrite:!filing!public! !

!DesktopView methodsFor!

iniWrite: anIni
	"Save the receiver's size and resolution to file."

	| extent res |
	extent := self rectangle extent.
	res := self resolution.
	anIni writeKey: 'Screen'
		value: (Array with: extent x with: extent y with: res x with: res y)! !
!DesktopView categoriesFor: #iniWrite:!filing!public! !

!Font methodsFor!

iniWrite: anIni key: key
	"Write a textual representation of the receiver to anIni file at key."

	anIni writeKey: key
		value: (Array with: self name with: self pointSize with: self isBold with: self isItalic)! !
!Font categoriesFor: #iniWrite:key:!filing!public! !

!Font class methodsFor!

iniRead: anIni key: key default: default
	"Answer an instance of the receiver, formed from either the value at key in anIni file, or default."

	| arr |
	^(arr := anIni readArray: key) isEmpty
		ifTrue: [default]
		ifFalse: [(self name: (arr at: 1) pointSize: (arr at: 2)) isBold: (arr at: 3); isItalic: (arr at: 4); yourself]! !
!Font class categoriesFor: #iniRead:key:default:!instance creation!public! !

!KernelLibrary methodsFor!

getPrivateProfileString: sectionString key: keyString default: defaultOrNil buffer: returnString bufferSize: nSize filename: filenameString
	"Retrieves the value for the given key in the given section in the given file and places it in the returnString buffer (or places the given default there).  The buffer size is nSize.  Answers the size of the string copied to the buffer.

		DWORD GetPrivateProfileString(
			LPCTSTR lpAppName,		// section name
			LPCTSTR lpKeyName,		// key name
			LPCTSTR lpDefault,		// default string
			LPTSTR lpReturnedString,	// destination buffer
			DWORD nSize,			// size of destination buffer
			LPCTSTR lpFileName		// initialization file name
		);"

	<stdcall: dword GetPrivateProfileStringA lpstr lpstr lpstr lpstr dword lpstr>
	^self invalidCall
!

writePrivateProfileString: sectionOrNil key: keyOrNil value: valueOrNil filename: filenameString
	"Write the specified key and value to the specified section in the specified file.  Answer true if successful, else false.

		BOOL WritePrivateProfileString(
			LPCTSTR lpAppName,	// section name
			LPCTSTR lpKeyName,	// key name
			LPCTSTR lpString,		// string to add
			LPCTSTR lpFileName		// initialization file
		);"

	<stdcall: bool WritePrivateProfileStringA lpstr lpstr lpstr lpstr>
	^self invalidCall! !
!KernelLibrary categoriesFor: #getPrivateProfileString:key:default:buffer:bufferSize:filename:!public!win32 functions-file! !
!KernelLibrary categoriesFor: #writePrivateProfileString:key:value:filename:!public!win32 functions-file! !

!ListView methodsFor!

iniRead: anIni
	"Restore the receiver's column order and column widths from anIni."

	| columnOrder columnWidths |
	super iniRead: anIni.
	columnOrder := anIni readArray: self name, ' column order'.
	(columnOrder size = self columnOrder size) ifTrue: [
		columnWidths := anIni readArray: self name, ' column widths'.
		columnOrder do: [:index | (self columnAtIndex: index) width: (columnWidths at: index)].
		self columnOrder: columnOrder]!

iniWrite: anIni
	"Save the receiver's state to anIni file."

	super iniWrite: anIni.
	anIni writeKey: self name, ' column order' value: self columnOrder.
	anIni writeKey: self name, ' column widths' value: (self allColumns collect: [:col | col width]) asArray! !
!ListView categoriesFor: #iniRead:!filing!public! !
!ListView categoriesFor: #iniWrite:!filing!public! !

!Object methodsFor!

iniWrite: anIni key: key
	"Save the receiver to file."

	anIni writeKey: key value: self! !
!Object categoriesFor: #iniWrite:key:!filing!public! !

!Point class methodsFor!

fromString: aString 
	"Answer an instance of the receiver constructed from aString."

	^self readFrom: aString readStream!

readFrom: aStream 
	"Answer an instance of the receiver constructed from aStream."

	^(aStream upTo: $@) trimBlanks asNumber @ aStream nextWord trimBlanks asNumber! !
!Point class categoriesFor: #fromString:!instance creation!public! !
!Point class categoriesFor: #readFrom:!instance creation!public! !

!Shell methodsFor!

iniRead: anIni
	"Restore the receiver's view state from a new Ini instance.  Default is to not use anIni."

	self view iniRead: (Ini on: self)!

iniWrite: anIni
	"Store the receiver's view state to a new ini instance.  Default is to not use anIni."

	self view iniWrite: (Ini on: self)! !
!Shell categoriesFor: #iniRead:!filing!public! !
!Shell categoriesFor: #iniWrite:!filing!public! !

!ShellView methodsFor!

iniRead: anIni
	"Restore the receiver's state.  Restore the state of all subviews."

	self placement: (WINDOWPLACEMENT 
				iniRead: anIni
				key: self class name , ' placement'
				default: self placement).
	(anIni readBoolean: self class name , ' isMaximized' default: false) 
		ifTrue: [SessionManager inputState queueDeferredAction: [self showMaximized]].
	self font: (Font 
				iniRead: anIni
				key: self class name , ' font'
				default: self font).
	self allSubViews do: [:subView | subView name notNil ifTrue: [subView iniRead: anIni]]!

iniWrite: anIni
	"Save the screen size and the receiver's state to anIni.  Inform all named subviews to save their state to anIni."

	DesktopView current iniWrite: anIni.
	self placement iniWrite: anIni key: self class name , ' placement'.
	anIni writeKey: self class name , ' isMaximized' value: (anIni isMaximized: self).
	self font iniWrite: anIni key: self class name , ' font'.
	self allSubViews do: [:subView | subView name notNil ifTrue: [subView iniWrite: anIni]]! !
!ShellView categoriesFor: #iniRead:!filing!public! !
!ShellView categoriesFor: #iniWrite:!filing!public! !

!TreeView methodsFor!

iniRead: anIni
	"Restore the receiver's state from anIni."

	super iniRead: anIni.
	self selectionByIndex: (anIni readNumber: self name, ' selection' default: self selectionByIndex) ifAbsent: [nil]!

iniWrite: anIni
	"Save the receiver's state to anIni file."

	super iniWrite: anIni.
	anIni writeKey: self name, ' selection' value: self selectionByIndex! !
!TreeView categoriesFor: #iniRead:!filing!public! !
!TreeView categoriesFor: #iniWrite:!filing!public! !

!View methodsFor!

iniRead: anIni
	"Restore the receiver's state from anIni file."

	self arrangement understandsArithmetic ifTrue: [
		self arrangement: (anIni readNumber: self name, ' arrangement' default: self arrangement)]!

iniWrite: anIni
	"Save the receiver's state to anIni file."

	self arrangement understandsArithmetic ifTrue: [
		 anIni writeKey: self name,' arrangement' value: self arrangement]! !
!View categoriesFor: #iniRead:!filing!public! !
!View categoriesFor: #iniWrite:!filing!public! !

!WINDOWPLACEMENT methodsFor!

iniWrite: anIni key: key
	"Save the receiver's state to anIni file."

	| rect |
	rect := self rcNormalPosition asRectangle.
	anIni writeKey: key
		value: (Array with: self showCmd with: rect left with: rect top with: rect right with: rect bottom)! !
!WINDOWPLACEMENT categoriesFor: #iniWrite:key:!filing!public! !

!WINDOWPLACEMENT class methodsFor!

iniRead: anIni key: key default: default
	"Answer an instance of the receiver, formed from either the value at key in anIni file, or default."

	| arr |
	^(arr := anIni readArray: key) isEmpty
		ifTrue: [default]
		ifFalse: [self new
			showCmd: 0 "(arr at: 1)";
			rcNormalPosition: (RECT left: (arr at: 2) top: (arr at: 3) right: (arr at: 4) bottom: (arr at: 5))]! !
!WINDOWPLACEMENT class categoriesFor: #iniRead:key:default:!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Ini guid: (GUID fromString: '{348df241-0863-11d5-b609-444553540000}')!
Ini comment: 'An Ini instance represents a section of a text file that contains a series of associations in the standard ".ini" format. '!
!Ini categoriesForClass!Unclassified! !
!Ini methodsFor!

contents
	"Answer the contents of the file."

	^(self filename notNil and: [File exists: self filename])
		ifTrue: [(FileStream read: self filename) contents]
		ifFalse: [String new]!

filename
	"Answer the receiver's filename."

	^filename!

filename: aString
	"Set the receiver's filename to aString."

	filename := aString!

initialize
	"Private - initialize the receiver."

	super initialize.
	filename := File change: SessionManager current imageFileName extension: 'ini'.
	section := 'Default'!

isMaximized: aShellView
	"Answer whether aShellView is currently maximized."

	^UserLibrary default isZoomed: aShellView asParameter!

keyExists: key
	"Answer true if key exists in the receiver's section, else false."

	^self keys includes: key!

keys
	"Answer a collection containing the keys in the receiver's section."

	^self readSection collect: [:assoc | assoc key]!

parseArrayItem: aString 
	aString first = $' ifTrue: [^aString midString: aString size - 2 from: 2].
	aString = 'false' ifTrue: [^false].
	aString = 'true' ifTrue: [^true].
	^aString asNumber!

parseArrayString: aString 
	"Private - Answer an Array whose elements are of classes String, Boolean, and/or Number."

	| strm col arr ch |
	strm := (aString midString: aString size - 3 from: 3) readStream.
	col := OrderedCollection new.
	[strm atEnd] whileFalse: 
			[ch := strm peek.
			ch = $' 
				ifTrue: 
					[strm next.
					col add: '''' , (strm upTo: $') , ''''.
					strm upTo: $ ]
				ifFalse: [col add: strm nextWord]].
	arr := Array new: col size.
	col keysAndValuesDo: [:i :e | arr at: i put: (self parseArrayItem: e)].
	^arr!

readArray: key 
	"Answer an array -- either the value at the specified key or an empty array if the key doesn't exist.
	Values in array must be of classes String, Boolean, and/or Number."

	^self readArray: key default: Array new!

readArray: key default: default 
	"Answer an array -- either the value at the specified key or default if the key doesn't exist.
	Values in array must be of classes String, Boolean, and/or Number."

	^self parseArrayString: (self readString: key default: default)!

readBoolean: key
	"Answer a boolean - either the value at the specified key or false if the key doesn't exist."

	^self readBoolean: key default: false!

readBoolean: key default: default
	"Answer a boolean -- either the value at the specified key or default if the key doesn't exist."

	^(self readString: key default: default) = 'true'!

readNumber: key
	"Answer a number - either the value at the specified key or 0 if the key doesn't exist."

	^self readNumber: key default: 0!

readNumber: key default: default
	"Answer a number -- either the value at the specified key or default if the key doesn't exist."

	^(self readString:key default: default) asNumber!

readSection
	"Answer a collection of associations that represents the receiver's current section"

	| col strm str |
	col := OrderedCollection new.
	strm := (self readString: nil) readStream.
	[strm atEnd or: [(str := strm upTo: Character null) isEmpty]] whileFalse: [
		col add: (Association key: str value: (self readString: str))].
	^col!

readString: key
	"Answer a string - either the value at the specified key or an empty string if the key doesn't exist."

	^self readString: key default: String new!

readString: key default: default
	"Answer a string - either the value at the specified key or default if the key doesn't exist."

	^self class readKey: key default: default section: self section filename: self filename!

removeKey: key
	"Remove the specified key from the current section in the receiver's file."

	self writeKey: key value: nil!

removeSection
	"Remove the current section from the receiver's file."

	self writeKey: nil value: nil!

section
	"Answer the receiver's section name."

	^section!

section: aString
	"Set the receiver's section name to aString."

	section := aString!

sectionNamesInFile
	"Answer a collection containing all of the section names in the receiver's file."

	^self class sectionNamesInFile: self filename!

writeKey: key value: value
	"Write the specified key and value using the receiver's section and filename.  Return true if successful, else false."

	^self class writeKey: key value: value section: self section filename: self filename! !
!Ini categoriesFor: #contents!accessing!public! !
!Ini categoriesFor: #filename!accessing!public! !
!Ini categoriesFor: #filename:!accessing!public! !
!Ini categoriesFor: #initialize!initializing!private! !
!Ini categoriesFor: #isMaximized:!public!testing! !
!Ini categoriesFor: #keyExists:!public!testing! !
!Ini categoriesFor: #keys!helpers!public! !
!Ini categoriesFor: #parseArrayItem:!operations!private! !
!Ini categoriesFor: #parseArrayString:!operations!private! !
!Ini categoriesFor: #readArray:!operations!public! !
!Ini categoriesFor: #readArray:default:!operations!public! !
!Ini categoriesFor: #readBoolean:!operations!public! !
!Ini categoriesFor: #readBoolean:default:!operations!public! !
!Ini categoriesFor: #readNumber:!operations!public! !
!Ini categoriesFor: #readNumber:default:!operations!public! !
!Ini categoriesFor: #readSection!helpers!public! !
!Ini categoriesFor: #readString:!operations!public! !
!Ini categoriesFor: #readString:default:!operations!public! !
!Ini categoriesFor: #removeKey:!public!removing! !
!Ini categoriesFor: #removeSection!public!removing! !
!Ini categoriesFor: #section!accessing!public! !
!Ini categoriesFor: #section:!accessing!public! !
!Ini categoriesFor: #sectionNamesInFile!helpers!public! !
!Ini categoriesFor: #writeKey:value:!operations!public! !

!Ini class methodsFor!

new
	"Answer an instance of the receiver."

	^super new initialize!

on: aShell
	"Answer an instance of the receiver, using aShell's class name and resourceNameString property
	as the instance's section."

	| subSection |
	subSection := aShell propertyAt: #resourceNameString ifAbsent: [nil].
	subSection := (subSection notNil and: [subSection ~= aShell view class defaultView]) 
				ifTrue: ['.' , subSection]
				ifFalse: [''].
	^self new section: aShell class name , subSection!

readKey: keyOrNil default: defaultOrNil section: sectionOrNil filename: filename
	"Answer a string - either the value at the specified key, or if the key doesn't exist, the defaultOrNil."

	| buffer retSize |
	filename isNil ifTrue: [^String new].
	buffer := String new: 32000.
	retSize := KernelLibrary default
		getPrivateProfileString: (sectionOrNil isNil ifTrue: [nil] ifFalse: [sectionOrNil displayString])
		key: (keyOrNil isNil ifTrue: [nil] ifFalse: [keyOrNil displayString])
		default: (defaultOrNil isNil ifTrue: [nil] ifFalse: [defaultOrNil displayString])
		buffer: buffer bufferSize: buffer size filename: filename.
	^buffer leftString: retSize!

readWriteOn: aShell
	"Create an instance of the receiver, using aShell's class name as the instance's section.  Setup observers
	on aShell to save and restore its state when aShell's view is opened (or already open) and closed."

	| ini |
	ini := self new section: aShell class name.
	aShell view isOpen 
		ifTrue: [aShell iniRead: ini]
		ifFalse: 
			[aShell 
				when: #viewOpened
				send: #iniRead:
				to: aShell
				with: ini].
	aShell 
		when: #viewClosed
		send: #iniWrite:
		to: aShell
		with: ini!

sectionNamesInFile: aFilename
	"Answer a collection containing all of the section names in the specified file."

	| col strm str |
	col := OrderedCollection new.
	strm := (self readKey: nil default: nil section: nil filename: aFilename) readStream.
	[strm atEnd or: [(str := strm upTo: Character null) isEmpty]] whileFalse: [col add: str].
	^col!

writeKey: keyOrNil value: objectOrNil section: section filename: filename
	"Write the specified key and value to the specified section in the specified file.  Answer true if successful, else false."

	| ret |
	filename isNil ifTrue: [^false].
	ret := (KernelLibrary default
		writePrivateProfileString: section displayString
		key: (keyOrNil isNil ifTrue: [nil] ifFalse: [keyOrNil displayString])
		value: (objectOrNil isNil ifTrue: [nil] ifFalse: [objectOrNil displayString])
		filename: filename).
	KernelLibrary default writePrivateProfileString: nil key: nil value: nil filename: filename.
	^ret! !
!Ini class categoriesFor: #new!instance creation!public! !
!Ini class categoriesFor: #on:!instance creation!public! !
!Ini class categoriesFor: #readKey:default:section:filename:!operations!public! !
!Ini class categoriesFor: #readWriteOn:!instance creation!public! !
!Ini class categoriesFor: #sectionNamesInFile:!helpers!public! !
!Ini class categoriesFor: #writeKey:value:section:filename:!operations!public! !

"Binary Globals"!

