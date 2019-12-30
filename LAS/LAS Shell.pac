| package |
package := Package name: 'LAS Shell'.
package paxVersion: 1;
	basicComment: 'This package defines some classes that provide base behavior for Shell and DocumentShell applications.  See also package LAS Runtime.
'.

package basicPackageVersion: '1.0.4'.


package classNames
	add: #LasDocumentShell;
	add: #LasShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Shell subclass: #LasShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DocumentShell subclass: #LasDocumentShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LasShell guid: (GUID fromString: '{f8834f16-e066-42ab-a71f-438f7198ef5c}')!
LasShell comment: 'This package defines some base behavior for Shell applications.'!
!LasShell categoriesForClass!MVP-Presenters! !
!LasShell methodsFor!

helpAbout
	"Display the receiver's help information."

	MessageBox new
		caption: 'About ' , self class applicationDisplayName;
		text: self class aboutText;
		open.
	self view lastFocus setFocus!

helpUserGuide
	"Open a browser on the receiver's online User Guide."

	ShellLibrary default shellOpen: self class urlUserGuide!

onViewClosed
	"Sent by the receiver's view when it has been closed"

	super onViewClosed.
	SessionManager current isRuntime ifTrue: [SessionManager current exit]! !
!LasShell categoriesFor: #helpAbout!commands!public! !
!LasShell categoriesFor: #helpUserGuide!commands!public! !
!LasShell categoriesFor: #onViewClosed!event handling!public! !

!LasShell class methodsFor!

aboutText
	"Answer the receiver's 'help about' text."

	^self aboutText: self!

aboutText: aClass 
	"Answer aClass' 'help about' text."

	^aClass applicationDisplayName 
		, '

Copyright: 2002-2004 Louis Sumberg <lsumberg@mindspring.com>

Version: %1 

DISCLAIMER: This software is freely provided purely as a sample and as such it is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE' 
			formatWith: aClass version!

applicationDisplayName
	"Answer the receiver's friendly or commercial name."

	self subclassResponsibility!

icon
	"Answer the receiver's icon."

	^self icon: self!

icon: aClass
	"Answer aClass' icon."

	^SessionManager current isRuntime
		ifTrue: [Icon fromId: aClass defaultIconName]
		ifFalse: 
			[Icon fromFile: aClass defaultIconName
				usingLocator: (FolderRelativeFileLocator
						basePath: (File splitPathFrom: aClass owningPackage packageFileName))]!

packageVersionString
	"Answer the receiver's package version as a string formatted like a VersionResource."

	^self packageVersionString: self!

packageVersionString: aClass
	"Answer the aClass' package version as a string formatted like a VersionResource."

	| pkgVersion words |
	pkgVersion := (PackageManager current packageOfClass: aClass) packageVersion.
	words := pkgVersion subStrings: $..
	words size = 2 ifTrue: [^'%1.0.0.%2' formatWith: words first with: words second].
	words size = 3
		ifTrue: 
			[^'%1.%2.0.%3'
				formatWith: words first
				with: words second
				with: words third]!

urlUserGuide
	"Answer a String URL where the receiver's User Guide is."

	self subclassResponsibility!

version
	"Answer the receiver's package version."

	^self version: self!

version: aClass
	"Answer aClass' package version."

	^SessionManager current isRuntime
		ifTrue: [SessionManager current versionInfo productVersionString]
		ifFalse: [aClass packageVersionString]! !
!LasShell class categoriesFor: #aboutText!enquiries!public! !
!LasShell class categoriesFor: #aboutText:!accessing!public! !
!LasShell class categoriesFor: #applicationDisplayName!constants!public! !
!LasShell class categoriesFor: #icon!constants!public! !
!LasShell class categoriesFor: #icon:!accessing!public! !
!LasShell class categoriesFor: #packageVersionString!accessing!private! !
!LasShell class categoriesFor: #packageVersionString:!accessing!private! !
!LasShell class categoriesFor: #urlUserGuide!constants!public! !
!LasShell class categoriesFor: #version!enquiries!public! !
!LasShell class categoriesFor: #version:!accessing!public! !

LasDocumentShell guid: (GUID fromString: '{f1a26400-0476-4051-a3a1-2d48ea994b2c}')!
LasDocumentShell comment: 'This package defines some base behavior for DocumentShell applications.'!
!LasDocumentShell categoriesForClass!MVP-Presenters! !
!LasDocumentShell methodsFor!

helpAbout
	"Display the receiver's help information."

	(MessageBox new)
		caption: 'About ' , self class applicationDisplayName;
		text: self class aboutText;
		open.
	self view lastFocus setFocus!

helpUserGuide
	"Open a browser on the receiver's online User Guide."

	ShellLibrary default shellOpen: self class urlUserGuide!

onViewClosed
	"Sent by the receiver's view when it has been closed"

	super onViewClosed.
	SessionManager current isRuntime ifTrue: [SessionManager current exit]! !
!LasDocumentShell categoriesFor: #helpAbout!commands!public! !
!LasDocumentShell categoriesFor: #helpUserGuide!commands!public! !
!LasDocumentShell categoriesFor: #onViewClosed!event handling!public! !

!LasDocumentShell class methodsFor!

aboutText
	"Answer the receiver's 'help about' text."

	^LasShell aboutText: self!

applicationDisplayName
	"Answer the receiver's friendly or commercial name."

	self subclassResponsibility!

icon
	"Answer the receiver's icon."

	^LasShell icon: self!

packageVersionString
	"Answer the receiver's package version as a string formatted like a VersionResource."

	^LasShell packageVersionString: self!

urlUserGuide
	"Answer a String URL where the receiver's User Guide is."

	self subclassResponsibility!

version
	"Answer the receiver's package version."

	^LasShell version: self! !
!LasDocumentShell class categoriesFor: #aboutText!enquiries!public! !
!LasDocumentShell class categoriesFor: #applicationDisplayName!constants!public! !
!LasDocumentShell class categoriesFor: #icon!constants!public! !
!LasDocumentShell class categoriesFor: #packageVersionString!accessing!private! !
!LasDocumentShell class categoriesFor: #urlUserGuide!constants!public! !
!LasDocumentShell class categoriesFor: #version!enquiries!public! !

"Binary Globals"!

