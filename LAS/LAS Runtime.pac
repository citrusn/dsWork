| package |
package := Package name: 'LAS Runtime'.
package paxVersion: 0;
	basicComment: 'This package defines some classes that provide base behavior for deployed (ToGo) applications.  See also package LAS Shell.'.

package basicPackageVersion: '1.0.3'.


package classNames
	add: #LasImageStripper;
	add: #LasRuntimeSessionManager;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	yourself).

package!

"Class Definitions"!

ImageStripper subclass: #LasImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #LasRuntimeSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LasImageStripper guid: (GUID fromString: '{4A2DFE6F-9A17-4040-9E4E-8ADFA076D623}')!
LasImageStripper comment: 'This class provides some base behavior for deployed (ToGo) applications.'!
!LasImageStripper categoriesForClass!MVP-Models! !
!LasImageStripper methodsFor!

versionResource
	"Answer the <VersionResource> describing the version resource to be built into	
	the deployed application/dll, and based on the application's package version."

	| verString |
	verString := self runtimeSessionManagerClass mainShellClass packageVersionString.
	^(super versionResource)
		productVersion: verString;
		fileVersion: verString;
		yourself! !
!LasImageStripper categoriesFor: #versionResource!accessing!private! !

LasRuntimeSessionManager guid: (GUID fromString: '{423EA7E9-F050-4F62-B4E2-5B21CB36A3D5}')!
LasRuntimeSessionManager comment: 'This class provides some base behavior for deployed (ToGo) applications.'!
!LasRuntimeSessionManager categoriesForClass!System-Support! !
!LasRuntimeSessionManager methodsFor!

defaultResLibPath
	"Answer the path of the development resource library. 'self argv first' may not 
	include the full path to the executable if started from the command line"

	^self imageFileName!

main
	"Start up the application"

	resourceLibrary := nil.
	self mainShellClass show! !
!LasRuntimeSessionManager categoriesFor: #defaultResLibPath!constants!public! !
!LasRuntimeSessionManager categoriesFor: #main!operations-startup!public! !

"Binary Globals"!

"Resources"!

