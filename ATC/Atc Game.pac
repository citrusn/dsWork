| package |
package := Package name: 'Atc Game'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.001'.


package classNames
	add: #AtcModel;
	add: #AtcPresenter;
	add: #AtcView;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Model subclass: #AtcModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #AtcPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleBufferedView subclass: #AtcView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AtcModel guid: (GUID fromString: '{8e76ab0a-c4c1-481c-93f2-fc53f7c503a3}')!
AtcModel comment: ''!
!AtcModel categoriesForClass!MVP-Models! !
AtcPresenter guid: (GUID fromString: '{30a91361-b3e9-41ed-9125-8d2f4e7a3611}')!
AtcPresenter comment: ''!
!AtcPresenter categoriesForClass!MVP-Presenters! !
AtcView guid: (GUID fromString: '{9c0172e5-ccb2-4d2d-bfbd-de3ad337b7f5}')!
AtcView comment: ''!
!AtcView categoriesForClass!Unclassified! !
"Binary Globals"!

