| package |
package := Package name: 'Themes'.
package paxVersion: 1;
	basicComment: 'Theme darkulaStyle'.


package classNames
	add: #SmalltalkSystemShellCopada;
	add: #Theme;
	yourself.

package methodNames
	add: #ClassBrowserShell -> #onViewOpened;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #Theme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkSystemShell subclass: #SmalltalkSystemShellCopada
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ClassBrowserShell methodsFor!

onViewOpened
	"Received when the receiver's view has been connected."

	super onViewOpened.
	self view allSubViewsDo: 
			[:each |
			each backcolor: (Color fromHTMLSpec: '#4B4B4B').
			each forecolor: Color white].
	(self view viewNamed: 'toolbar') forecolor: (Color fromHTMLSpec: '#4B4B4B').
	(self view viewNamed: 'toolbar') allSubViewsDo: 
			[:each |
			each backcolor: (Color fromHTMLSpec: '#4B4B4B').
			each forecolor: (Color fromHTMLSpec: '#4B4B4B')].
	(self view viewNamed: 'mode') forecolor: (Color fromHTMLSpec: '#4B4B4B').
	(self view viewNamed: 'mode') allSubViewsDo: [:each | each forecolor: Color white].
	(self view viewNamed: 'search') forecolor: Color white! !
!ClassBrowserShell categoriesFor: #onViewOpened!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Theme guid: (GUID fromString: '{219bbdad-ce85-41c7-bf0b-61ce55f9a2ce}')!
Theme comment: ''!
!Theme categoriesForClass!Kernel-Objects! !
!Theme class methodsFor!

darkulaStyle
	"This style is inspired on darkula theme from Eclipse: http://www.eclipsecolorthemes.org/?view=theme&id=15515"

	SmalltalkSystemShell backcolor: (Color fromHTMLSpec: '#2B2B2B').
	SmalltalkSystemShell textcolor: Color white.
	SmalltalkSystem current toolBackgroundColor: Color darkGray.
	SmalltalkWorkspace defaultBackcolor: (Color fromHTMLSpec: '#212121').
	SmalltalkWorkspace defaultCaretColor: (Color fromHTMLSpec: '#CCFFFF').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #normal]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #comment])
		forecolor: (Color fromHTMLSpec: '#7D8C93').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #illegal])
		forecolor: (Color fromHTMLSpec: '#FF8A80').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalNumber])
		forecolor: (Color fromHTMLSpec: '#FFE54B').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalString])
		forecolor: (Color fromHTMLSpec: '#E38C1E').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalSymbol])
		forecolor: (Color fromHTMLSpec: '#E38C1E').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalArray]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #unaryMessage]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #unarySelector]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #keywordMessage]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #keywordSelector])
		forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalBytes])
		forecolor: (Color fromHTMLSpec: '#F89C28').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalCharacter])
		forecolor: (Color fromHTMLSpec: '#F89C28').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #literalPseudo])
		forecolor: (Color fromHTMLSpec: '#F89C28').
	(SmalltalkWorkspace textStyles detect: [:each | each name = #binaryMessage]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #binarySelector]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #argDecl]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #assignment]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #blockArgDecl]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #braceHighlight]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #braceMismatch]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #identifier]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #indentGuide]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #specialCharacter])
		forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #tag]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #tempCloseBar]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #tempDecl]) forecolor: Color white.
	(SmalltalkWorkspace textStyles detect: [:each | each name = #tempOpenBar]) forecolor: Color white! !
!Theme class categoriesFor: #darkulaStyle!public! !

SmalltalkSystemShellCopada guid: (GUID fromString: '{649228bc-2e72-4cb7-a8ec-3c5fbb2c7b27}')!
SmalltalkSystemShellCopada comment: ''!
!SmalltalkSystemShellCopada categoriesForClass!Development! !
!SmalltalkSystemShellCopada class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 196934 1 ##(Smalltalk.RGB) 8 4294967295 0 39 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.Toolbar) 34 25 0 416 34 2 8 1140851524 131073 544 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 519 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 544 610 8 4278190080 8 4294902613 170 192 34 6 410 ##(Smalltalk.Toolbar) 34 25 0 544 34 2 8 1140853572 65 848 0 610 640 738 251 51 519 0 658 0 true 690 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 738 193 193 0 848 610 784 8 4294902613 170 192 8 #() 170 192 34 8 21213 1115910 ##(Smalltalk.ToolbarIconButton) 21213 0 848 1 1180998 4 ##(Smalltalk.CommandDescription) #up 8 'Up' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Up.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 395334 3 ##(Smalltalk.Bitmap) 0 true 0 0 0 0 3 738 33 33 1 21215 1106 21215 0 848 1 1138 #historyBack 8 'Back' 1 1 1186 0 true 1232 8 'HistoryBack.ico' 1280 1314 0 true 0 0 0 0 3 738 33 33 1 21217 1106 21217 0 848 1 1138 #historyForward 8 'Forward' 1 1 1186 0 true 1232 8 'HistoryForward.ico' 1280 1314 0 true 0 0 0 0 3 738 33 33 1 21219 853766 ##(Smalltalk.ToolbarButton) 21219 0 848 17 1138 #viewModeSelect 8 'Views' 1 1 0 1314 0 true 1232 8 'ViewBar.bmp' 1280 0 7 738 129 33 3 34 6 1120 1360 1472 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 848 3 0 1 1600 1714 0 0 848 3 0 1 170 176 34 8 1328 1 1440 3 1552 5 1648 7 0 1 0 738 33 33 738 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 738 1053 1 738 251 51 848 1922 #updateSizePosted 1056 848 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 0 0 0 0 139 2 0 0 25 0 0 0] 8 #() 738 193 193 0 27 8 'systemShellTools' 410 ##(Smalltalk.ReferenceView) 34 14 0 544 34 2 8 1140850688 131073 2112 0 721158 ##(Smalltalk.SystemColor) 31 0 7 0 0 0 2112 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.Toolbar) #resource_Image_tools 0 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 1 1 738 63 51 2112 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 0 0 0 25 0 0 0] 1056 2080 0 27 8 'imageTools' 410 ##(Smalltalk.ReferenceView) 34 14 0 544 34 2 8 1140850688 131073 2400 0 2178 31 0 7 0 0 0 2400 2210 ##(Smalltalk.Toolbar) #resource_Smalltalk_tools 0 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 63 1 738 991 51 2400 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 0 0 0 0 0 0 0 14 2 0 0 25 0 0 0] 1056 2080 0 27 8 'smalltalkTools' 170 192 1056 138 144 1056 170 176 1056 0 1 0 738 33 33 738 45 45 0 1826 1 1 1 1858 138 144 34 2 1922 #createAt:extent: 34 2 738 1 1 738 1709 51 544 1922 #updateSizePosted 1056 544 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 3 0 0 25 0 0 0] 34 3 2112 2400 848 2080 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 2928 0 610 8 4278190080 0 7 0 0 0 2928 1180166 ##(Smalltalk.ProportionalLayout) 170 176 1056 false 170 192 1056 0 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 1 51 738 1709 723 2928 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 86 3 0 0 130 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 2928 34 2 8 1140850688 131073 3248 0 0 0 7 0 0 0 3248 0 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 0 3248 34 2 8 1140932940 1 3344 590662 2 ##(Smalltalk.ListModel) 138 144 1056 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 1138 #properties 8 '&Properties' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3570 1 1138 #whatsThis 8 '&What''s This?' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3344 0 8 4294902587 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin List Presenter' 8 'BasicListAbstract' 8 'doIt' 8 '[:item | item description]' 8 #[30 105 17 158 106] #description 3760 7 257 0 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 738 65 65 0 0 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 1119 #left 3746 0 0 3778 2 1 3808 8 'doIt' 8 '[:item | item description]' 8 #[30 105 17 158 106] #description 4032 7 257 0 ##(Smalltalk.SortedCollection) 459270 ##(Smalltalk.Message) #yourself 8 #() 0 3344 3984 3 0 0 #largeIcons 1056 738 241 241 131169 0 34 4 0 0 738 1 1 0 1858 138 144 34 3 1922 #createAt:extent: 34 2 738 1 1 738 1709 723 3344 1922 #contextMenu: 34 1 3536 3344 1922 #text: 34 1 8 'Column 1' 3344 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 3 0 0 105 1 0 0] 8 #() 2080 0 27 8 'systemFolder' 0 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 1 1 738 849 723 3248 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 168 1 0 0 105 1 0 0] 34 1 3344 2080 0 27 410 ##(Smalltalk.Splitter) 34 12 0 2928 34 2 8 1140850688 1 4624 0 610 8 4278190080 0 519 0 0 0 4624 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 849 1 738 11 723 4624 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 168 1 0 0 0 0 0 0 173 1 0 0 105 1 0 0] 8 #() 2080 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 2928 34 2 8 1140850688 131073 4880 0 0 0 7 0 0 0 4880 0 170 192 1056 0 1858 138 144 34 1 1922 #createAt:extent: 34 2 738 859 1 738 851 723 4880 2018 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 1 0 0 0 0 0 0 86 3 0 0 105 1 0 0] 8 #() 2080 0 27 2080 0 27 170 192 34 2 544 8 'toolbar' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 5 3522 0 true 34 10 3570 1 1138 #newWorkspace 8 '&New Workspace' 9373 1 1186 0 true 1232 8 'SmalltalkWorkspace.ico' 1280 0 0 3570 1 1138 #openWorkspace 8 '&Open Workspace...' 9375 1 1186 0 true 1232 8 'FileOpen.ico' 1280 0 0 3570 1 1138 #fileFileIn 8 '&File In...' 1 1 0 0 0 3634 4097 3570 1 1138 #saveImage 8 'Sa&ve Image' 1 1 1186 0 true 1232 8 'Snapshot.ico' 1280 0 0 3570 1 1138 #saveImageAs 8 'Save Image As...' 1 1 0 0 0 3570 1 1138 #compressChanges 8 '&Compress Changes...' 1 1 0 0 0 3570 1 1138 #compressSources 8 'Co&mpress Sources...' 1 1 0 0 0 3634 4097 3570 1 1138 #smalltalkExit 8 'E&xit Dolphin' 1 1 1186 0 true 1232 8 'PowerSwitch.ico' 1280 0 0 8 '&File' 0 134217729 0 0 21121 0 0 3522 0 true 34 12 3570 1 1138 #browseHierarchy 8 '&Classes' 1 1 1186 0 true 1232 8 'ClassBrowserShell.ico' 1280 0 0 3570 1 1138 #browsePackages 8 '&Packages' 1 1 1186 0 true 1232 8 'Package.ico' 1280 0 0 3570 1 1138 #browseProtocols 8 'Pr&otocols' 1 1 1186 0 true 1232 8 'ProtocolBrowserShell.ico' 1280 0 0 3570 1 1138 #browseResources 8 '&Resources' 1 1 1186 0 true 1232 8 'Resource.ico' 1280 0 0 3570 1 1138 #browseSystem 8 'S&ystem' 1 1 1186 0 true 1232 8 'SystemBrowserShell.ico' 1280 0 0 3634 4097 3570 1 1138 #browseMessageDefinitions 8 'All &Definitions Of...' 1 1 0 0 0 3570 1 1138 #browseMessageReferences 8 'All Re&ferences To...' 1 1 0 0 0 3570 1 1138 4114 #browseReferencesToGlobal: 34 1 8 '' 8 'References to Global...' 1 1 0 0 0 3570 1 1138 #browseContainingText 8 'Containing &Text...' 1 1 1186 0 true 1232 47 786694 ##(Smalltalk.ShellLibrary) 0 0 3570 1 1138 #browseAllUnimplementedSelectors 8 'All &Unimplemented' 1 1 0 0 0 3522 0 true 34 5 3570 1 1138 #browseCompilationFailures 8 '&Compilation Failures' 1 1 0 0 0 3570 1 1138 #browseDeprecatedMethods 8 '&Deprecated' 1 1 0 0 0 3570 1 1138 #browseNotYetImplemented 8 '&Not Yet Implemented' 1 1 0 0 0 3570 1 1138 #browseUnclassifiedMethods 8 '&Unclassified' 1 1 0 0 0 3570 1 1138 #browseMethodCategory 8 '&Other...' 1 1 0 0 0 8 'Method Cate&gory' 0 134217729 0 0 21153 0 0 8 '&Browse' 0 134217729 0 0 21155 0 0 3522 0 true 8 #() 8 '&Tools' #toolsMenu 134217729 0 0 21157 0 0 3522 0 true 8 #() 8 'Wi&ndow' #windowMenu 134217729 0 0 21159 0 0 3522 0 true 34 15 3570 1 1138 #helpContents 8 '&Contents' 1025 1 1186 0 true 1232 49 6528 0 0 3570 1 1138 #help 8 'On this &Tool' 1249 1 0 0 0 3570 1 1138 #helpWhatsThis 8 'What''s This?' 5345 1 0 0 0 3634 4097 3570 1 1138 #learnSmalltalk 8 'Learn Smalltalk' 1 1 0 0 0 3634 4097 3570 1 1138 #helpWhatsNew 8 'What''s &New' 1 1 0 0 0 3570 1 1138 #helpGuidedTour 8 '&Guided Tour' 1 1 0 0 0 3570 1 1138 #helpTutorials 8 'Tutorials' 1 1 0 0 0 3634 4097 3570 1 1138 #objectArtsHomePage 8 'Object Arts Homepage' 1 1 0 0 0 3570 1 1138 #dolphinNewsgroup 8 'Dolphin Newsgroup/Forum' 1 1 0 0 0 3570 1 1138 #dolphinWikiWeb 8 'Dolphin WikiWeb' 1 1 0 0 0 3634 4097 3570 1 1138 #aboutDolphin 8 '&About Dolphin Smalltalk' 1 1 1186 0 true 1232 8 '!!APPLICATION' 1280 0 0 8 '&Help' 0 134217729 0 0 21183 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 738 2049 2049 0 1 0 0 1858 138 144 34 3 1922 #createAt:extent: 34 2 738 3839 21 738 1741 891 416 1922 #text: 34 1 8 'Dolphin Smalltalk' 416 1922 #updateMenuBar 1056 416 2018 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 229 10 0 0 199 1 0 0] 34 2 544 2928 2080 0 27 )! !
!SmalltalkSystemShellCopada class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

