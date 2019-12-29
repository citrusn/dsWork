| package |
package := Package name: 'OpcClient'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #OpcClientLibrary;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

ExternalLibrary subclass: #OpcClientLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OpcClientLibrary guid: (GUID fromString: '{eee34939-420a-45d8-8b96-3792cd94e381}')!
OpcClientLibrary comment: 'o := OpcClientLibrary default.
o opcConnect: ''opcserversim.Instance.1''
'!
!OpcClientLibrary categoriesForClass!External-Libraries! !
!OpcClientLibrary methodsFor!

opcAddGroup: aGroup
	"EXTERN_C _declspec(dllexport)  void opc_add_group(char *group) "

	<cdecl: void opc_add_group char*>
	^self invalidCall!

opcAddItem: anInt
	"EXTERN_C _declspec(dllexport)  void opc_add_item(int *item_i) "

	<cdecl: void opc_add_item dword*>
	^self invalidCall!

opcConnect: aServer
"EXTERN_C _declspec(dllexport)  void opc_connect(char *server) "

	<cdecl: void  opc_connect char*>
	^self invalidCall!

opcCopyItem: anIndex item: aName
	"EXTERN_C _declspec(dllexport)  void copy_item(int *index_item,char *item) "

	<cdecl: void copy_item dword* char*>
	^self invalidCall!

opcDisconnect
	"EXTERN_C _declspec(dllexport)  void opc_disconnect() "

	<cdecl: void opc_disconnect>
	^self invalidCall!

opcItemBrowse: aItems
	"EXTERN_C _declspec(dllexport)  void opc_item_browse(char item_n[500][40]) "

	<cdecl: void opc_item_browse lpvoid>
	^self invalidCall!

opcRead: anPtrInt value: aFloat
	"EXTERN_C _declspec(dllexport)  void opc_read(int *item_i,float item_v[20]) "

	!

opcServerBrowse: aServer
	"EXTERN_C _declspec(dllexport)  void opc_server_browse(char server[40][40]) "

	<cdecl: void opc_server_browse lpvoid>
	^self invalidCall! !
!OpcClientLibrary categoriesFor: #opcAddGroup:!public! !
!OpcClientLibrary categoriesFor: #opcAddItem:!public! !
!OpcClientLibrary categoriesFor: #opcConnect:!public! !
!OpcClientLibrary categoriesFor: #opcCopyItem:item:!public! !
!OpcClientLibrary categoriesFor: #opcDisconnect!public! !
!OpcClientLibrary categoriesFor: #opcItemBrowse:!public! !
!OpcClientLibrary categoriesFor: #opcRead:value:!public! !
!OpcClientLibrary categoriesFor: #opcServerBrowse:!public! !

!OpcClientLibrary class methodsFor!

fileName
	"Answer the host system file name of the external library which
	the receiver represents."

	^'opc_client_x86'! !
!OpcClientLibrary class categoriesFor: #fileName!public! !

"Binary Globals"!

