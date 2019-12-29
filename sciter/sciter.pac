| package |
package := Package name: 'sciter'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #FunctionPointerEg;
	add: #Sciter;
	add: #SCITER_CALLBACK_NOTIFICATION;
	add: #SCITERAPI;
	add: #SciterGraphicsAPI;
	add: #SciterLibrary;
	add: #SciterWrapLibrary;
	add: #SCN_ATTACH_BEHAVIOR;
	add: #SCN_DATA_LOADED;
	add: #SCN_ENGINE_DESTROYED;
	add: #SCN_GRAPHICS_CRITICAL_FAILURE;
	add: #SCN_LOAD_DATA;
	add: #SCN_POSTED_NOTIFICATION;
	add: #Value;
	add: #ValueStructure;
	yourself.

package globalNames
	add: #SciterConstants;
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

Object subclass: #FunctionPointerEg
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sciter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Value
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #SciterLibrary
	instanceVariableNames: 'api'
	classVariableNames: ''
	poolDictionaries: 'SciterConstants'
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #SciterWrapLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'SciterConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #SCITER_CALLBACK_NOTIFICATION
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #SCITERAPI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #SciterGraphicsAPI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ValueStructure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'ValueConstants'!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_ATTACH_BEHAVIOR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_DATA_LOADED
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_ENGINE_DESTROYED
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_GRAPHICS_CRITICAL_FAILURE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_LOAD_DATA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SCITER_CALLBACK_NOTIFICATION subclass: #SCN_POSTED_NOTIFICATION
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #SciterConstants put: (PoolConstantsDictionary named: #SciterConstants)!
SciterConstants at: 'ALLOW_EVAL' put: 16r4!
SciterConstants at: 'ALLOW_FILE_IO' put: 16r1!
SciterConstants at: 'ALLOW_SOCKET_IO' put: 16r2!
SciterConstants at: 'ALLOW_SYSINFO' put: 16r8!
SciterConstants at: 'DT_HAS_DATE' put: 16r1!
SciterConstants at: 'DT_HAS_SECONDS' put: 16r4!
SciterConstants at: 'DT_HAS_TIME' put: 16r2!
SciterConstants at: 'DT_UTC' put: 16r10!
SciterConstants at: 'GFX_LAYER_AUTO' put: 16rFFFF!
SciterConstants at: 'GFX_LAYER_D2D' put: 16r3!
SciterConstants at: 'GFX_LAYER_GDI' put: 16r1!
SciterConstants at: 'GFX_LAYER_WARP' put: 16r2!
SciterConstants at: 'HV_BAD_PARAMETER' put: 16r1!
SciterConstants at: 'HV_INCOMPATIBLE_TYPE' put: 16r2!
SciterConstants at: 'HV_OK' put: 16r0!
SciterConstants at: 'HV_OK_TRUE' put: -16r1!
SciterConstants at: 'LOAD_DELAYED' put: 16r2!
SciterConstants at: 'LOAD_DISCARD' put: 16r1!
SciterConstants at: 'LOAD_MYSELF' put: 16r3!
SciterConstants at: 'LOAD_OK' put: 16r0!
SciterConstants at: 'OS_ERROR' put: 16r3!
SciterConstants at: 'OS_INFO' put: 16r1!
SciterConstants at: 'OS_WARNING' put: 16r2!
SciterConstants at: 'OT_CSS' put: 16r2!
SciterConstants at: 'OT_CSSS' put: 16r1!
SciterConstants at: 'OT_DOM' put: 16r0!
SciterConstants at: 'OT_TIS' put: 16r3!
SciterConstants at: 'SC_ATTACH_BEHAVIOR' put: 16r4!
SciterConstants at: 'SC_DATA_LOADED' put: 16r2!
SciterConstants at: 'SC_ENGINE_DESTROYED' put: 16r5!
SciterConstants at: 'SC_GRAPHICS_CRITICAL_FAILURE' put: 16r7!
SciterConstants at: 'SC_LOAD_DATA' put: 16r1!
SciterConstants at: 'SC_POSTED_NOTIFICATION' put: 16r6!
SciterConstants at: 'SCITER_ALPHA_WINDOW' put: 16rC!
SciterConstants at: 'SCITER_CONNECTION_TIMEOUT' put: 16r2!
SciterConstants at: 'SCITER_FONT_SMOOTHING' put: 16r4!
SciterConstants at: 'SCITER_HTTPS_ERROR' put: 16r3!
SciterConstants at: 'SCITER_SET_DEBUG_MODE' put: 16rA!
SciterConstants at: 'SCITER_SET_GFX_LAYER' put: 16r9!
SciterConstants at: 'SCITER_SET_GPU_BLACKLIST' put: 16r7!
SciterConstants at: 'SCITER_SET_SCRIPT_RUNTIME_FEATURES' put: 16r8!
SciterConstants at: 'SCITER_SET_UX_THEMING' put: 16rB!
SciterConstants at: 'SCITER_SMOOTH_SCROLL' put: 16r1!
SciterConstants at: 'SCITER_TRANSPARENT_WINDOW' put: 16r6!
SciterConstants at: 'SW_ALPHA' put: 16r40!
SciterConstants at: 'SW_CHILD' put: 16r1!
SciterConstants at: 'SW_CONTROLS' put: 16r10!
SciterConstants at: 'SW_ENABLE_DEBUG' put: 16r200!
SciterConstants at: 'SW_GLASSY' put: 16r20!
SciterConstants at: 'SW_MAIN' put: 16r80!
SciterConstants at: 'SW_OWNS_VM' put: 16r400!
SciterConstants at: 'SW_POPUP' put: 16r100!
SciterConstants at: 'SW_RESIZEABLE' put: 16r4!
SciterConstants at: 'SW_TITLEBAR' put: 16r2!
SciterConstants at: 'SW_TOOL' put: 16r8!
SciterConstants at: 'T_ANGLE' put: 16r12!
SciterConstants at: 'T_ARRAY' put: 16r9!
SciterConstants at: 'T_BOOL' put: 16r2!
SciterConstants at: 'T_BYTES' put: 16rC!
SciterConstants at: 'T_COLOR' put: 16r13!
SciterConstants at: 'T_CURRENCY' put: 16r7!
SciterConstants at: 'T_DATE' put: 16r6!
SciterConstants at: 'T_DOM_OBJECT' put: 16rE!
SciterConstants at: 'T_DURATION' put: 16r11!
SciterConstants at: 'T_FLOAT' put: 16r4!
SciterConstants at: 'T_FUNCTION' put: 16rB!
SciterConstants at: 'T_INT' put: 16r3!
SciterConstants at: 'T_LENGTH' put: 16r8!
SciterConstants at: 'T_MAP' put: 16rA!
SciterConstants at: 'T_NULL' put: 16r1!
SciterConstants at: 'T_OBJECT' put: 16rD!
SciterConstants at: 'T_RANGE' put: 16r10!
SciterConstants at: 'T_RESOURCE' put: 16rF!
SciterConstants at: 'T_STRING' put: 16r5!
SciterConstants at: 'T_UNDEFINED' put: 16r0!
SciterConstants at: 'UT_CM' put: 16r9!
SciterConstants at: 'UT_DIP' put: 16rD!
SciterConstants at: 'UT_EM' put: 16r1!
SciterConstants at: 'UT_EX' put: 16r2!
SciterConstants at: 'UT_IN' put: 16r8!
SciterConstants at: 'UT_MM' put: 16rA!
SciterConstants at: 'UT_OBJECT_ARRAY' put: 16r0!
SciterConstants at: 'UT_OBJECT_CLASS' put: 16r2!
SciterConstants at: 'UT_OBJECT_ERROR' put: 16r5!
SciterConstants at: 'UT_OBJECT_FUNCTION' put: 16r4!
SciterConstants at: 'UT_OBJECT_NATIVE' put: 16r3!
SciterConstants at: 'UT_OBJECT_OBJECT' put: 16r1!
SciterConstants at: 'UT_PC' put: 16rC!
SciterConstants at: 'UT_PR' put: 16r3!
SciterConstants at: 'UT_PT' put: 16rB!
SciterConstants at: 'UT_PX' put: 16r7!
SciterConstants at: 'UT_SP' put: 16r4!
SciterConstants at: 'UT_STRING_ERROR' put: 16r1!
SciterConstants at: 'UT_STRING_SECURE' put: 16r2!
SciterConstants at: 'UT_STRING_STRING' put: 16r0!
SciterConstants at: 'UT_STRING_SYMBOL' put: 16rFFFF!
SciterConstants at: 'UT_URL' put: 16r10!
SciterConstants shrink!

"Classes"!

FunctionPointerEg guid: (GUID fromString: '{6c5aa195-02f5-4915-872f-a2837622c449}')!
FunctionPointerEg comment: 'Invoking a function at a known address - fuction prototype staticallaly known:
FunctionPointerEg new  examples1

Same again, but copying the metod in case we want call same function at multiple addresses:
FunctionPointerEg new examples2

Same again, but dynamically constructed external call method (build with the compiler):
FunctionPointerEg new examples3

FunctionPointerEg new
'!
!FunctionPointerEg categoriesForClass!Kernel-Objects! !
!FunctionPointerEg methodsFor!

beep: anInteger dwDuration: dwDuration
	<stdcall: bool Beep dword dword>
	^self invalidCall!

examples1
	| method |
	method := self class compiledMethodAt: #beep:dwDuration:.
	self halt.
	method descriptorLiteral dwordAtOffset: 0 put: (KernelLibrary default getProcAddress: 'Beep').
	300 to: 600
		by: 100
		do: [:i | self beep: i dwDuration: 100]!

examples2
	| method |
	method := (self class compiledMethodAt: #beep:dwDuration:) deepCopy.
	method descriptorLiteral dwordAtOffset: 0 put: (KernelLibrary default getProcAddress: 'Beep').
	600 to: 300
		by: -100
		do: [:i | self beep: i dwDuration: 100]!

examples3
	"the easiest way to build an ExternalMethod dynamically is to get the compiler to do it"

	| method methodSource |
	methodSource := 'i: i j: j <stdcall: bool _ dword dword>'.
	method := Compiler compile: methodSource in: Object.
	method descriptorLiteral dwordAtOffset: 0 put: (KernelLibrary default getProcAddress: 'Beep').
	(300 to: 600	by: 100), (600 to: 300 by: -100) do: [:i | method value: self withArguments:( Array with: i with: 100)]
!

initialize
	| call |
	call := self class compiledMethodAt: #beep:dwDuration:.
self halt.
	call descriptorLiteral dwordAtOffset: 0 put: (KernelLibrary default getProcAddress: 'Beep').
	call value: self withArguments: #(400 400)! !
!FunctionPointerEg categoriesFor: #beep:dwDuration:!public! !
!FunctionPointerEg categoriesFor: #examples1!public! !
!FunctionPointerEg categoriesFor: #examples2!public! !
!FunctionPointerEg categoriesFor: #examples3!public! !
!FunctionPointerEg categoriesFor: #initialize!public! !

!FunctionPointerEg class methodsFor!

new
	^super new initialize! !
!FunctionPointerEg class categoriesFor: #new!public! !

Sciter guid: (GUID fromString: '{1739f9e1-693a-4ac1-ad79-cf967cda1dca}')!
Sciter comment: ''!
!Sciter categoriesForClass!Kernel-Objects! !
Value guid: (GUID fromString: '{b0380bfe-3639-47c9-94b4-60b5ab9e6726}')!
Value comment: ''!
!Value categoriesForClass!Kernel-Objects! !
!Value class methodsFor!

new
! !
!Value class categoriesFor: #new!public! !

SciterLibrary guid: (GUID fromString: '{fbb7c1ef-4d33-473c-9cff-c48cccc31e81}')!
SciterLibrary comment: ''!
!SciterLibrary categoriesForClass!External-Libraries! !
!SciterLibrary methodsFor!

api
	api := self SciterAPI.
	^api!

api: anObject
	api := anObject!

debugCallBack
	" DEBUG_OUTPUT_PROC* = proc (param: pointer; subsystem: uint32; ## #OUTPUT_SUBSYTEMS
                            severity: uint32; text: WideCString;
                            text_length: uint32) {.stdcall.}"

	^ExternalCallback block: 
			[:param :subsystem :severity :text :text_length |
			Transcript
				show: 'subsystem:' , (self outputSubsystems: subsystem) ,
					' severity: ' , (self outputSeverity: severity) , (text ifNil: [''] ifNotNil: [:value | value])]
		argumentTypes: 'lpvoid dword dword lpwstr dword'!

delegateSciterCallBack
	"typedef LRESULT SC_CALLBACK SciterWindowDelegate(HWINDOW hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LPVOID pParam, BOOL* handled);"

	^ExternalCallback block: 
			[:hWnd :msg :wPAram :lParam :pParam :pHandled |			
			Transcript show: hWnd asInteger asHexString, ' msg: ', msg asHexString; cr.
	"self halt."
			msg == WM_CLOSE ifTrue: 
					[Transcript show: 'closing Sciter window ...'; cr.
					"UserLibrary default postQuitMessage: 0."
					UserLibrary default destroyWindow: hWnd.
					pHandled value: 1 ].
			0]
		descriptor: (ExternalDescriptor fromString: 'stdcall: intptr lpvoid  dword uintptr intptr lpvoid BOOL*')!

initialize
	super initialize.
	api := nil!

outputSeverity: anInt
	anInt = OS_INFO ifTrue: [^'OS_INFO'].
	anInt = OS_WARNING ifTrue: [^'OT_WARNING'].
	anInt = OS_ERROR ifTrue: [^'OT_ERROR'].
	^'Severity is unknown'!

outputSubsystems: anInt
	anInt = OT_DOM ifTrue: [^'OT_DOM'].
	anInt = OT_CSSS ifTrue: [^'OT_CSSS'].
	anInt = OT_CSS ifTrue: [^'OT_CSS'].
	anInt = OT_TIS ifTrue: [^'OT_TIS'].
	^ 'Subsystem is unknown'!

runSciterMethod: aSource address: anAddress arguments: anArgs
	| method val |	
	method := Compiler compile: aSource in: Object.
	method descriptorLiteral dwordAtOffset: 0 put: anAddress.
	val := method value: self withArguments: anArgs.
	^val!

SciterAPI
	"Private - SciterAPI_ptr sciterAPI = (SciterAPI_ptr) GetProcAddress(hm, SciterAPI);"

	<stdcall: SCITERAPI* SciterAPI>
	^self invalidCall!

SciterClassName
	| val |
	val := self
				runSciterMethod: 's <stdcall: byte* _  >'
				address: api SciterClassName
				arguments: #().
	^Utf16String fromAddress: val!

SciterCreateElement: aTagString text: aStringOrNil
	"proc SciterCreateElement*(tagname: cstring; textOrNull: WideCString;
                         phe: ptr HELEMENT): int32"

	| e |
	e := ExternalHandle new.
	self
		runSciterMethod: ' t:t s:s e:e<stdcall: sword _ lpstr lpwstr handle*>'
		address: api SciterCreateElement
		arguments: (Array
				with: aTagString
				with: aStringOrNil
				with: e).
	^e!

SciterCreateWindow: aCreationFlags frame: aFrame delegate: aDelegate delegateParam: aParams parent: aParendWnd
	"SciterCreateWindow*: proc (creationFlags: uint32; frame: ptr Rect;
                            delegate: SciterWindowDelegate;
                            delegateParam: pointer; parent: HWINDOW): HWINDOW {.stdcall.}"

	^self
		runSciterMethod: 'fl: fl fr:fr d:d pr:pr par:par <stdcall: handle _ dword RECT* lpvoid lpvoid handle>'
		address: api SciterCreateWindow
		arguments: (Array
				with: aCreationFlags
				with: aFrame asParameter
				with: aDelegate
				with: aParams
				with: aParendWnd)!

SciterGetRootElement: hwnd
	"SciterGetRootElement*: proc (hwnd: HWINDOW; phe: ptr HELEMENT): int32 {.stdcall.}"

	| root val |
	root := ExternalHandle new.
	val := self
				runSciterMethod: ' h:h r:r <stdcall: sdword _ handle handle*>'
				address: api SciterGetRootElement
				arguments: (Array with: hwnd with: root).
	^root!

sciterHostCallback
	"SciterHostCallback*  (pns: LPSCITER_CALLBACK_NOTIFICATION;
                           callbackParam: pointer): SC_LOAD_DATA_RETURN_CODES {.stdcall.}"

	"if params.code == SC_LOAD_DATA:
    return OnLoadData(cast[LPSCN_LOAD_DATA](params))"

	^ExternalCallback block: 
			[:param :callbackParam |
			Transcript
				show: 'code:' , param code displayString; cr.
				param code == 1 ifTrue:[
				Transcript show: (SCN_LOAD_DATA fromAddress: param asParameter )  displayString ; cr. ] ]
		argumentTypes: 'SCITER_CALLBACK_NOTIFICATION* lpvoid'!

SciterInsertElement: he parent: phe index: anInt
	"proc SciterInsertElement*(he: HELEMENT; hparent: HELEMENT; index: uint32): int32 "

	^self
		runSciterMethod: 'e:e p:p i:i <stdcall: sdword _ handle handle dword>'
		address: api SciterInsertElement
		arguments: (Array
				with: he
				with: phe
				with: anInt)!

SciterLoadFile: hWndSciter filename: aFileString
	"SciterLoadFile*: proc (hWndSciter: HWINDOW;
		filename: WideCString): bool {.stdcall.}"

	^self
		runSciterMethod: 'h:h f:f <stdcall: bool _ handle lpwstr>'
		address: api SciterLoadFile
		arguments: (Array with: hWndSciter with: aFileString asParameter)!

SciterLoadHtml: aWndSciter html: aWStr htmlSize: aSize baseUrl: aStrBase
	"SciterLoadHtml*: proc (hWndSciter: HWINDOW; html: pointer; htmlSize: uint32;
                         baseUrl: WideCString): bool {.stdcall.}"

	^self
		runSciterMethod: 'h:h p:p s:s b:b <stdcall: bool _ handle lpvoid dword lpwstr>'
		address: api SciterLoadHtml
		arguments: (Array
				with: aWndSciter
				with: aWStr
				with: aSize
				with: aStrBase)!

SciterSetCallback: hWndSciter  cb: SciterHostCallback cbParam:  aPointer
"SciterSetCallback*(hWndSciter: HWINDOW; cb: SciterHostCallback; cbParam: pointer)"

	| methodSource  val |
	methodSource  := 'h: h o: o v: v <stdcall: void _ handle lpvoid lpvoid>'.	
	val := self
			runSciterMethod: methodSource
			address: api SciterSetCallback
			arguments: (Array
					with: hWndSciter
					with: SciterHostCallback
					with: aPointer).
	^val!

SciterSetOption: hWnd option: anOption value: aVal
	"SciterSetOption*: proc (hWnd: HWINDOW; 
		option: uint32; value: uint32): bool {.stdcall.}"

	| methodSource  val |
	methodSource  := 'h: h o: o v: v <stdcall: bool _ handle dword dword>'.	
	val := self
			runSciterMethod: methodSource
			address: api SciterSetOption
			arguments: (Array
					with: hWnd
					with: anOption
					with: aVal).
	^val!

SciterSetupDebugOutput: hwndOrNull param: APointer pfOutput: DEBUG_OUTPUT_PROC
	"SciterSetupDebugOutput*: proc (hwndOrNull: HWINDOW; 
		param: pointer; pfOutput: DEBUG_OUTPUT_PROC) {.stdcall.}"

	| methodSource |
	methodSource := 'h: h pr: pr d:d <stdcall: void _ handle lpvoid lpvoid >'.
	self
		runSciterMethod: methodSource
		address: api SciterSetupDebugOutput
		arguments: (Array
				with: hwndOrNull
				with: APointer
				with: DEBUG_OUTPUT_PROC)!

SciterVersion: major
	| val |
	val := self
				runSciterMethod: 'm: m <stdcall: dword _ bool >'
				address: api SciterVersion
				arguments: (Array with: major).
	^val!

SciterVersionAsString
	| major minor |
	major := self SciterVersion: true.
	minor := self SciterVersion: false.
	^'<P>.<P>.<P>.<P>' expandMacrosWithArguments: {major bitShift: -16.
				major bitAnd: 65535.
				minor bitShift: -16.
				minor bitAnd: 65535}!

SetTitle: hWnd caption: aStr
	UserLibrary default setWindowText: hWnd lpString: aStr!

ShowWindow: hWnd
	UserLibrary default showWindow: hWnd nCmdShow: SW_SHOW!

ValueCopy: aSrcPtr
	| val  dst|
	dst := ValueStructure new.
	val := self
				runSciterMethod: 'm: m d: d <stdcall: sdword _ ValueStructure* ValueStructure*>'
				address: api ValueCopy
				arguments: (Array with: dst with: aSrcPtr).
	^dst!

ValueCopy: aDstPtr to: aSrcPtr 
	| val  r|
	r := ValueStructure new.
	val := self
				runSciterMethod: 'm: m d: d <stdcall: sdword _ ValueStructure* ValueStructure*>'
				address: api ValueCopy
				arguments: (Array with: aDstPtr with: aSrcPtr).
	^r!

ValueInit: aValuePtr
	| val |
	val := self
				runSciterMethod: 'm: m <stdcall: sdword _ ValueStructure*>'
				address: api ValueInit
				arguments: (Array with: aValuePtr).
	^val!

ValueIntData: aValuePtr
	| val r |
	r := SDWORD new.
	val := self
				runSciterMethod: 'm: m d: d <stdcall: sdword _ ValueStructure* sdword*>'
				address: api ValueIntData
				arguments: (Array with: aValuePtr with: r).
	val == 0 ifFalse: [RaisedSignal signal: 'ValueIntData result is not HV_OK'].
	^r asInteger!

ValueIntDataSet: aValuePtr data: anInt
	"ValueIntDataSet*: proc (pval: ptr VALUE; data: int32;`type`: uint32; units: uint32): VALUE_RESULT {.stdcall.}"

	| val |
	val := self
				runSciterMethod: 'm: m d: d  t: t u: u <stdcall: sdword _ ValueStructure* sdword dword dword>'
				address: api ValueIntDataSet
				arguments: (Array
						with: aValuePtr
						with: anInt
						with: 3
						with: 0).
	^val!

ValueStringData: aValuePtr
	"ValueStringData*: proc (pval: ptr VALUE; pChars: ptr WideCString;
                            pNumChars: ptr uint32): VALUE_RESULT {.stdcall.}"

	| val n b |
	b := ExternalAddress new.
	n := DWORD fromInteger: 0.	
	val := self
				runSciterMethod: 'm: m d: d  n:n <stdcall: sdword _ ValueStructure* lpwstr* dword*>'
				address: api ValueStringData
				arguments: (Array
						with: aValuePtr
						with: b 
						with: n asParameter).
	"val == 0 ifFalse: [RaisedSignal signal: 'ValueStringData: result is not HV_OK']."
self halt.
	Transcript show: 'ValueStringData: ' , val displayString, ' b: ' , b asString; cr.
	"^Utf16String fromAddress: b length: n asInteger" "(ExternalAddress fromInteger: b asInteger )"!

ValueStringDataSet: aValuePtr data: aStr
	"ValueStringDataSet*: proc (pval: ptr VALUE; chars: WideCString;
                              numChars: uint32; units: uint32): VALUE_RESULT {.stdcall.}"

	| val |
	val := self
				runSciterMethod: 'm: m d: d  t: t u: u <stdcall: sdword _ ValueStructure* lpwstr dword dword>'
				address: api ValueStringDataSet
				arguments: (Array
						with: aValuePtr
						with: aStr
						with: aStr size
						with: 0).
	^val!

ValueToString: aValuePtr
	"ValueToString*: proc (pval: ptr VALUE; 
		how: uint32): VALUE_RESULT {.stdcall.} ## #VALUE_STRING_CVT_TYPE"

	| val  |
	val := self
				runSciterMethod: 'm: m h:h <stdcall: sdword _ ValueStructure* sdword>'
				address: api ValueToString
				arguments: (Array with: aValuePtr with: 0).
	val == 0 ifFalse: [RaisedSignal signal: 'ValueToString result is not HV_OK'].
	^val! !
!SciterLibrary categoriesFor: #api!accessing!public! !
!SciterLibrary categoriesFor: #api:!accessing!public! !
!SciterLibrary categoriesFor: #debugCallBack!public! !
!SciterLibrary categoriesFor: #delegateSciterCallBack!public! !
!SciterLibrary categoriesFor: #initialize!private! !
!SciterLibrary categoriesFor: #outputSeverity:!private! !
!SciterLibrary categoriesFor: #outputSubsystems:!private! !
!SciterLibrary categoriesFor: #runSciterMethod:address:arguments:!private! !
!SciterLibrary categoriesFor: #SciterAPI!private! !
!SciterLibrary categoriesFor: #SciterClassName!public! !
!SciterLibrary categoriesFor: #SciterCreateElement:text:!public! !
!SciterLibrary categoriesFor: #SciterCreateWindow:frame:delegate:delegateParam:parent:!public! !
!SciterLibrary categoriesFor: #SciterGetRootElement:!public! !
!SciterLibrary categoriesFor: #sciterHostCallback!public! !
!SciterLibrary categoriesFor: #SciterInsertElement:parent:index:!public! !
!SciterLibrary categoriesFor: #SciterLoadFile:filename:!public! !
!SciterLibrary categoriesFor: #SciterLoadHtml:html:htmlSize:baseUrl:!public! !
!SciterLibrary categoriesFor: #SciterSetCallback:cb:cbParam:!public! !
!SciterLibrary categoriesFor: #SciterSetOption:option:value:!public! !
!SciterLibrary categoriesFor: #SciterSetupDebugOutput:param:pfOutput:!public! !
!SciterLibrary categoriesFor: #SciterVersion:!public! !
!SciterLibrary categoriesFor: #SciterVersionAsString!public! !
!SciterLibrary categoriesFor: #SetTitle:caption:!public! !
!SciterLibrary categoriesFor: #ShowWindow:!public! !
!SciterLibrary categoriesFor: #ValueCopy:!public! !
!SciterLibrary categoriesFor: #ValueCopy:to:!public! !
!SciterLibrary categoriesFor: #ValueInit:!public! !
!SciterLibrary categoriesFor: #ValueIntData:!public! !
!SciterLibrary categoriesFor: #ValueIntDataSet:data:!public! !
!SciterLibrary categoriesFor: #ValueStringData:!public! !
!SciterLibrary categoriesFor: #ValueStringDataSet:data:!public! !
!SciterLibrary categoriesFor: #ValueToString:!public! !

!SciterLibrary class methodsFor!

fileName
	"Answer the host system file name of the external library which the receiver represents"

	^'Sciter'! !
!SciterLibrary class categoriesFor: #fileName!public! !

SciterWrapLibrary guid: (GUID fromString: '{d9fa8568-b19c-4470-bc9c-5bda011349e7}')!
SciterWrapLibrary comment: ''!
!SciterWrapLibrary categoriesForClass!External-Libraries! !
!SciterWrapLibrary methodsFor!

debugCallBack
	" DEBUG_OUTPUT_PROC* = proc (param: pointer; subsystem: uint32; ## #OUTPUT_SUBSYTEMS
                            severity: uint32; text: WideCString;
                            text_length: uint32) {.stdcall.}"

	^ExternalCallback block: 
			[:param :subsystem :severity :text :text_length |
			Transcript
				show: 'subsystem:' , (self  outputSubsystems: subsystem ), ' severity: ' , (self outputSeverity: severity) , text]
		argumentTypes: 'lpvoid dword dword lpwstr dword'!

outputSeverity: anInt
	anInt = 0 ifTrue: [^'OS_INFO'].
	anInt = 1 ifTrue: [^'OT_WARNING'].
	anInt = 2 ifTrue: [^'OT_ERROR']!

outputSubsystems: anInt
	anInt = 0 ifTrue: [^'OT_DOM'].
	anInt = 1 ifTrue: [^'OT_CSSS'].
	anInt = 2 ifTrue: [^'OT_CSS'].
	anInt = 3 ifTrue: [^'OT_TIS']!

SciterAPI
	"SciterAPI_ptr sciterAPI = (SciterAPI_ptr) GetProcAddress(hm, SciterAPI);"

	<stdcall: SCITERAPI* SciterAPI>
	^self invalidCall!

SciterClassName
	" SciterClassName*: proc (): WideCString {.stdcall.}"

	<stdcall: lpwstr SciterClassNameWrap>
	^self invalidCall!

SciterCreateElement: aTagString text: aStringOrNil element: phe
	"proc SciterCreateElement*(tagname: cstring; textOrNull: WideCString;
                         phe: ptr HELEMENT): int32"

	<stdcall: sword SciterCreateElementWrap lpstr lpwstr handle*>
	^self invalidCall!

SciterCreateWindow: aCreationFlags frame: aFrame delegate: aDelegate delegateParam: aParams parent: aParendWnd
	"SciterCreateWindow*: proc (creationFlags: uint32; frame: ptr Rect;
                            delegate: SciterWindowDelegate;
                            delegateParam: pointer; parent: HWINDOW): HWINDOW {.stdcall.}"

	<stdcall: handle SciterCreateWindowWrap dword RECT* lpvoid lpvoid handle>
	^self invalidCall!

SciterGetRootElement: hwnd element: phe
	"SciterGetRootElement*: proc (hwnd: HWINDOW; phe: ptr HELEMENT): int32 {.stdcall.}"

	<stdcall: sdword SciterGetRootElementWrap handle handle*>
	^self invalidCall!

SciterInsertElement: he parent:  phe index: anInt
	"proc SciterInsertElement*(he: HELEMENT; hparent: HELEMENT; index: uint32): int32 "

	<stdcall: sdword SciterInsertElementWrap handle handle dword>
	^self invalidCall!

SciterLoadFile: hWndSciter filename: aFileString
	"SciterLoadFile*: proc (hWndSciter: HWINDOW; filename: WideCString): bool {.stdcall.}"

	<stdcall: bool SciterLoadFileWrap handle lpwstr>
	^self invalidCall!

SciterLoadHtml: aWndSciter html: aPointer htmlSize: aSize baseUrl: aStrBase
	"SciterLoadHtml*: proc (hWndSciter: HWINDOW; html: pointer; htmlSize: uint32;
                         baseUrl: WideCString): bool {.stdcall.}"

	<stdcall: bool SciterLoadHtmlWrap handle lpvoid dword lpwstr>
	^self invalidCall!

SciterSetOption: hWnd option: anOption value: aVal
	"SciterSetOption*: proc (hWnd: HWINDOW; option: uint32; value: uint32): bool {.stdcall.}"

	<stdcall: bool SciterSetOptionWrap handle dword dword>
	^self invalidCall!

SciterSetupDebugOutput: hwndOrNull param: APointer pfOutput: DEBUG_OUTPUT_PROC
	"SciterSetupDebugOutput*: proc (hwndOrNull: HWINDOW; param: pointer;
        pfOutput: DEBUG_OUTPUT_PROC) {.stdcall.}"

	<stdcall: void SciterSetupDebugOutputWrap handle lpvoid lpvoid >
	^self invalidCall!

SciterVersion: major
	"UINT SCAPI SciterVersionWrap (BOOL major)"

	<stdcall: dword SciterVersionWrap bool>
	^self invalidCall!

ShowWindow: hWnd
	^UserLibrary default showWindow: hWnd nCmdShow: SW_SHOW! !
!SciterWrapLibrary categoriesFor: #debugCallBack!public! !
!SciterWrapLibrary categoriesFor: #outputSeverity:!public! !
!SciterWrapLibrary categoriesFor: #outputSubsystems:!public! !
!SciterWrapLibrary categoriesFor: #SciterAPI!public! !
!SciterWrapLibrary categoriesFor: #SciterClassName!public! !
!SciterWrapLibrary categoriesFor: #SciterCreateElement:text:element:!public! !
!SciterWrapLibrary categoriesFor: #SciterCreateWindow:frame:delegate:delegateParam:parent:!public! !
!SciterWrapLibrary categoriesFor: #SciterGetRootElement:element:!public! !
!SciterWrapLibrary categoriesFor: #SciterInsertElement:parent:index:!public! !
!SciterWrapLibrary categoriesFor: #SciterLoadFile:filename:!public! !
!SciterWrapLibrary categoriesFor: #SciterLoadHtml:html:htmlSize:baseUrl:!public! !
!SciterWrapLibrary categoriesFor: #SciterSetOption:option:value:!public! !
!SciterWrapLibrary categoriesFor: #SciterSetupDebugOutput:param:pfOutput:!public! !
!SciterWrapLibrary categoriesFor: #SciterVersion:!public! !
!SciterWrapLibrary categoriesFor: #ShowWindow:!public! !

!SciterWrapLibrary class methodsFor!

fileName
	"Answer the host system file name of the external library which the receiver represents"

	^'SciterWraper'! !
!SciterWrapLibrary class categoriesFor: #fileName!public! !

SCITER_CALLBACK_NOTIFICATION guid: (GUID fromString: '{c423019c-2220-4e14-8975-ce3527e5af10}')!
SCITER_CALLBACK_NOTIFICATION comment: ' SCITER_CALLBACK_NOTIFICATION* = object
    code*: uint32  ## #*< [in] one of the codes above.
    hwnd*: HWINDOW ## #*< [in] HWINDOW of the window this callback was attached to.'!
!SCITER_CALLBACK_NOTIFICATION categoriesForClass!External-Data-Structured! !
!SCITER_CALLBACK_NOTIFICATION class methodsFor!

defineFields
	self
		defineField: #code type: DWORDField readOnly;
		defineField: #hwnd type: HANDLEField readOnly! !
!SCITER_CALLBACK_NOTIFICATION class categoriesFor: #defineFields!public! !

SCITERAPI guid: (GUID fromString: '{99e6d917-1b5c-48fb-b696-bcfbc94c33c8}')!
SCITERAPI comment: 'self compileDefinition
'!
!SCITERAPI categoriesForClass!External-Data-Structured! !
!SCITERAPI methodsFor!

GetSciterGraphicsAPI
	"Answer the <Integer> value of the receiver's 'GetSciterGraphicsAPI' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #GetSciterGraphicsAPI)!

GetSciterGraphicsAPI: anInteger
	"Set the receiver's 'GetSciterGraphicsAPI' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #GetSciterGraphicsAPI) put: anInteger!

GetSciterRequestAPI
	"Answer the <Integer> value of the receiver's 'GetSciterRequestAPI' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #GetSciterRequestAPI)!

GetSciterRequestAPI: anInteger
	"Set the receiver's 'GetSciterRequestAPI' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #GetSciterRequestAPI) put: anInteger!

Sciter_tv2V
	"Answer the <Integer> value of the receiver's 'Sciter_tv2V' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #Sciter_tv2V)!

Sciter_tv2V: anInteger
	"Set the receiver's 'Sciter_tv2V' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #Sciter_tv2V) put: anInteger!

Sciter_UnuseElement
	"Answer the <Integer> value of the receiver's 'Sciter_UnuseElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #Sciter_UnuseElement)!

Sciter_UnuseElement: anInteger
	"Set the receiver's 'Sciter_UnuseElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #Sciter_UnuseElement) put: anInteger!

Sciter_UseElement
	"Answer the <Integer> value of the receiver's 'Sciter_UseElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #Sciter_UseElement)!

Sciter_UseElement: anInteger
	"Set the receiver's 'Sciter_UseElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #Sciter_UseElement) put: anInteger!

Sciter_V2tv
	"Answer the <Integer> value of the receiver's 'Sciter_V2tv' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #Sciter_V2tv)!

Sciter_V2tv: anInteger
	"Set the receiver's 'Sciter_V2tv' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #Sciter_V2tv) put: anInteger!

SciterAppendMasterCSS
	"Answer the <Integer> value of the receiver's 'SciterAppendMasterCSS' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterAppendMasterCSS)!

SciterAppendMasterCSS: anInteger
	"Set the receiver's 'SciterAppendMasterCSS' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterAppendMasterCSS) put: anInteger!

SciterAttachEventHandler
	"Answer the <Integer> value of the receiver's 'SciterAttachEventHandler' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterAttachEventHandler)!

SciterAttachEventHandler: anInteger
	"Set the receiver's 'SciterAttachEventHandler' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterAttachEventHandler) put: anInteger!

SciterAttachHwndToElement
	"Answer the <Integer> value of the receiver's 'SciterAttachHwndToElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterAttachHwndToElement)!

SciterAttachHwndToElement: anInteger
	"Set the receiver's 'SciterAttachHwndToElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterAttachHwndToElement) put: anInteger!

SciterCall
	"Answer the <Integer> value of the receiver's 'SciterCall' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCall)!

SciterCall: anInteger
	"Set the receiver's 'SciterCall' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCall) put: anInteger!

SciterCallBehaviorMethod
	"Answer the <Integer> value of the receiver's 'SciterCallBehaviorMethod' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCallBehaviorMethod)!

SciterCallBehaviorMethod: anInteger
	"Set the receiver's 'SciterCallBehaviorMethod' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCallBehaviorMethod) put: anInteger!

SciterCallScriptingFunction
	"Answer the <Integer> value of the receiver's 'SciterCallScriptingFunction' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCallScriptingFunction)!

SciterCallScriptingFunction: anInteger
	"Set the receiver's 'SciterCallScriptingFunction' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCallScriptingFunction) put: anInteger!

SciterCallScriptingMethod
	"Answer the <Integer> value of the receiver's 'SciterCallScriptingMethod' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCallScriptingMethod)!

SciterCallScriptingMethod: anInteger
	"Set the receiver's 'SciterCallScriptingMethod' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCallScriptingMethod) put: anInteger!

SciterClassName
	"Answer the <Integer> value of the receiver's 'SciterClassName' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterClassName)!

SciterClassName: anInteger
	"Set the receiver's 'SciterClassName' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterClassName) put: anInteger!

SciterClearAttributes
	"Answer the <Integer> value of the receiver's 'SciterClearAttributes' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterClearAttributes)!

SciterClearAttributes: anInteger
	"Set the receiver's 'SciterClearAttributes' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterClearAttributes) put: anInteger!

SciterCloneElement
	"Answer the <Integer> value of the receiver's 'SciterCloneElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCloneElement)!

SciterCloneElement: anInteger
	"Set the receiver's 'SciterCloneElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCloneElement) put: anInteger!

SciterCloseArchive
	"Answer the <Integer> value of the receiver's 'SciterCloseArchive' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCloseArchive)!

SciterCloseArchive: anInteger
	"Set the receiver's 'SciterCloseArchive' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCloseArchive) put: anInteger!

SciterCombineURL
	"Answer the <Integer> value of the receiver's 'SciterCombineURL' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCombineURL)!

SciterCombineURL: anInteger
	"Set the receiver's 'SciterCombineURL' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCombineURL) put: anInteger!

SciterControlGetType
	"Answer the <Integer> value of the receiver's 'SciterControlGetType' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterControlGetType)!

SciterControlGetType: anInteger
	"Set the receiver's 'SciterControlGetType' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterControlGetType) put: anInteger!

SciterCreateCommentNode
	"Answer the <Integer> value of the receiver's 'SciterCreateCommentNode' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateCommentNode)!

SciterCreateCommentNode: anInteger
	"Set the receiver's 'SciterCreateCommentNode' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateCommentNode) put: anInteger!

SciterCreateElement
	"Answer the <Integer> value of the receiver's 'SciterCreateElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateElement)!

SciterCreateElement: anInteger
	"Set the receiver's 'SciterCreateElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateElement) put: anInteger!

SciterCreateOnDirectXWindow
	"Answer the <Integer> value of the receiver's 'SciterCreateOnDirectXWindow' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateOnDirectXWindow)!

SciterCreateOnDirectXWindow: anInteger
	"Set the receiver's 'SciterCreateOnDirectXWindow' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateOnDirectXWindow) put: anInteger!

SciterCreateTextNode
	"Answer the <Integer> value of the receiver's 'SciterCreateTextNode' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateTextNode)!

SciterCreateTextNode: anInteger
	"Set the receiver's 'SciterCreateTextNode' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateTextNode) put: anInteger!

SciterCreateWindow
	"Answer the <Integer> value of the receiver's 'SciterCreateWindow' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateWindow)!

SciterCreateWindow: anInteger
	"Set the receiver's 'SciterCreateWindow' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterCreateWindow) put: anInteger!

SciterD2DFactory
	"Answer the <Integer> value of the receiver's 'SciterD2DFactory' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterD2DFactory)!

SciterD2DFactory: anInteger
	"Set the receiver's 'SciterD2DFactory' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterD2DFactory) put: anInteger!

SciterDataReady
	"Answer the <Integer> value of the receiver's 'SciterDataReady' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDataReady)!

SciterDataReady: anInteger
	"Set the receiver's 'SciterDataReady' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDataReady) put: anInteger!

SciterDataReadyAsync
	"Answer the <Integer> value of the receiver's 'SciterDataReadyAsync' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDataReadyAsync)!

SciterDataReadyAsync: anInteger
	"Set the receiver's 'SciterDataReadyAsync' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDataReadyAsync) put: anInteger!

SciterDeleteElement
	"Answer the <Integer> value of the receiver's 'SciterDeleteElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDeleteElement)!

SciterDeleteElement: anInteger
	"Set the receiver's 'SciterDeleteElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDeleteElement) put: anInteger!

SciterDetachElement
	"Answer the <Integer> value of the receiver's 'SciterDetachElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDetachElement)!

SciterDetachElement: anInteger
	"Set the receiver's 'SciterDetachElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDetachElement) put: anInteger!

SciterDetachEventHandler
	"Answer the <Integer> value of the receiver's 'SciterDetachEventHandler' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDetachEventHandler)!

SciterDetachEventHandler: anInteger
	"Set the receiver's 'SciterDetachEventHandler' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDetachEventHandler) put: anInteger!

SciterDWFactory
	"Answer the <Integer> value of the receiver's 'SciterDWFactory' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterDWFactory)!

SciterDWFactory: anInteger
	"Set the receiver's 'SciterDWFactory' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterDWFactory) put: anInteger!

SciterEval
	"Answer the <Integer> value of the receiver's 'SciterEval' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterEval)!

SciterEval: anInteger
	"Set the receiver's 'SciterEval' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterEval) put: anInteger!

SciterEvalElementScript
	"Answer the <Integer> value of the receiver's 'SciterEvalElementScript' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterEvalElementScript)!

SciterEvalElementScript: anInteger
	"Set the receiver's 'SciterEvalElementScript' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterEvalElementScript) put: anInteger!

SciterFindElement
	"Answer the <Integer> value of the receiver's 'SciterFindElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterFindElement)!

SciterFindElement: anInteger
	"Set the receiver's 'SciterFindElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterFindElement) put: anInteger!

SciterFireEvent
	"Answer the <Integer> value of the receiver's 'SciterFireEvent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterFireEvent)!

SciterFireEvent: anInteger
	"Set the receiver's 'SciterFireEvent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterFireEvent) put: anInteger!

SciterGetArchiveItem
	"Answer the <Integer> value of the receiver's 'SciterGetArchiveItem' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetArchiveItem)!

SciterGetArchiveItem: anInteger
	"Set the receiver's 'SciterGetArchiveItem' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetArchiveItem) put: anInteger!

SciterGetAttributeByNameCB
	"Answer the <Integer> value of the receiver's 'SciterGetAttributeByNameCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetAttributeByNameCB)!

SciterGetAttributeByNameCB: anInteger
	"Set the receiver's 'SciterGetAttributeByNameCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetAttributeByNameCB) put: anInteger!

SciterGetAttributeCount
	"Answer the <Integer> value of the receiver's 'SciterGetAttributeCount' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetAttributeCount)!

SciterGetAttributeCount: anInteger
	"Set the receiver's 'SciterGetAttributeCount' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetAttributeCount) put: anInteger!

SciterGetCallbackParam
	"Answer the <Integer> value of the receiver's 'SciterGetCallbackParam' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetCallbackParam)!

SciterGetCallbackParam: anInteger
	"Set the receiver's 'SciterGetCallbackParam' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetCallbackParam) put: anInteger!

SciterGetChildrenCount
	"Answer the <Integer> value of the receiver's 'SciterGetChildrenCount' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetChildrenCount)!

SciterGetChildrenCount: anInteger
	"Set the receiver's 'SciterGetChildrenCount' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetChildrenCount) put: anInteger!

SciterGetElementByUID
	"Answer the <Integer> value of the receiver's 'SciterGetElementByUID' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementByUID)!

SciterGetElementByUID: anInteger
	"Set the receiver's 'SciterGetElementByUID' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementByUID) put: anInteger!

SciterGetElementHtmlCB
	"Answer the <Integer> value of the receiver's 'SciterGetElementHtmlCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementHtmlCB)!

SciterGetElementHtmlCB: anInteger
	"Set the receiver's 'SciterGetElementHtmlCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementHtmlCB) put: anInteger!

SciterGetElementHwnd
	"Answer the <Integer> value of the receiver's 'SciterGetElementHwnd' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementHwnd)!

SciterGetElementHwnd: anInteger
	"Set the receiver's 'SciterGetElementHwnd' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementHwnd) put: anInteger!

SciterGetElementIndex
	"Answer the <Integer> value of the receiver's 'SciterGetElementIndex' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIndex)!

SciterGetElementIndex: anInteger
	"Set the receiver's 'SciterGetElementIndex' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIndex) put: anInteger!

SciterGetElementIntrinsicHeight
	"Answer the <Integer> value of the receiver's 'SciterGetElementIntrinsicHeight' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIntrinsicHeight)!

SciterGetElementIntrinsicHeight: anInteger
	"Set the receiver's 'SciterGetElementIntrinsicHeight' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIntrinsicHeight) put: anInteger!

SciterGetElementIntrinsicWidths
	"Answer the <Integer> value of the receiver's 'SciterGetElementIntrinsicWidths' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIntrinsicWidths)!

SciterGetElementIntrinsicWidths: anInteger
	"Set the receiver's 'SciterGetElementIntrinsicWidths' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementIntrinsicWidths) put: anInteger!

SciterGetElementLocation
	"Answer the <Integer> value of the receiver's 'SciterGetElementLocation' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementLocation)!

SciterGetElementLocation: anInteger
	"Set the receiver's 'SciterGetElementLocation' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementLocation) put: anInteger!

SciterGetElementNamespace
	"Answer the <Integer> value of the receiver's 'SciterGetElementNamespace' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementNamespace)!

SciterGetElementNamespace: anInteger
	"Set the receiver's 'SciterGetElementNamespace' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementNamespace) put: anInteger!

SciterGetElementState
	"Answer the <Integer> value of the receiver's 'SciterGetElementState' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementState)!

SciterGetElementState: anInteger
	"Set the receiver's 'SciterGetElementState' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementState) put: anInteger!

SciterGetElementTextCB
	"Answer the <Integer> value of the receiver's 'SciterGetElementTextCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementTextCB)!

SciterGetElementTextCB: anInteger
	"Set the receiver's 'SciterGetElementTextCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementTextCB) put: anInteger!

SciterGetElementType
	"Answer the <Integer> value of the receiver's 'SciterGetElementType' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementType)!

SciterGetElementType: anInteger
	"Set the receiver's 'SciterGetElementType' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementType) put: anInteger!

SciterGetElementTypeCB
	"Answer the <Integer> value of the receiver's 'SciterGetElementTypeCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementTypeCB)!

SciterGetElementTypeCB: anInteger
	"Set the receiver's 'SciterGetElementTypeCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementTypeCB) put: anInteger!

SciterGetElementUID
	"Answer the <Integer> value of the receiver's 'SciterGetElementUID' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementUID)!

SciterGetElementUID: anInteger
	"Set the receiver's 'SciterGetElementUID' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetElementUID) put: anInteger!

SciterGetExpando
	"Answer the <Integer> value of the receiver's 'SciterGetExpando' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetExpando)!

SciterGetExpando: anInteger
	"Set the receiver's 'SciterGetExpando' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetExpando) put: anInteger!

SciterGetFocusElement
	"Answer the <Integer> value of the receiver's 'SciterGetFocusElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetFocusElement)!

SciterGetFocusElement: anInteger
	"Set the receiver's 'SciterGetFocusElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetFocusElement) put: anInteger!

SciterGetHighlightedElement
	"Answer the <Integer> value of the receiver's 'SciterGetHighlightedElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetHighlightedElement)!

SciterGetHighlightedElement: anInteger
	"Set the receiver's 'SciterGetHighlightedElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetHighlightedElement) put: anInteger!

SciterGetMinHeight
	"Answer the <Integer> value of the receiver's 'SciterGetMinHeight' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetMinHeight)!

SciterGetMinHeight: anInteger
	"Set the receiver's 'SciterGetMinHeight' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetMinHeight) put: anInteger!

SciterGetMinWidth
	"Answer the <Integer> value of the receiver's 'SciterGetMinWidth' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetMinWidth)!

SciterGetMinWidth: anInteger
	"Set the receiver's 'SciterGetMinWidth' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetMinWidth) put: anInteger!

SciterGetNthAttributeNameCB
	"Answer the <Integer> value of the receiver's 'SciterGetNthAttributeNameCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthAttributeNameCB)!

SciterGetNthAttributeNameCB: anInteger
	"Set the receiver's 'SciterGetNthAttributeNameCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthAttributeNameCB) put: anInteger!

SciterGetNthAttributeValueCB
	"Answer the <Integer> value of the receiver's 'SciterGetNthAttributeValueCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthAttributeValueCB)!

SciterGetNthAttributeValueCB: anInteger
	"Set the receiver's 'SciterGetNthAttributeValueCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthAttributeValueCB) put: anInteger!

SciterGetNthChild
	"Answer the <Integer> value of the receiver's 'SciterGetNthChild' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthChild)!

SciterGetNthChild: anInteger
	"Set the receiver's 'SciterGetNthChild' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetNthChild) put: anInteger!

SciterGetObject
	"Answer the <Integer> value of the receiver's 'SciterGetObject' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetObject)!

SciterGetObject: anInteger
	"Set the receiver's 'SciterGetObject' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetObject) put: anInteger!

SciterGetParentElement
	"Answer the <Integer> value of the receiver's 'SciterGetParentElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetParentElement)!

SciterGetParentElement: anInteger
	"Set the receiver's 'SciterGetParentElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetParentElement) put: anInteger!

SciterGetPPI
	"Answer the <Integer> value of the receiver's 'SciterGetPPI' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetPPI)!

SciterGetPPI: anInteger
	"Set the receiver's 'SciterGetPPI' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetPPI) put: anInteger!

SciterGetRootElement
	"Answer the <Integer> value of the receiver's 'SciterGetRootElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetRootElement)!

SciterGetRootElement: anInteger
	"Set the receiver's 'SciterGetRootElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetRootElement) put: anInteger!

SciterGetScrollInfo
	"Answer the <Integer> value of the receiver's 'SciterGetScrollInfo' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetScrollInfo)!

SciterGetScrollInfo: anInteger
	"Set the receiver's 'SciterGetScrollInfo' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetScrollInfo) put: anInteger!

SciterGetStyleAttributeCB
	"Answer the <Integer> value of the receiver's 'SciterGetStyleAttributeCB' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetStyleAttributeCB)!

SciterGetStyleAttributeCB: anInteger
	"Set the receiver's 'SciterGetStyleAttributeCB' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetStyleAttributeCB) put: anInteger!

SciterGetValue
	"Answer the <Integer> value of the receiver's 'SciterGetValue' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetValue)!

SciterGetValue: anInteger
	"Set the receiver's 'SciterGetValue' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetValue) put: anInteger!

SciterGetViewExpando
	"Answer the <Integer> value of the receiver's 'SciterGetViewExpando' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetViewExpando)!

SciterGetViewExpando: anInteger
	"Set the receiver's 'SciterGetViewExpando' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetViewExpando) put: anInteger!

SciterGetVM
	"Answer the <Integer> value of the receiver's 'SciterGetVM' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGetVM)!

SciterGetVM: anInteger
	"Set the receiver's 'SciterGetVM' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGetVM) put: anInteger!

SciterGraphicsCaps
	"Answer the <Integer> value of the receiver's 'SciterGraphicsCaps' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterGraphicsCaps)!

SciterGraphicsCaps: anInteger
	"Set the receiver's 'SciterGraphicsCaps' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterGraphicsCaps) put: anInteger!

SciterHidePopup
	"Answer the <Integer> value of the receiver's 'SciterHidePopup' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterHidePopup)!

SciterHidePopup: anInteger
	"Set the receiver's 'SciterHidePopup' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterHidePopup) put: anInteger!

SciterHttpRequest
	"Answer the <Integer> value of the receiver's 'SciterHttpRequest' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterHttpRequest)!

SciterHttpRequest: anInteger
	"Set the receiver's 'SciterHttpRequest' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterHttpRequest) put: anInteger!

SciterInsertElement
	"Answer the <Integer> value of the receiver's 'SciterInsertElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterInsertElement)!

SciterInsertElement: anInteger
	"Set the receiver's 'SciterInsertElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterInsertElement) put: anInteger!

SciterIsElementEnabled
	"Answer the <Integer> value of the receiver's 'SciterIsElementEnabled' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterIsElementEnabled)!

SciterIsElementEnabled: anInteger
	"Set the receiver's 'SciterIsElementEnabled' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterIsElementEnabled) put: anInteger!

SciterIsElementVisible
	"Answer the <Integer> value of the receiver's 'SciterIsElementVisible' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterIsElementVisible)!

SciterIsElementVisible: anInteger
	"Set the receiver's 'SciterIsElementVisible' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterIsElementVisible) put: anInteger!

SciterLoadFile
	"Answer the <Integer> value of the receiver's 'SciterLoadFile' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterLoadFile)!

SciterLoadFile: anInteger
	"Set the receiver's 'SciterLoadFile' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterLoadFile) put: anInteger!

SciterLoadHtml
	"Answer the <Integer> value of the receiver's 'SciterLoadHtml' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterLoadHtml)!

SciterLoadHtml: anInteger
	"Set the receiver's 'SciterLoadHtml' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterLoadHtml) put: anInteger!

SciterNodeAddRef
	"Answer the <Integer> value of the receiver's 'SciterNodeAddRef' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeAddRef)!

SciterNodeAddRef: anInteger
	"Set the receiver's 'SciterNodeAddRef' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeAddRef) put: anInteger!

SciterNodeCastFromElement
	"Answer the <Integer> value of the receiver's 'SciterNodeCastFromElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeCastFromElement)!

SciterNodeCastFromElement: anInteger
	"Set the receiver's 'SciterNodeCastFromElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeCastFromElement) put: anInteger!

SciterNodeCastToElement
	"Answer the <Integer> value of the receiver's 'SciterNodeCastToElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeCastToElement)!

SciterNodeCastToElement: anInteger
	"Set the receiver's 'SciterNodeCastToElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeCastToElement) put: anInteger!

SciterNodeChildrenCount
	"Answer the <Integer> value of the receiver's 'SciterNodeChildrenCount' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeChildrenCount)!

SciterNodeChildrenCount: anInteger
	"Set the receiver's 'SciterNodeChildrenCount' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeChildrenCount) put: anInteger!

SciterNodeFirstChild
	"Answer the <Integer> value of the receiver's 'SciterNodeFirstChild' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeFirstChild)!

SciterNodeFirstChild: anInteger
	"Set the receiver's 'SciterNodeFirstChild' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeFirstChild) put: anInteger!

SciterNodeGetText
	"Answer the <Integer> value of the receiver's 'SciterNodeGetText' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeGetText)!

SciterNodeGetText: anInteger
	"Set the receiver's 'SciterNodeGetText' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeGetText) put: anInteger!

SciterNodeInsert
	"Answer the <Integer> value of the receiver's 'SciterNodeInsert' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeInsert)!

SciterNodeInsert: anInteger
	"Set the receiver's 'SciterNodeInsert' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeInsert) put: anInteger!

SciterNodeLastChild
	"Answer the <Integer> value of the receiver's 'SciterNodeLastChild' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeLastChild)!

SciterNodeLastChild: anInteger
	"Set the receiver's 'SciterNodeLastChild' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeLastChild) put: anInteger!

SciterNodeNextSibling
	"Answer the <Integer> value of the receiver's 'SciterNodeNextSibling' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeNextSibling)!

SciterNodeNextSibling: anInteger
	"Set the receiver's 'SciterNodeNextSibling' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeNextSibling) put: anInteger!

SciterNodeNthChild
	"Answer the <Integer> value of the receiver's 'SciterNodeNthChild' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeNthChild)!

SciterNodeNthChild: anInteger
	"Set the receiver's 'SciterNodeNthChild' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeNthChild) put: anInteger!

SciterNodeParent
	"Answer the <Integer> value of the receiver's 'SciterNodeParent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeParent)!

SciterNodeParent: anInteger
	"Set the receiver's 'SciterNodeParent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeParent) put: anInteger!

SciterNodePrevSibling
	"Answer the <Integer> value of the receiver's 'SciterNodePrevSibling' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodePrevSibling)!

SciterNodePrevSibling: anInteger
	"Set the receiver's 'SciterNodePrevSibling' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodePrevSibling) put: anInteger!

SciterNodeRelease
	"Answer the <Integer> value of the receiver's 'SciterNodeRelease' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeRelease)!

SciterNodeRelease: anInteger
	"Set the receiver's 'SciterNodeRelease' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeRelease) put: anInteger!

SciterNodeRemove
	"Answer the <Integer> value of the receiver's 'SciterNodeRemove' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeRemove)!

SciterNodeRemove: anInteger
	"Set the receiver's 'SciterNodeRemove' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeRemove) put: anInteger!

SciterNodeSetText
	"Answer the <Integer> value of the receiver's 'SciterNodeSetText' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeSetText)!

SciterNodeSetText: anInteger
	"Set the receiver's 'SciterNodeSetText' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeSetText) put: anInteger!

SciterNodeType
	"Answer the <Integer> value of the receiver's 'SciterNodeType' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeType)!

SciterNodeType: anInteger
	"Set the receiver's 'SciterNodeType' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterNodeType) put: anInteger!

SciterOpenArchive
	"Answer the <Integer> value of the receiver's 'SciterOpenArchive' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterOpenArchive)!

SciterOpenArchive: anInteger
	"Set the receiver's 'SciterOpenArchive' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterOpenArchive) put: anInteger!

SciterPostCallback
	"Answer the <Integer> value of the receiver's 'SciterPostCallback' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterPostCallback)!

SciterPostCallback: anInteger
	"Set the receiver's 'SciterPostCallback' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterPostCallback) put: anInteger!

SciterPostEvent
	"Answer the <Integer> value of the receiver's 'SciterPostEvent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterPostEvent)!

SciterPostEvent: anInteger
	"Set the receiver's 'SciterPostEvent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterPostEvent) put: anInteger!

SciterProc
	"Answer the <Integer> value of the receiver's 'SciterProc' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterProc)!

SciterProc: anInteger
	"Set the receiver's 'SciterProc' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterProc) put: anInteger!

SciterProcND
	"Answer the <Integer> value of the receiver's 'SciterProcND' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterProcND)!

SciterProcND: anInteger
	"Set the receiver's 'SciterProcND' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterProcND) put: anInteger!

SciterRefreshElementArea
	"Answer the <Integer> value of the receiver's 'SciterRefreshElementArea' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterRefreshElementArea)!

SciterRefreshElementArea: anInteger
	"Set the receiver's 'SciterRefreshElementArea' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterRefreshElementArea) put: anInteger!

SciterReleaseCapture
	"Answer the <Integer> value of the receiver's 'SciterReleaseCapture' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterReleaseCapture)!

SciterReleaseCapture: anInteger
	"Set the receiver's 'SciterReleaseCapture' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterReleaseCapture) put: anInteger!

SciterRenderD2D
	"Answer the <Integer> value of the receiver's 'SciterRenderD2D' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderD2D)!

SciterRenderD2D: anInteger
	"Set the receiver's 'SciterRenderD2D' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderD2D) put: anInteger!

SciterRenderOnDirectXTexture
	"Answer the <Integer> value of the receiver's 'SciterRenderOnDirectXTexture' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderOnDirectXTexture)!

SciterRenderOnDirectXTexture: anInteger
	"Set the receiver's 'SciterRenderOnDirectXTexture' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderOnDirectXTexture) put: anInteger!

SciterRenderOnDirectXWindow
	"Answer the <Integer> value of the receiver's 'SciterRenderOnDirectXWindow' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderOnDirectXWindow)!

SciterRenderOnDirectXWindow: anInteger
	"Set the receiver's 'SciterRenderOnDirectXWindow' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterRenderOnDirectXWindow) put: anInteger!

SciterRequestElementData
	"Answer the <Integer> value of the receiver's 'SciterRequestElementData' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterRequestElementData)!

SciterRequestElementData: anInteger
	"Set the receiver's 'SciterRequestElementData' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterRequestElementData) put: anInteger!

SciterScrollToView
	"Answer the <Integer> value of the receiver's 'SciterScrollToView' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterScrollToView)!

SciterScrollToView: anInteger
	"Set the receiver's 'SciterScrollToView' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterScrollToView) put: anInteger!

SciterSelectElements
	"Answer the <Integer> value of the receiver's 'SciterSelectElements' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectElements)!

SciterSelectElements: anInteger
	"Set the receiver's 'SciterSelectElements' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectElements) put: anInteger!

SciterSelectElementsW
	"Answer the <Integer> value of the receiver's 'SciterSelectElementsW' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectElementsW)!

SciterSelectElementsW: anInteger
	"Set the receiver's 'SciterSelectElementsW' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectElementsW) put: anInteger!

SciterSelectParent
	"Answer the <Integer> value of the receiver's 'SciterSelectParent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectParent)!

SciterSelectParent: anInteger
	"Set the receiver's 'SciterSelectParent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectParent) put: anInteger!

SciterSelectParentW
	"Answer the <Integer> value of the receiver's 'SciterSelectParentW' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectParentW)!

SciterSelectParentW: anInteger
	"Set the receiver's 'SciterSelectParentW' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSelectParentW) put: anInteger!

SciterSendEvent
	"Answer the <Integer> value of the receiver's 'SciterSendEvent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSendEvent)!

SciterSendEvent: anInteger
	"Set the receiver's 'SciterSendEvent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSendEvent) put: anInteger!

SciterSetAttributeByName
	"Answer the <Integer> value of the receiver's 'SciterSetAttributeByName' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetAttributeByName)!

SciterSetAttributeByName: anInteger
	"Set the receiver's 'SciterSetAttributeByName' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetAttributeByName) put: anInteger!

SciterSetCallback
	"Answer the <Integer> value of the receiver's 'SciterSetCallback' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCallback)!

SciterSetCallback: anInteger
	"Set the receiver's 'SciterSetCallback' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCallback) put: anInteger!

SciterSetCapture
	"Answer the <Integer> value of the receiver's 'SciterSetCapture' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCapture)!

SciterSetCapture: anInteger
	"Set the receiver's 'SciterSetCapture' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCapture) put: anInteger!

SciterSetCSS
	"Answer the <Integer> value of the receiver's 'SciterSetCSS' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCSS)!

SciterSetCSS: anInteger
	"Set the receiver's 'SciterSetCSS' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetCSS) put: anInteger!

SciterSetElementHtml
	"Answer the <Integer> value of the receiver's 'SciterSetElementHtml' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementHtml)!

SciterSetElementHtml: anInteger
	"Set the receiver's 'SciterSetElementHtml' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementHtml) put: anInteger!

SciterSetElementState
	"Answer the <Integer> value of the receiver's 'SciterSetElementState' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementState)!

SciterSetElementState: anInteger
	"Set the receiver's 'SciterSetElementState' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementState) put: anInteger!

SciterSetElementText
	"Answer the <Integer> value of the receiver's 'SciterSetElementText' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementText)!

SciterSetElementText: anInteger
	"Set the receiver's 'SciterSetElementText' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetElementText) put: anInteger!

SciterSetHighlightedElement
	"Answer the <Integer> value of the receiver's 'SciterSetHighlightedElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetHighlightedElement)!

SciterSetHighlightedElement: anInteger
	"Set the receiver's 'SciterSetHighlightedElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetHighlightedElement) put: anInteger!

SciterSetHomeURL
	"Answer the <Integer> value of the receiver's 'SciterSetHomeURL' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetHomeURL)!

SciterSetHomeURL: anInteger
	"Set the receiver's 'SciterSetHomeURL' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetHomeURL) put: anInteger!

SciterSetMasterCSS
	"Answer the <Integer> value of the receiver's 'SciterSetMasterCSS' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMasterCSS)!

SciterSetMasterCSS: anInteger
	"Set the receiver's 'SciterSetMasterCSS' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMasterCSS) put: anInteger!

SciterSetMediaType
	"Answer the <Integer> value of the receiver's 'SciterSetMediaType' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMediaType)!

SciterSetMediaType: anInteger
	"Set the receiver's 'SciterSetMediaType' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMediaType) put: anInteger!

SciterSetMediaVars
	"Answer the <Integer> value of the receiver's 'SciterSetMediaVars' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMediaVars)!

SciterSetMediaVars: anInteger
	"Set the receiver's 'SciterSetMediaVars' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetMediaVars) put: anInteger!

SciterSetOption
	"Answer the <Integer> value of the receiver's 'SciterSetOption' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetOption)!

SciterSetOption: anInteger
	"Set the receiver's 'SciterSetOption' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetOption) put: anInteger!

SciterSetScrollPos
	"Answer the <Integer> value of the receiver's 'SciterSetScrollPos' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetScrollPos)!

SciterSetScrollPos: anInteger
	"Set the receiver's 'SciterSetScrollPos' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetScrollPos) put: anInteger!

SciterSetStyleAttribute
	"Answer the <Integer> value of the receiver's 'SciterSetStyleAttribute' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetStyleAttribute)!

SciterSetStyleAttribute: anInteger
	"Set the receiver's 'SciterSetStyleAttribute' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetStyleAttribute) put: anInteger!

SciterSetTimer
	"Answer the <Integer> value of the receiver's 'SciterSetTimer' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetTimer)!

SciterSetTimer: anInteger
	"Set the receiver's 'SciterSetTimer' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetTimer) put: anInteger!

SciterSetupDebugOutput
	"Answer the <Integer> value of the receiver's 'SciterSetupDebugOutput' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetupDebugOutput)!

SciterSetupDebugOutput: anInteger
	"Set the receiver's 'SciterSetupDebugOutput' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetupDebugOutput) put: anInteger!

SciterSetValue
	"Answer the <Integer> value of the receiver's 'SciterSetValue' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSetValue)!

SciterSetValue: anInteger
	"Set the receiver's 'SciterSetValue' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSetValue) put: anInteger!

SciterShowPopup
	"Answer the <Integer> value of the receiver's 'SciterShowPopup' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterShowPopup)!

SciterShowPopup: anInteger
	"Set the receiver's 'SciterShowPopup' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterShowPopup) put: anInteger!

SciterShowPopupAt
	"Answer the <Integer> value of the receiver's 'SciterShowPopupAt' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterShowPopupAt)!

SciterShowPopupAt: anInteger
	"Set the receiver's 'SciterShowPopupAt' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterShowPopupAt) put: anInteger!

SciterSortElements
	"Answer the <Integer> value of the receiver's 'SciterSortElements' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSortElements)!

SciterSortElements: anInteger
	"Set the receiver's 'SciterSortElements' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSortElements) put: anInteger!

SciterSwapElements
	"Answer the <Integer> value of the receiver's 'SciterSwapElements' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterSwapElements)!

SciterSwapElements: anInteger
	"Set the receiver's 'SciterSwapElements' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterSwapElements) put: anInteger!

SciterTranslateMessage
	"Answer the <Integer> value of the receiver's 'SciterTranslateMessage' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterTranslateMessage)!

SciterTranslateMessage: anInteger
	"Set the receiver's 'SciterTranslateMessage' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterTranslateMessage) put: anInteger!

SciterTraverseUIEvent
	"Answer the <Integer> value of the receiver's 'SciterTraverseUIEvent' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterTraverseUIEvent)!

SciterTraverseUIEvent: anInteger
	"Set the receiver's 'SciterTraverseUIEvent' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterTraverseUIEvent) put: anInteger!

SciterUpdateElement
	"Answer the <Integer> value of the receiver's 'SciterUpdateElement' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterUpdateElement)!

SciterUpdateElement: anInteger
	"Set the receiver's 'SciterUpdateElement' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterUpdateElement) put: anInteger!

SciterUpdateWindow
	"Answer the <Integer> value of the receiver's 'SciterUpdateWindow' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterUpdateWindow)!

SciterUpdateWindow: anInteger
	"Set the receiver's 'SciterUpdateWindow' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterUpdateWindow) put: anInteger!

SciterVersion
	"Answer the <Integer> value of the receiver's 'SciterVersion' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterVersion)!

SciterVersion: anInteger
	"Set the receiver's 'SciterVersion' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterVersion) put: anInteger!

SciterWindowAttachEventHandler
	"Answer the <Integer> value of the receiver's 'SciterWindowAttachEventHandler' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterWindowAttachEventHandler)!

SciterWindowAttachEventHandler: anInteger
	"Set the receiver's 'SciterWindowAttachEventHandler' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterWindowAttachEventHandler) put: anInteger!

SciterWindowDetachEventHandler
	"Answer the <Integer> value of the receiver's 'SciterWindowDetachEventHandler' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #SciterWindowDetachEventHandler)!

SciterWindowDetachEventHandler: anInteger
	"Set the receiver's 'SciterWindowDetachEventHandler' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #SciterWindowDetachEventHandler) put: anInteger!

TIScriptAPI
	"Answer the <Integer> value of the receiver's 'TIScriptAPI' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #TIScriptAPI)!

TIScriptAPI: anInteger
	"Set the receiver's 'TIScriptAPI' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #TIScriptAPI) put: anInteger!

ValueBinaryData
	"Answer the <Integer> value of the receiver's 'ValueBinaryData' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueBinaryData)!

ValueBinaryData: anInteger
	"Set the receiver's 'ValueBinaryData' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueBinaryData) put: anInteger!

ValueBinaryDataSet
	"Answer the <Integer> value of the receiver's 'ValueBinaryDataSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueBinaryDataSet)!

ValueBinaryDataSet: anInteger
	"Set the receiver's 'ValueBinaryDataSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueBinaryDataSet) put: anInteger!

ValueClear
	"Answer the <Integer> value of the receiver's 'ValueClear' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueClear)!

ValueClear: anInteger
	"Set the receiver's 'ValueClear' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueClear) put: anInteger!

ValueCompare
	"Answer the <Integer> value of the receiver's 'ValueCompare' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueCompare)!

ValueCompare: anInteger
	"Set the receiver's 'ValueCompare' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueCompare) put: anInteger!

ValueCopy
	"Answer the <Integer> value of the receiver's 'ValueCopy' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueCopy)!

ValueCopy: anInteger
	"Set the receiver's 'ValueCopy' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueCopy) put: anInteger!

ValueElementsCount
	"Answer the <Integer> value of the receiver's 'ValueElementsCount' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueElementsCount)!

ValueElementsCount: anInteger
	"Set the receiver's 'ValueElementsCount' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueElementsCount) put: anInteger!

ValueEnumElements
	"Answer the <Integer> value of the receiver's 'ValueEnumElements' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueEnumElements)!

ValueEnumElements: anInteger
	"Set the receiver's 'ValueEnumElements' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueEnumElements) put: anInteger!

ValueFloatData
	"Answer the <Integer> value of the receiver's 'ValueFloatData' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueFloatData)!

ValueFloatData: anInteger
	"Set the receiver's 'ValueFloatData' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueFloatData) put: anInteger!

ValueFloatDataSet
	"Answer the <Integer> value of the receiver's 'ValueFloatDataSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueFloatDataSet)!

ValueFloatDataSet: anInteger
	"Set the receiver's 'ValueFloatDataSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueFloatDataSet) put: anInteger!

ValueFromString
	"Answer the <Integer> value of the receiver's 'ValueFromString' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueFromString)!

ValueFromString: anInteger
	"Set the receiver's 'ValueFromString' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueFromString) put: anInteger!

ValueGetValueOfKey
	"Answer the <Integer> value of the receiver's 'ValueGetValueOfKey' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueGetValueOfKey)!

ValueGetValueOfKey: anInteger
	"Set the receiver's 'ValueGetValueOfKey' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueGetValueOfKey) put: anInteger!

ValueInit
	"Answer the <Integer> value of the receiver's 'ValueInit' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueInit)!

ValueInit: anInteger
	"Set the receiver's 'ValueInit' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueInit) put: anInteger!

ValueInt64Data
	"Answer the <Integer> value of the receiver's 'ValueInt64Data' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueInt64Data)!

ValueInt64Data: anInteger
	"Set the receiver's 'ValueInt64Data' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueInt64Data) put: anInteger!

ValueInt64DataSet
	"Answer the <Integer> value of the receiver's 'ValueInt64DataSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueInt64DataSet)!

ValueInt64DataSet: anInteger
	"Set the receiver's 'ValueInt64DataSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueInt64DataSet) put: anInteger!

ValueIntData
	"Answer the <Integer> value of the receiver's 'ValueIntData' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueIntData)!

ValueIntData: anInteger
	"Set the receiver's 'ValueIntData' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueIntData) put: anInteger!

ValueIntDataSet
	"Answer the <Integer> value of the receiver's 'ValueIntDataSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueIntDataSet)!

ValueIntDataSet: anInteger
	"Set the receiver's 'ValueIntDataSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueIntDataSet) put: anInteger!

ValueInvoke
	"Answer the <Integer> value of the receiver's 'ValueInvoke' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueInvoke)!

ValueInvoke: anInteger
	"Set the receiver's 'ValueInvoke' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueInvoke) put: anInteger!

ValueIsNativeFunctor
	"Answer the <Integer> value of the receiver's 'ValueIsNativeFunctor' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueIsNativeFunctor)!

ValueIsNativeFunctor: anInteger
	"Set the receiver's 'ValueIsNativeFunctor' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueIsNativeFunctor) put: anInteger!

ValueIsolate
	"Answer the <Integer> value of the receiver's 'ValueIsolate' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueIsolate)!

ValueIsolate: anInteger
	"Set the receiver's 'ValueIsolate' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueIsolate) put: anInteger!

ValueNativeFunctorSet
	"Answer the <Integer> value of the receiver's 'ValueNativeFunctorSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueNativeFunctorSet)!

ValueNativeFunctorSet: anInteger
	"Set the receiver's 'ValueNativeFunctorSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueNativeFunctorSet) put: anInteger!

ValueNthElementKey
	"Answer the <Integer> value of the receiver's 'ValueNthElementKey' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementKey)!

ValueNthElementKey: anInteger
	"Set the receiver's 'ValueNthElementKey' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementKey) put: anInteger!

ValueNthElementValue
	"Answer the <Integer> value of the receiver's 'ValueNthElementValue' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementValue)!

ValueNthElementValue: anInteger
	"Set the receiver's 'ValueNthElementValue' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementValue) put: anInteger!

ValueNthElementValueSet
	"Answer the <Integer> value of the receiver's 'ValueNthElementValueSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementValueSet)!

ValueNthElementValueSet: anInteger
	"Set the receiver's 'ValueNthElementValueSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueNthElementValueSet) put: anInteger!

ValueSetValueToKey
	"Answer the <Integer> value of the receiver's 'ValueSetValueToKey' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueSetValueToKey)!

ValueSetValueToKey: anInteger
	"Set the receiver's 'ValueSetValueToKey' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueSetValueToKey) put: anInteger!

ValueStringData
	"Answer the <Integer> value of the receiver's 'ValueStringData' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueStringData)!

ValueStringData: anInteger
	"Set the receiver's 'ValueStringData' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueStringData) put: anInteger!

ValueStringDataSet
	"Answer the <Integer> value of the receiver's 'ValueStringDataSet' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueStringDataSet)!

ValueStringDataSet: anInteger
	"Set the receiver's 'ValueStringDataSet' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueStringDataSet) put: anInteger!

ValueToString
	"Answer the <Integer> value of the receiver's 'ValueToString' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueToString)!

ValueToString: anInteger
	"Set the receiver's 'ValueToString' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueToString) put: anInteger!

ValueType
	"Answer the <Integer> value of the receiver's 'ValueType' field."

	^bytes intPtrAtOffset: ##(self offsetOf: #ValueType)!

ValueType: anInteger
	"Set the receiver's 'ValueType' field to the value of the argument, anInteger"

	bytes intPtrAtOffset: ##(self offsetOf: #ValueType) put: anInteger!

version
	"Answer the <Integer> value of the receiver's 'version' field."

	^bytes wordAtOffset: 0!

version: anInteger
	"Set the receiver's 'version' field to the value of the argument, anInteger"

	bytes wordAtOffset: 0 put: anInteger! !
!SCITERAPI categoriesFor: #GetSciterGraphicsAPI!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #GetSciterGraphicsAPI:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #GetSciterRequestAPI!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #GetSciterRequestAPI:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_tv2V!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_tv2V:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_UnuseElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_UnuseElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_UseElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_UseElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_V2tv!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #Sciter_V2tv:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAppendMasterCSS!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAppendMasterCSS:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAttachEventHandler!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAttachEventHandler:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAttachHwndToElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterAttachHwndToElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCall!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCall:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallBehaviorMethod!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallBehaviorMethod:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallScriptingFunction!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallScriptingFunction:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallScriptingMethod!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCallScriptingMethod:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterClassName!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterClassName:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterClearAttributes!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterClearAttributes:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCloneElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCloneElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCloseArchive!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCloseArchive:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCombineURL!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCombineURL:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterControlGetType!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterControlGetType:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateCommentNode!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateCommentNode:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateOnDirectXWindow!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateOnDirectXWindow:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateTextNode!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateTextNode:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateWindow!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterCreateWindow:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterD2DFactory!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterD2DFactory:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDataReady!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDataReady:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDataReadyAsync!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDataReadyAsync:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDeleteElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDeleteElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDetachElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDetachElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDetachEventHandler!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDetachEventHandler:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDWFactory!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterDWFactory:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterEval!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterEval:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterEvalElementScript!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterEvalElementScript:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterFindElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterFindElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterFireEvent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterFireEvent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetArchiveItem!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetArchiveItem:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetAttributeByNameCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetAttributeByNameCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetAttributeCount!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetAttributeCount:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetCallbackParam!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetCallbackParam:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetChildrenCount!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetChildrenCount:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementByUID!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementByUID:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementHtmlCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementHtmlCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementHwnd!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementHwnd:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIndex!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIndex:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIntrinsicHeight!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIntrinsicHeight:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIntrinsicWidths!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementIntrinsicWidths:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementLocation!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementLocation:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementNamespace!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementNamespace:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementState!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementState:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementTextCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementTextCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementType!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementType:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementTypeCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementTypeCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementUID!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetElementUID:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetExpando!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetExpando:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetFocusElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetFocusElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetHighlightedElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetHighlightedElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetMinHeight!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetMinHeight:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetMinWidth!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetMinWidth:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthAttributeNameCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthAttributeNameCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthAttributeValueCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthAttributeValueCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthChild!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetNthChild:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetObject!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetObject:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetParentElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetParentElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetPPI!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetPPI:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetRootElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetRootElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetScrollInfo!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetScrollInfo:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetStyleAttributeCB!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetStyleAttributeCB:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetValue!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetValue:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetViewExpando!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetViewExpando:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetVM!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGetVM:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGraphicsCaps!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterGraphicsCaps:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterHidePopup!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterHidePopup:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterHttpRequest!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterHttpRequest:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterInsertElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterInsertElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterIsElementEnabled!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterIsElementEnabled:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterIsElementVisible!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterIsElementVisible:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterLoadFile!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterLoadFile:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterLoadHtml!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterLoadHtml:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeAddRef!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeAddRef:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeCastFromElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeCastFromElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeCastToElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeCastToElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeChildrenCount!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeChildrenCount:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeFirstChild!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeFirstChild:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeGetText!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeGetText:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeInsert!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeInsert:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeLastChild!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeLastChild:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeNextSibling!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeNextSibling:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeNthChild!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeNthChild:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeParent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeParent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodePrevSibling!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodePrevSibling:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeRelease!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeRelease:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeRemove!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeRemove:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeSetText!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeSetText:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeType!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterNodeType:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterOpenArchive!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterOpenArchive:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterPostCallback!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterPostCallback:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterPostEvent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterPostEvent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterProc!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterProc:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterProcND!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterProcND:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRefreshElementArea!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRefreshElementArea:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterReleaseCapture!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterReleaseCapture:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderD2D!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderD2D:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderOnDirectXTexture!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderOnDirectXTexture:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderOnDirectXWindow!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRenderOnDirectXWindow:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRequestElementData!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterRequestElementData:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterScrollToView!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterScrollToView:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectElements!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectElements:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectElementsW!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectElementsW:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectParent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectParent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectParentW!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSelectParentW:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSendEvent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSendEvent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetAttributeByName!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetAttributeByName:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCallback!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCallback:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCapture!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCapture:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCSS!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetCSS:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementHtml!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementHtml:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementState!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementState:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementText!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetElementText:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetHighlightedElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetHighlightedElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetHomeURL!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetHomeURL:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMasterCSS!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMasterCSS:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMediaType!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMediaType:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMediaVars!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetMediaVars:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetOption!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetOption:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetScrollPos!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetScrollPos:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetStyleAttribute!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetStyleAttribute:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetTimer!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetTimer:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetupDebugOutput!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetupDebugOutput:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetValue!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSetValue:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterShowPopup!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterShowPopup:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterShowPopupAt!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterShowPopupAt:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSortElements!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSortElements:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSwapElements!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterSwapElements:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterTranslateMessage!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterTranslateMessage:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterTraverseUIEvent!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterTraverseUIEvent:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterUpdateElement!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterUpdateElement:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterUpdateWindow!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterUpdateWindow:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterVersion!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterVersion:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterWindowAttachEventHandler!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterWindowAttachEventHandler:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterWindowDetachEventHandler!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #SciterWindowDetachEventHandler:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #TIScriptAPI!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #TIScriptAPI:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueBinaryData!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueBinaryData:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueBinaryDataSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueBinaryDataSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueClear!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueClear:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueCompare!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueCompare:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueCopy!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueCopy:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueElementsCount!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueElementsCount:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueEnumElements!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueEnumElements:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFloatData!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFloatData:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFloatDataSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFloatDataSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFromString!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueFromString:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueGetValueOfKey!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueGetValueOfKey:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInit!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInit:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInt64Data!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInt64Data:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInt64DataSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInt64DataSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIntData!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIntData:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIntDataSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIntDataSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInvoke!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueInvoke:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIsNativeFunctor!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIsNativeFunctor:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIsolate!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueIsolate:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNativeFunctorSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNativeFunctorSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementKey!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementKey:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementValue!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementValue:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementValueSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueNthElementValueSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueSetValueToKey!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueSetValueToKey:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueStringData!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueStringData:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueStringDataSet!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueStringDataSet:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueToString!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueToString:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueType!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #ValueType:!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #version!**compiled accessors**!public! !
!SCITERAPI categoriesFor: #version:!**compiled accessors**!public! !

!SCITERAPI class methodsFor!

defineFields
	self
		defineField: #version type: WORDField new; "UINT version; // is zero for now"
		defineField: #SciterClassName type: INT_PTRField new ;
		defineField: #SciterVersion type: INT_PTRField new ;
		defineField: #SciterDataReady type: INT_PTRField new ;
		defineField: #SciterDataReadyAsync type: INT_PTRField new ;
		defineField: #SciterProc type: INT_PTRField new ;
		defineField: #SciterProcND type: INT_PTRField new ;
		defineField: #SciterLoadFile type: INT_PTRField new ;
		defineField: #SciterLoadHtml type: INT_PTRField new ;
		defineField: #SciterSetCallback type: INT_PTRField new ;
		defineField: #SciterSetMasterCSS type: INT_PTRField new ;
		defineField: #SciterAppendMasterCSS type: INT_PTRField new ;
		defineField: #SciterSetCSS type: INT_PTRField new ;
		defineField: #SciterSetMediaType type: INT_PTRField new ;
		defineField: #SciterSetMediaVars type: INT_PTRField new ;
		defineField: #SciterGetMinWidth type: INT_PTRField new ;
		defineField: #SciterGetMinHeight type: INT_PTRField new ;
		defineField: #SciterCall type: INT_PTRField new ;
		defineField: #SciterEval type: INT_PTRField new ;
		defineField: #SciterUpdateWindow type: INT_PTRField new ;
		defineField: #SciterTranslateMessage type: INT_PTRField new ;
		defineField: #SciterSetOption type: INT_PTRField new ;
		defineField: #SciterGetPPI type: INT_PTRField new ;
		defineField: #SciterGetViewExpando type: INT_PTRField new ;
		defineField: #SciterRenderD2D type: INT_PTRField new ;
		defineField: #SciterD2DFactory type: INT_PTRField new ;
		defineField: #SciterDWFactory type: INT_PTRField new ;
		defineField: #SciterGraphicsCaps type: INT_PTRField new ;
		defineField: #SciterSetHomeURL type: INT_PTRField new ;
		defineField: #SciterCreateWindow type: INT_PTRField new ;
		defineField: #SciterSetupDebugOutput type: INT_PTRField new ;
		"DOM Element API"
		defineField: #Sciter_UseElement type: INT_PTRField new ;
		defineField: #Sciter_UnuseElement type: INT_PTRField new ;
		defineField: #SciterGetRootElement type: INT_PTRField new ;
		defineField: #SciterGetFocusElement type: INT_PTRField new ;
		defineField: #SciterFindElement type: INT_PTRField new ;
		defineField: #SciterGetChildrenCount type: INT_PTRField new ;
		defineField: #SciterGetNthChild	 type: INT_PTRField new ;
		defineField: #SciterGetParentElement type: INT_PTRField new ;
		defineField: #SciterGetElementHtmlCB type: INT_PTRField new ;
		defineField: #SciterGetElementTextCB type: INT_PTRField new ;
		defineField: #SciterSetElementText type: INT_PTRField new ;
		defineField: #SciterGetAttributeCount type: INT_PTRField new ;
		defineField: #SciterGetNthAttributeNameCB type: INT_PTRField new ;
		defineField: #SciterGetNthAttributeValueCB type: INT_PTRField new ;
		defineField: #SciterGetAttributeByNameCB type: INT_PTRField new ;
		defineField: #SciterSetAttributeByName type: INT_PTRField new ;
		defineField: #SciterClearAttributes type: INT_PTRField new ;
		defineField: #SciterGetElementIndex type: INT_PTRField new ;
		defineField: #SciterGetElementType type: INT_PTRField new ;
		defineField: #SciterGetElementTypeCB type: INT_PTRField new ;
		defineField: #SciterGetStyleAttributeCB type: INT_PTRField new ;
		defineField: #SciterSetStyleAttribute type: INT_PTRField new ;
		defineField: #SciterGetElementLocation type: INT_PTRField new ;
		"ELEMENT_AREAS"
		defineField: #SciterScrollToView type: INT_PTRField new ;
		defineField: #SciterUpdateElement type: INT_PTRField new ;
		defineField: #SciterRefreshElementArea type: INT_PTRField new ;
		defineField: #SciterSetCapture type: INT_PTRField new ;
		defineField: #SciterReleaseCapture type: INT_PTRField new ;
		defineField: #SciterGetElementHwnd type: INT_PTRField new ;
		defineField: #SciterCombineURL type: INT_PTRField new ;
		defineField: #SciterSelectElements type: INT_PTRField new ;
		defineField: #SciterSelectElementsW type: INT_PTRField new ;
		defineField: #SciterSelectParent type: INT_PTRField new ;
		defineField: #SciterSelectParentW type: INT_PTRField new ;
		defineField: #SciterSetElementHtml type: INT_PTRField new ;
		defineField: #SciterGetElementUID type: INT_PTRField new ;
		defineField: #SciterGetElementByUID type: INT_PTRField new ;
		defineField: #SciterShowPopup type: INT_PTRField new ;
		defineField: #SciterShowPopupAt type: INT_PTRField new ;
		defineField: #SciterHidePopup type: INT_PTRField new ;
		defineField: #SciterGetElementState type: INT_PTRField new ;
		defineField: #SciterSetElementState type: INT_PTRField new ;
		defineField: #SciterCreateElement type: INT_PTRField new ;
		defineField: #SciterCloneElement type: INT_PTRField new ;
		defineField: #SciterInsertElement type: INT_PTRField new ;
		defineField: #SciterDetachElement type: INT_PTRField new ;
		defineField: #SciterDeleteElement type: INT_PTRField new ;
		defineField: #SciterSetTimer type: INT_PTRField new ;
		defineField: #SciterDetachEventHandler type: INT_PTRField new ;
    		defineField: #SciterAttachEventHandler type: INT_PTRField new ;
		defineField: #SciterWindowAttachEventHandler type: INT_PTRField new ;
		defineField: #SciterWindowDetachEventHandler type: INT_PTRField new ;
		defineField: #SciterSendEvent type: INT_PTRField new ;
		defineField: #SciterPostEvent type: INT_PTRField new ;
		defineField: #SciterCallBehaviorMethod type: INT_PTRField new ;
		defineField: #SciterRequestElementData type: INT_PTRField new ;
		defineField: #SciterHttpRequest type: INT_PTRField new ;

		defineField: #SciterGetScrollInfo type: INT_PTRField new ;
		defineField: #SciterSetScrollPos type: INT_PTRField new ;
		defineField: #SciterGetElementIntrinsicWidths type: INT_PTRField new ;
		defineField: #SciterGetElementIntrinsicHeight type: INT_PTRField new ;
		defineField: #SciterIsElementVisible type: INT_PTRField new ;
		defineField: #SciterIsElementEnabled type: INT_PTRField new ;
		defineField: #SciterSortElements type: INT_PTRField new ;
		defineField: #SciterSwapElements type: INT_PTRField new ;
		defineField: #SciterTraverseUIEvent type: INT_PTRField new ;
		defineField: #SciterCallScriptingMethod type: INT_PTRField new ;
		defineField: #SciterCallScriptingFunction type: INT_PTRField new ;
		defineField: #SciterEvalElementScript type: INT_PTRField new ;
		defineField: #SciterAttachHwndToElement type: INT_PTRField new ;
		defineField: #SciterControlGetType type: INT_PTRField new ;
		defineField: #SciterGetValue type: INT_PTRField new ;
		defineField: #SciterSetValue type: INT_PTRField new ;
		defineField: #SciterGetExpando type: INT_PTRField new ;
		defineField: #SciterGetObject type: INT_PTRField new ;
		defineField: #SciterGetElementNamespace type: INT_PTRField new ;
		defineField: #SciterGetHighlightedElement type: INT_PTRField new ;
		defineField: #SciterSetHighlightedElement type: INT_PTRField new ;
		"DOM Node API"
		defineField: #SciterNodeAddRef type: INT_PTRField new ;
		defineField: #SciterNodeRelease type: INT_PTRField new ;
		defineField: #SciterNodeCastFromElement type: INT_PTRField new ;
		defineField: #SciterNodeCastToElement type: INT_PTRField new ;
		defineField: #SciterNodeFirstChild type: INT_PTRField new ;
		defineField: #SciterNodeLastChild type: INT_PTRField new ;
		defineField: #SciterNodeNextSibling type: INT_PTRField new ;
		defineField: #SciterNodePrevSibling type: INT_PTRField new ;
		defineField: #SciterNodeParent type: INT_PTRField new ;
		defineField: #SciterNodeNthChild type: INT_PTRField new ;
		defineField: #SciterNodeChildrenCount type: INT_PTRField new ;
		defineField: #SciterNodeType type: INT_PTRField new ;
		defineField: #SciterNodeGetText type: INT_PTRField new ;
		defineField: #SciterNodeSetText type: INT_PTRField new ;
		defineField: #SciterNodeInsert type: INT_PTRField new ;
		defineField: #SciterNodeRemove type: INT_PTRField new ;
		defineField: #SciterCreateTextNode type: INT_PTRField new ;
		defineField: #SciterCreateCommentNode type: INT_PTRField new ;
		" VALUE API" 
		defineField: #ValueInit type: INT_PTRField new ;
		defineField: #ValueClear type: INT_PTRField new ;
		defineField: #ValueCompare type: INT_PTRField new ;
		defineField: #ValueCopy type: INT_PTRField new ;
		defineField: #ValueIsolate type: INT_PTRField new ;
		defineField: #ValueType type: INT_PTRField new ;
		defineField: #ValueStringData type: INT_PTRField new ;
		defineField: #ValueStringDataSet type: INT_PTRField new ;
		defineField: #ValueIntData type: INT_PTRField new ;
		defineField: #ValueIntDataSet type: INT_PTRField new ;
		defineField: #ValueInt64Data type: INT_PTRField new ;
		defineField: #ValueInt64DataSet type: INT_PTRField new ;
		defineField: #ValueFloatData type: INT_PTRField new ;
		defineField: #ValueFloatDataSet type: INT_PTRField new ;
		defineField: #ValueBinaryData type: INT_PTRField new ;
		defineField: #ValueBinaryDataSet type: INT_PTRField new ;
		defineField: #ValueElementsCount type: INT_PTRField new ;
		defineField: #ValueNthElementValue type: INT_PTRField new ;
		defineField: #ValueNthElementValueSet type: INT_PTRField new ;
		defineField: #ValueNthElementKey type: INT_PTRField new ;
		defineField: #ValueEnumElements type: INT_PTRField new ;
		defineField: #ValueSetValueToKey type: INT_PTRField new ;
		defineField: #ValueGetValueOfKey type: INT_PTRField new ;
		defineField: #ValueToString type: INT_PTRField new ;
		defineField: #ValueFromString type: INT_PTRField new ;
		defineField: #ValueInvoke type: INT_PTRField new ;
		defineField: #ValueNativeFunctorSet type: INT_PTRField new ;
		defineField: #ValueIsNativeFunctor type: INT_PTRField new ;
		"tiscript VM API"
		defineField: #TIScriptAPI type: INT_PTRField new ;
		defineField: #SciterGetVM type: INT_PTRField new ;
		defineField: #Sciter_tv2V type: INT_PTRField new ;
		defineField: #Sciter_V2tv type: INT_PTRField new ;
		"Archive support"
		defineField: #SciterOpenArchive type: INT_PTRField new ;
		defineField: #SciterGetArchiveItem type: INT_PTRField new ;
		defineField: #SciterCloseArchive type: INT_PTRField new ;
		defineField: #SciterFireEvent type: INT_PTRField new ;
		defineField: #SciterGetCallbackParam type: INT_PTRField new ;
		defineField: #SciterPostCallback type: INT_PTRField new ;
		defineField: #GetSciterGraphicsAPI type: INT_PTRField new ;
		defineField: #GetSciterRequestAPI type: INT_PTRField new ;
		"DirectX"
		defineField: #SciterCreateOnDirectXWindow type: INT_PTRField new ;
		defineField: #SciterRenderOnDirectXWindow type: INT_PTRField new ;
		defineField: #SciterRenderOnDirectXTexture type: INT_PTRField new 


























		

! !
!SCITERAPI class categoriesFor: #defineFields!public! !

SciterGraphicsAPI guid: (GUID fromString: '{c6163e73-0afd-4d82-ac62-fb013e2652b8}')!
SciterGraphicsAPI comment: 'self compileDefinition'!
!SciterGraphicsAPI categoriesForClass!External-Data-Structured! !
!SciterGraphicsAPI methodsFor!

gAddRef
	"Answer the <Integer> value of the receiver's 'gAddRef' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gAddRef)!

gAddRef: anInteger
	"Set the receiver's 'gAddRef' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gAddRef) put: anInteger!

gArc
	"Answer the <Integer> value of the receiver's 'gArc' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gArc)!

gArc: anInteger
	"Set the receiver's 'gArc' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gArc) put: anInteger!

gCreate
	"Answer the <Integer> value of the receiver's 'gCreate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gCreate)!

gCreate: anInteger
	"Set the receiver's 'gCreate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gCreate) put: anInteger!

gDrawImage
	"Answer the <Integer> value of the receiver's 'gDrawImage' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gDrawImage)!

gDrawImage: anInteger
	"Set the receiver's 'gDrawImage' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gDrawImage) put: anInteger!

gDrawPath
	"Answer the <Integer> value of the receiver's 'gDrawPath' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gDrawPath)!

gDrawPath: anInteger
	"Set the receiver's 'gDrawPath' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gDrawPath) put: anInteger!

gDrawText
	"Answer the <Integer> value of the receiver's 'gDrawText' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gDrawText)!

gDrawText: anInteger
	"Set the receiver's 'gDrawText' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gDrawText) put: anInteger!

gEllipse
	"Answer the <Integer> value of the receiver's 'gEllipse' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gEllipse)!

gEllipse: anInteger
	"Set the receiver's 'gEllipse' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gEllipse) put: anInteger!

gFillColor
	"Answer the <Integer> value of the receiver's 'gFillColor' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gFillColor)!

gFillColor: anInteger
	"Set the receiver's 'gFillColor' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gFillColor) put: anInteger!

gFillGradientLinear
	"Answer the <Integer> value of the receiver's 'gFillGradientLinear' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gFillGradientLinear)!

gFillGradientLinear: anInteger
	"Set the receiver's 'gFillGradientLinear' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gFillGradientLinear) put: anInteger!

gFillGradientRadial
	"Answer the <Integer> value of the receiver's 'gFillGradientRadial' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gFillGradientRadial)!

gFillGradientRadial: anInteger
	"Set the receiver's 'gFillGradientRadial' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gFillGradientRadial) put: anInteger!

gFillMode
	"Answer the <Integer> value of the receiver's 'gFillMode' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gFillMode)!

gFillMode: anInteger
	"Set the receiver's 'gFillMode' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gFillMode) put: anInteger!

gLine
	"Answer the <Integer> value of the receiver's 'gLine' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLine)!

gLine: anInteger
	"Set the receiver's 'gLine' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLine) put: anInteger!

gLineCap
	"Answer the <Integer> value of the receiver's 'gLineCap' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineCap)!

gLineCap: anInteger
	"Set the receiver's 'gLineCap' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineCap) put: anInteger!

gLineColor
	"Answer the <Integer> value of the receiver's 'gLineColor' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineColor)!

gLineColor: anInteger
	"Set the receiver's 'gLineColor' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineColor) put: anInteger!

gLineGradientLinear
	"Answer the <Integer> value of the receiver's 'gLineGradientLinear' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineGradientLinear)!

gLineGradientLinear: anInteger
	"Set the receiver's 'gLineGradientLinear' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineGradientLinear) put: anInteger!

gLineGradientRadial
	"Answer the <Integer> value of the receiver's 'gLineGradientRadial' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineGradientRadial)!

gLineGradientRadial: anInteger
	"Set the receiver's 'gLineGradientRadial' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineGradientRadial) put: anInteger!

gLineJoin
	"Answer the <Integer> value of the receiver's 'gLineJoin' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineJoin)!

gLineJoin: anInteger
	"Set the receiver's 'gLineJoin' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineJoin) put: anInteger!

gLineWidth
	"Answer the <Integer> value of the receiver's 'gLineWidth' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gLineWidth)!

gLineWidth: anInteger
	"Set the receiver's 'gLineWidth' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gLineWidth) put: anInteger!

gPolygon
	"Answer the <Integer> value of the receiver's 'gPolygon' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gPolygon)!

gPolygon: anInteger
	"Set the receiver's 'gPolygon' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gPolygon) put: anInteger!

gPolyline
	"Answer the <Integer> value of the receiver's 'gPolyline' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gPolyline)!

gPolyline: anInteger
	"Set the receiver's 'gPolyline' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gPolyline) put: anInteger!

gPopClip
	"Answer the <Integer> value of the receiver's 'gPopClip' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gPopClip)!

gPopClip: anInteger
	"Set the receiver's 'gPopClip' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gPopClip) put: anInteger!

gPushClipBox
	"Answer the <Integer> value of the receiver's 'gPushClipBox' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gPushClipBox)!

gPushClipBox: anInteger
	"Set the receiver's 'gPushClipBox' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gPushClipBox) put: anInteger!

gPushClipPath
	"Answer the <Integer> value of the receiver's 'gPushClipPath' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gPushClipPath)!

gPushClipPath: anInteger
	"Set the receiver's 'gPushClipPath' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gPushClipPath) put: anInteger!

gRectangle
	"Answer the <Integer> value of the receiver's 'gRectangle' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gRectangle)!

gRectangle: anInteger
	"Set the receiver's 'gRectangle' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gRectangle) put: anInteger!

gRelease
	"Answer the <Integer> value of the receiver's 'gRelease' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gRelease)!

gRelease: anInteger
	"Set the receiver's 'gRelease' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gRelease) put: anInteger!

gRotate
	"Answer the <Integer> value of the receiver's 'gRotate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gRotate)!

gRotate: anInteger
	"Set the receiver's 'gRotate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gRotate) put: anInteger!

gRoundedRectangle
	"Answer the <Integer> value of the receiver's 'gRoundedRectangle' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gRoundedRectangle)!

gRoundedRectangle: anInteger
	"Set the receiver's 'gRoundedRectangle' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gRoundedRectangle) put: anInteger!

gScale
	"Answer the <Integer> value of the receiver's 'gScale' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gScale)!

gScale: anInteger
	"Set the receiver's 'gScale' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gScale) put: anInteger!

gScreenToWorld
	"Answer the <Integer> value of the receiver's 'gScreenToWorld' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gScreenToWorld)!

gScreenToWorld: anInteger
	"Set the receiver's 'gScreenToWorld' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gScreenToWorld) put: anInteger!

gSkew
	"Answer the <Integer> value of the receiver's 'gSkew' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gSkew)!

gSkew: anInteger
	"Set the receiver's 'gSkew' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gSkew) put: anInteger!

gStar
	"Answer the <Integer> value of the receiver's 'gStar' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gStar)!

gStar: anInteger
	"Set the receiver's 'gStar' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gStar) put: anInteger!

gStateRestore
	"Answer the <Integer> value of the receiver's 'gStateRestore' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gStateRestore)!

gStateRestore: anInteger
	"Set the receiver's 'gStateRestore' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gStateRestore) put: anInteger!

gStateSave
	"Answer the <Integer> value of the receiver's 'gStateSave' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gStateSave)!

gStateSave: anInteger
	"Set the receiver's 'gStateSave' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gStateSave) put: anInteger!

gTransform
	"Answer the <Integer> value of the receiver's 'gTransform' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gTransform)!

gTransform: anInteger
	"Set the receiver's 'gTransform' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gTransform) put: anInteger!

gTranslate
	"Answer the <Integer> value of the receiver's 'gTranslate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gTranslate)!

gTranslate: anInteger
	"Set the receiver's 'gTranslate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gTranslate) put: anInteger!

gWorldToScreen
	"Answer the <Integer> value of the receiver's 'gWorldToScreen' field."

	^bytes dwordAtOffset: ##(self offsetOf: #gWorldToScreen)!

gWorldToScreen: anInteger
	"Set the receiver's 'gWorldToScreen' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #gWorldToScreen) put: anInteger!

imageAddRef
	"Answer the <Integer> value of the receiver's 'imageAddRef' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageAddRef)!

imageAddRef: anInteger
	"Set the receiver's 'imageAddRef' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageAddRef) put: anInteger!

imageClear
	"Answer the <Integer> value of the receiver's 'imageClear' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageClear)!

imageClear: anInteger
	"Set the receiver's 'imageClear' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageClear) put: anInteger!

imageCreate
	"Answer the <Integer> value of the receiver's 'imageCreate' field."

	^bytes dwordAtOffset: 0!

imageCreate: anInteger
	"Set the receiver's 'imageCreate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: 0 put: anInteger!

imageCreateFromPixmap
	"Answer the <Integer> value of the receiver's 'imageCreateFromPixmap' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageCreateFromPixmap)!

imageCreateFromPixmap: anInteger
	"Set the receiver's 'imageCreateFromPixmap' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageCreateFromPixmap) put: anInteger!

imageGetInfo
	"Answer the <Integer> value of the receiver's 'imageGetInfo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageGetInfo)!

imageGetInfo: anInteger
	"Set the receiver's 'imageGetInfo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageGetInfo) put: anInteger!

imageLoad
	"Answer the <Integer> value of the receiver's 'imageLoad' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageLoad)!

imageLoad: anInteger
	"Set the receiver's 'imageLoad' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageLoad) put: anInteger!

imagePaint
	"Answer the <Integer> value of the receiver's 'imagePaint' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imagePaint)!

imagePaint: anInteger
	"Set the receiver's 'imagePaint' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imagePaint) put: anInteger!

imageRelease
	"Answer the <Integer> value of the receiver's 'imageRelease' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageRelease)!

imageRelease: anInteger
	"Set the receiver's 'imageRelease' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageRelease) put: anInteger!

imageSave
	"Answer the <Integer> value of the receiver's 'imageSave' field."

	^bytes dwordAtOffset: ##(self offsetOf: #imageSave)!

imageSave: anInteger
	"Set the receiver's 'imageSave' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #imageSave) put: anInteger!

pathAddRef
	"Answer the <Integer> value of the receiver's 'pathAddRef' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathAddRef)!

pathAddRef: anInteger
	"Set the receiver's 'pathAddRef' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathAddRef) put: anInteger!

pathArcTo
	"Answer the <Integer> value of the receiver's 'pathArcTo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathArcTo)!

pathArcTo: anInteger
	"Set the receiver's 'pathArcTo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathArcTo) put: anInteger!

pathBezierCurveTo
	"Answer the <Integer> value of the receiver's 'pathBezierCurveTo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathBezierCurveTo)!

pathBezierCurveTo: anInteger
	"Set the receiver's 'pathBezierCurveTo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathBezierCurveTo) put: anInteger!

pathClosePath
	"Answer the <Integer> value of the receiver's 'pathClosePath' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathClosePath)!

pathClosePath: anInteger
	"Set the receiver's 'pathClosePath' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathClosePath) put: anInteger!

pathCreate
	"Answer the <Integer> value of the receiver's 'pathCreate' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathCreate)!

pathCreate: anInteger
	"Set the receiver's 'pathCreate' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathCreate) put: anInteger!

pathLineTo
	"Answer the <Integer> value of the receiver's 'pathLineTo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathLineTo)!

pathLineTo: anInteger
	"Set the receiver's 'pathLineTo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathLineTo) put: anInteger!

pathMoveTo
	"Answer the <Integer> value of the receiver's 'pathMoveTo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathMoveTo)!

pathMoveTo: anInteger
	"Set the receiver's 'pathMoveTo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathMoveTo) put: anInteger!

pathQuadraticCurveTo
	"Answer the <Integer> value of the receiver's 'pathQuadraticCurveTo' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathQuadraticCurveTo)!

pathQuadraticCurveTo: anInteger
	"Set the receiver's 'pathQuadraticCurveTo' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathQuadraticCurveTo) put: anInteger!

pathRelease
	"Answer the <Integer> value of the receiver's 'pathRelease' field."

	^bytes dwordAtOffset: ##(self offsetOf: #pathRelease)!

pathRelease: anInteger
	"Set the receiver's 'pathRelease' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #pathRelease) put: anInteger!

RGBA
	"Answer the <Integer> value of the receiver's 'RGBA' field."

	^bytes dwordAtOffset: ##(self offsetOf: #RGBA)!

RGBA: anInteger
	"Set the receiver's 'RGBA' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #RGBA) put: anInteger!

textAddRef
	"Answer the <Integer> value of the receiver's 'textAddRef' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textAddRef)!

textAddRef: anInteger
	"Set the receiver's 'textAddRef' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textAddRef) put: anInteger!

textCreateForElement
	"Answer the <Integer> value of the receiver's 'textCreateForElement' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textCreateForElement)!

textCreateForElement: anInteger
	"Set the receiver's 'textCreateForElement' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textCreateForElement) put: anInteger!

textCreateForElementAndStyle
	"Answer the <Integer> value of the receiver's 'textCreateForElementAndStyle' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textCreateForElementAndStyle)!

textCreateForElementAndStyle: anInteger
	"Set the receiver's 'textCreateForElementAndStyle' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textCreateForElementAndStyle) put: anInteger!

textGetMetrics
	"Answer the <Integer> value of the receiver's 'textGetMetrics' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textGetMetrics)!

textGetMetrics: anInteger
	"Set the receiver's 'textGetMetrics' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textGetMetrics) put: anInteger!

textRelease
	"Answer the <Integer> value of the receiver's 'textRelease' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textRelease)!

textRelease: anInteger
	"Set the receiver's 'textRelease' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textRelease) put: anInteger!

textSetBox
	"Answer the <Integer> value of the receiver's 'textSetBox' field."

	^bytes dwordAtOffset: ##(self offsetOf: #textSetBox)!

textSetBox: anInteger
	"Set the receiver's 'textSetBox' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #textSetBox) put: anInteger!

vUnWrapGfx
	"Answer the <Integer> value of the receiver's 'vUnWrapGfx' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vUnWrapGfx)!

vUnWrapGfx: anInteger
	"Set the receiver's 'vUnWrapGfx' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vUnWrapGfx) put: anInteger!

vUnWrapImage
	"Answer the <Integer> value of the receiver's 'vUnWrapImage' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vUnWrapImage)!

vUnWrapImage: anInteger
	"Set the receiver's 'vUnWrapImage' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vUnWrapImage) put: anInteger!

vUnWrapPath
	"Answer the <Integer> value of the receiver's 'vUnWrapPath' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vUnWrapPath)!

vUnWrapPath: anInteger
	"Set the receiver's 'vUnWrapPath' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vUnWrapPath) put: anInteger!

vUnWrapText
	"Answer the <Integer> value of the receiver's 'vUnWrapText' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vUnWrapText)!

vUnWrapText: anInteger
	"Set the receiver's 'vUnWrapText' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vUnWrapText) put: anInteger!

vWrapGfx
	"Answer the <Integer> value of the receiver's 'vWrapGfx' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vWrapGfx)!

vWrapGfx: anInteger
	"Set the receiver's 'vWrapGfx' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vWrapGfx) put: anInteger!

vWrapImage
	"Answer the <Integer> value of the receiver's 'vWrapImage' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vWrapImage)!

vWrapImage: anInteger
	"Set the receiver's 'vWrapImage' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vWrapImage) put: anInteger!

vWrapPath
	"Answer the <Integer> value of the receiver's 'vWrapPath' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vWrapPath)!

vWrapPath: anInteger
	"Set the receiver's 'vWrapPath' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vWrapPath) put: anInteger!

vWrapTex
	"Answer the <Integer> value of the receiver's 'vWrapTex' field."

	^bytes dwordAtOffset: ##(self offsetOf: #vWrapTex)!

vWrapTex: anInteger
	"Set the receiver's 'vWrapTex' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #vWrapTex) put: anInteger! !
!SciterGraphicsAPI categoriesFor: #gAddRef!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gAddRef:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gArc!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gArc:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gCreate!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gCreate:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawImage!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawImage:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawPath!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawPath:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawText!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gDrawText:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gEllipse!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gEllipse:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillColor!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillColor:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillGradientLinear!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillGradientLinear:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillGradientRadial!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillGradientRadial:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillMode!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gFillMode:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLine!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLine:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineCap!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineCap:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineColor!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineColor:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineGradientLinear!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineGradientLinear:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineGradientRadial!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineGradientRadial:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineJoin!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineJoin:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineWidth!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gLineWidth:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPolygon!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPolygon:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPolyline!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPolyline:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPopClip!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPopClip:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPushClipBox!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPushClipBox:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPushClipPath!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gPushClipPath:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRectangle!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRectangle:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRelease!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRelease:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRotate!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRotate:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRoundedRectangle!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gRoundedRectangle:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gScale!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gScale:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gScreenToWorld!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gScreenToWorld:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gSkew!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gSkew:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStar!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStar:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStateRestore!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStateRestore:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStateSave!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gStateSave:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gTransform!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gTransform:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gTranslate!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gTranslate:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gWorldToScreen!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #gWorldToScreen:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageAddRef!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageAddRef:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageClear!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageClear:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageCreate!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageCreate:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageCreateFromPixmap!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageCreateFromPixmap:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageGetInfo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageGetInfo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageLoad!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageLoad:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imagePaint!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imagePaint:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageRelease!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageRelease:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageSave!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #imageSave:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathAddRef!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathAddRef:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathArcTo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathArcTo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathBezierCurveTo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathBezierCurveTo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathClosePath!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathClosePath:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathCreate!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathCreate:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathLineTo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathLineTo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathMoveTo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathMoveTo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathQuadraticCurveTo!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathQuadraticCurveTo:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathRelease!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #pathRelease:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #RGBA!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #RGBA:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textAddRef!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textAddRef:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textCreateForElement!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textCreateForElement:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textCreateForElementAndStyle!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textCreateForElementAndStyle:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textGetMetrics!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textGetMetrics:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textRelease!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textRelease:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textSetBox!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #textSetBox:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapGfx!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapGfx:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapImage!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapImage:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapPath!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapPath:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapText!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vUnWrapText:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapGfx!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapGfx:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapImage!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapImage:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapPath!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapPath:!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapTex!**compiled accessors**!public! !
!SciterGraphicsAPI categoriesFor: #vWrapTex:!**compiled accessors**!public! !

!SciterGraphicsAPI class methodsFor!

defineFields
	self
		defineField: #imageCreate type: DWORDField new; 
		defineField: #imageCreateFromPixmap type: DWORDField new ;
		defineField: #imageAddRef type: DWORDField new;
		defineField: #imageRelease type: DWORDField new;
		defineField: #imageGetInfo type: DWORDField new;
		defineField: #imageClear type: DWORDField new;
		defineField: #imageLoad type: DWORDField new;
		defineField: #imageSave type: DWORDField new;
		defineField: #RGBA type: DWORDField new;
		defineField: #gCreate type: DWORDField new;
		defineField: #gAddRef type: DWORDField new;
		defineField: #gRelease type: DWORDField new;
		defineField: #gLine type: DWORDField new;
		defineField: #gRectangle type: DWORDField new;
		defineField: #gRoundedRectangle type: DWORDField new;
		defineField: #gEllipse type: DWORDField new;
		defineField: #gArc type: DWORDField new;
		defineField: #gStar type: DWORDField new;
		defineField: #gPolygon type: DWORDField new;
		defineField: #gPolyline type: DWORDField new;
		defineField: #pathCreate type: DWORDField new;
		defineField: #pathAddRef type: DWORDField new;
		defineField: #pathRelease type: DWORDField new;
		defineField: #pathMoveTo type: DWORDField new;
		defineField: #pathLineTo type: DWORDField new;
		defineField: #pathArcTo type: DWORDField new;
		defineField: #pathQuadraticCurveTo type: DWORDField new;
		defineField: #pathBezierCurveTo type: DWORDField new;
		defineField: #pathClosePath type: DWORDField new;
		defineField: #gDrawPath type: DWORDField new;
		defineField: #gRotate type: DWORDField new;
		defineField: #gTranslate type: DWORDField new;
		defineField: #gScale type: DWORDField new;
		defineField: #gSkew type: DWORDField new;
		defineField: #gTransform type: DWORDField new;
		defineField: #gStateSave type: DWORDField new;
		defineField: #gStateRestore type: DWORDField new;
		defineField: #gLineWidth type: DWORDField new;
		defineField: #gLineJoin type: DWORDField new;
		defineField: #gLineCap type: DWORDField new;
		defineField: #gLineColor type: DWORDField new;
		defineField: #gFillColor type: DWORDField new;
		defineField: #gLineGradientLinear type: DWORDField new;
		defineField: #gFillGradientLinear type: DWORDField new;
		defineField: #gLineGradientRadial type: DWORDField new;
		defineField: #gFillGradientRadial type: DWORDField new;
		defineField: #gFillMode type: DWORDField new;
		defineField: #textCreateForElement type: DWORDField new;
		defineField: #textCreateForElementAndStyle type: DWORDField new;
		defineField: #textAddRef type: DWORDField new;
		defineField: #textRelease type: DWORDField new;
		defineField: #textGetMetrics type: DWORDField new;
		defineField: #textSetBox type: DWORDField new;
		defineField: #gDrawText type: DWORDField new;
		defineField: #gDrawImage type: DWORDField new;
		defineField: #gWorldToScreen type: DWORDField new;
		defineField: #gScreenToWorld type: DWORDField new;
		defineField: #gPushClipBox type: DWORDField new;
		defineField: #gPushClipPath type: DWORDField new;
		defineField: #gPopClip type: DWORDField new;
		defineField: #imagePaint type: DWORDField new;
		defineField: #vWrapGfx type: DWORDField new;
		defineField: #vWrapImage type: DWORDField new;
		defineField: #vWrapPath type: DWORDField new;
		defineField: #vWrapTex type: DWORDField new;
		defineField: #vUnWrapGfx type: DWORDField new;
		defineField: #vUnWrapImage type: DWORDField new;
		defineField: #vUnWrapPath type: DWORDField new;
		defineField: #vUnWrapText type: DWORDField new


















! !
!SciterGraphicsAPI class categoriesFor: #defineFields!public! !

ValueStructure guid: (GUID fromString: '{ba2cbab1-8761-4369-8733-39bc5eaf489a}')!
ValueStructure comment: ''!
!ValueStructure categoriesForClass!External-Data-Structured! !
!ValueStructure methodsFor!

d
	"Answer the <Integer> value of the receiver's 'd' field."

	^bytes qwordAtOffset: ##(self offsetOf: #d)!

d: anInteger
	"Set the receiver's 'd' field to the value of the argument, anInteger"

	bytes qwordAtOffset: ##(self offsetOf: #d) put: anInteger!

t
	"Answer the <Integer> value of the receiver's 't' field."

	^bytes dwordAtOffset: 0!

t: anInteger
	"Set the receiver's 't' field to the value of the argument, anInteger"

	bytes dwordAtOffset: 0 put: anInteger!

u
	"Answer the <Integer> value of the receiver's 'u' field."

	^bytes dwordAtOffset: ##(self offsetOf: #u)!

u: anInteger
	"Set the receiver's 'u' field to the value of the argument, anInteger"

	bytes dwordAtOffset: ##(self offsetOf: #u) put: anInteger! !
!ValueStructure categoriesFor: #d!**compiled accessors**!public! !
!ValueStructure categoriesFor: #d:!**compiled accessors**!public! !
!ValueStructure categoriesFor: #t!**compiled accessors**!public! !
!ValueStructure categoriesFor: #t:!**compiled accessors**!public! !
!ValueStructure categoriesFor: #u!**compiled accessors**!public! !
!ValueStructure categoriesFor: #u:!**compiled accessors**!public! !

!ValueStructure class methodsFor!

defineFields
	self
		defineField: #t type: DWORDField new;
		defineField: #u type: DWORDField new;
		defineField: #d type: QWORDField new! !
!ValueStructure class categoriesFor: #defineFields!public! !

SCN_ATTACH_BEHAVIOR guid: (GUID fromString: '{4f075039-80e4-4f0b-82b9-41fb63fdb010}')!
SCN_ATTACH_BEHAVIOR comment: ''!
!SCN_ATTACH_BEHAVIOR categoriesForClass!External-Data-Structured! !
SCN_DATA_LOADED guid: (GUID fromString: '{deb9a3c4-ab9e-4fb4-a352-d41b2616e700}')!
SCN_DATA_LOADED comment: ''!
!SCN_DATA_LOADED categoriesForClass!External-Data-Structured! !
!SCN_DATA_LOADED class methodsFor!

defineFields
	" self compileDefinition "
	self
		defineField: #code type: DWORDField readOnly;
		defineField: #hwnd type: HANDLEField readOnly;
		defineField: #uri type:  (PointerField type: Utf16String);
		defineField: #outData type: (ArrayPointerField type: ByteArray lengthField: #outDataSize);
		defineField: #outDataSize type: DWORDField readOnly;
		defineField: #dataType type: DWORDField readOnly;
		defineField: #requestId type: HANDLEField readOnly;
		defineField: #principal type: HANDLEField readOnly;
		defineField: #initiator type: HANDLEField readOnly! !
!SCN_DATA_LOADED class categoriesFor: #defineFields!public! !

SCN_ENGINE_DESTROYED guid: (GUID fromString: '{80d0c6e7-00ec-4210-8c59-79f79bee3184}')!
SCN_ENGINE_DESTROYED comment: ''!
!SCN_ENGINE_DESTROYED categoriesForClass!External-Data-Structured! !
SCN_GRAPHICS_CRITICAL_FAILURE guid: (GUID fromString: '{7f3dc0f6-eccb-494b-9f8e-60b2c86de1e2}')!
SCN_GRAPHICS_CRITICAL_FAILURE comment: ''!
!SCN_GRAPHICS_CRITICAL_FAILURE categoriesForClass!External-Data-Structured! !
SCN_LOAD_DATA guid: (GUID fromString: '{e045b4b2-1de2-4606-b18a-aca71947b976}')!
SCN_LOAD_DATA comment: 'SCN_LOAD_DATA* = object
    code*: uint32     ## #*< [in] one of the codes above.
    hwnd*: HWINDOW    ## #*< [in] HWINDOW of the window this callback was attached to.
    uri*: WideCString ## #*< [in] Zero terminated string, fully qualified uri, for example "http://server/folder/file.ext".
    outData*: pointer ## #*< [in,out] pointer to loaded data to return. if data exists in the cache then this field contain pointer to it
    outDataSize*: uint32 ## #*< [in,out] loaded data size to return.
    dataType*: uint32    ## #*< [in] SciterResourceType
    requestId*: HREQUEST ## #*< [in] request handle that can be used with sciter-x-request API
    principal*: HELEMENT
    initiator*: HELEMENT
'!
!SCN_LOAD_DATA categoriesForClass!External-Data-Structured! !
!SCN_LOAD_DATA methodsFor!

dataType
	"Answer the <Integer> value of the receiver's 'dataType' field."

	^bytes dwordAtOffset: ##(self offsetOf: #dataType)!

initiator
	"Answer the <ExternalHandle> value of the receiver's 'initiator' field."

	^(bytes uintPtrAtOffset: ##(self offsetOf: #initiator)) asExternalHandle!

outData
	"Answer the <BYTE> value of the receiver's 'outData' field."

	^ByteArray fromAddress: (bytes intPtrAtOffset: ##(self offsetOf: #outData)) length: self outDataSize!

outData: aBYTE
	"Set the receiver's 'outData' field to the value of the argument, aBYTE"

	bytes uintPtrAtOffset: ##(self offsetOf: #outData) put: aBYTE yourAddress!

outDataSize
	"Answer the <Integer> value of the receiver's 'outDataSize' field."

	^bytes dwordAtOffset: ##(self offsetOf: #outDataSize)!

principal
	"Answer the <ExternalHandle> value of the receiver's 'principal' field."

	^(bytes uintPtrAtOffset: ##(self offsetOf: #principal)) asExternalHandle!

requestId
	"Answer the <ExternalHandle> value of the receiver's 'requestId' field."

	^(bytes uintPtrAtOffset: ##(self offsetOf: #requestId)) asExternalHandle!

uri
	"Answer the <Utf16String> value of the receiver's 'uri' field."

	^Utf16String fromAddress: (bytes intPtrAtOffset: ##(self offsetOf: #uri))!

uri: anUtf16String
	"Set the receiver's 'uri' field to the value of the argument, anUtf16String"

	bytes uintPtrAtOffset: ##(self offsetOf: #uri) put: anUtf16String yourAddress! !
!SCN_LOAD_DATA categoriesFor: #dataType!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #initiator!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #outData!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #outData:!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #outDataSize!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #principal!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #requestId!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #uri!**compiled accessors**!public! !
!SCN_LOAD_DATA categoriesFor: #uri:!**compiled accessors**!public! !

!SCN_LOAD_DATA class methodsFor!

defineFields
	" self compileDefinition "
	self
		defineField: #code type: DWORDField readOnly;
		defineField: #hwnd type: HANDLEField readOnly;
		defineField: #uri type:  (PointerField type: Utf16String);
		defineField: #outData type: (ArrayPointerField type: ByteArray lengthField: #outDataSize);
		defineField: #outDataSize type: DWORDField readOnly;
		defineField: #dataType type: DWORDField readOnly;
		defineField: #requestId type: HANDLEField readOnly;
		defineField: #principal type: HANDLEField readOnly;
		defineField: #initiator type: HANDLEField readOnly! !
!SCN_LOAD_DATA class categoriesFor: #defineFields!public! !

SCN_POSTED_NOTIFICATION guid: (GUID fromString: '{7c30752e-aa34-481c-9b86-e69844f87ff0}')!
SCN_POSTED_NOTIFICATION comment: ''!
!SCN_POSTED_NOTIFICATION categoriesForClass!External-Data-Structured! !
"Binary Globals"!

