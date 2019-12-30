SciterLibrary closeDefault; clear.
sc:= SciterLibrary default.
sc api.
"OLELibrary default  oleInitialize: nil."
sc SciterClassName.
sc SciterVersionAsString.
(sc SciterVersion: true) asHexString.

sc SciterSetOption: nil option: SCITER_SET_DEBUG_MODE value: 1.
sc SciterSetOption: nil option: SCITER_SET_SCRIPT_RUNTIME_FEATURES value: ALLOW_FILE_IO + ALLOW_SOCKET_IO + ALLOW_EVAL + ALLOW_SYSINFO.
cb :=  sc debugCallBack.
sc SciterSetupDebugOutput: nil param: nil pfOutput: cb asParameter.
"cb free."

r := RECT  left: 1 top: 1 width: 600 height: 300.

sc SciterLoadFile: wnd filename: '.\test1.html'.false
sc SciterLoadFile: wnd filename: '.\particle.html'.

SCN_LOAD_DATA new

dlg free.
dlg := nil.
dlg := sc delegateSciterCallBack.
wnd := sc SciterCreateWindow: SW_CONTROLS +  SW_TITLEBAR + SW_RESIZEABLE frame: r delegate: nil delegateParam: nil parent: nil.

scn := sc sciterHostCallback.
sc SciterSetCallback: wnd cb: scn asParameter cbParam: nil.
htmlw:= '<html> <head><title>Test Html Page</title></head> hello world! </html>'.
sc SciterLoadHtml: wnd html: htmlw htmlSize: htmlw size baseUrl: ''.

root := sc SciterGetRootElement: wnd.

div:=sc SciterCreateElement: 'div' text: 'Весна в Ангарске'.
sc SciterInsertElement: div parent: root index: 0.
div:=sc SciterCreateElement: 'div' text: 'Весна в Ангарске'.
sc SciterInsertElement: div parent: root index: 3.

sc SetTitle: wnd caption:  'Sciter привет из Смаллтока!'.
sc ShowWindow: wnd.
v:=View fromHandle: wnd.
v show
UserLibrary default destroyWindow: wnd

UserLibrary default updateWindow: wnd.
msg := MSG new.
[(UserLibrary default getMessage: msg hWnd: wnd wMsgFilterMin: 0 wMsgFilterMax: 0 )== 1]
	whileTrue: [ 
		(UserLibrary default)
			translateMessage: msg;
			dispatchMessage: msg].

OLELibrary default  oleUninitialize.
