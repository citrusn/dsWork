| package |
package := Package name: 'ftp'.
package paxVersion: 1;
	basicComment: 'FTPSessionBrowser show'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAAAAAAAAUgAAAAMAAABmdHBSAAAADwAAAGM6
XHNtYWxsZnRwLmV4ZZoAAAAAAAAAUgAAAAMAAABmdHBSAAAAEQAAAEZUUFNlc3Npb25NYW5hZ2Vy
778lAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=').

package classNames
	add: #FTPCantOpenDataConnection;
	add: #FTPCdup;
	add: #FTPClosingConection;
	add: #FTPCommandIncorrect;
	add: #FTPConectionClosed;
	add: #FTPControlSocket;
	add: #FTPCwd;
	add: #FTPDataSocket;
	add: #FTPDel;
	add: #FTPDir;
	add: #FTPDownloadingShell;
	add: #FTPEnteringPassiveMode;
	add: #FTPFileSize;
	add: #FTPGet;
	add: #FTPHelp;
	add: #FTPLoginIncorrect;
	add: #FTPMkd;
	add: #FTPNlst;
	add: #FTPOpeningDataConnection;
	add: #FTPPasswordRequired;
	add: #FTPPrintMessage;
	add: #FTPProtocol;
	add: #FTPProtocolShell;
	add: #FTPPut;
	add: #FTPQuit;
	add: #FTPQuitting;
	add: #FTPRawCommand;
	add: #FTPReceivedMessage;
	add: #FTPRename;
	add: #FTPRmd;
	add: #FTPSendingMessage;
	add: #FTPServerReady;
	add: #FTPSession;
	add: #FTPSessionBrowser;
	add: #FTPSessionBrowserShell;
	add: #FTPSessionManager;
	add: #FTPTypeSetTo;
	add: #FTPUserLoggedIn;
	add: #NetWork;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\..\Object Arts\Dolphin\Sockets\Dolphin Sockets';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\Sockets\Sockets Connection';
	yourself).

package!

"Class Definitions"!

Object subclass: #NetWork
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NetWork subclass: #FTPProtocol
	instanceVariableNames: 'readSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPProtocol subclass: #FTPReceivedMessage
	instanceVariableNames: 'message code hide'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPProtocol subclass: #FTPSendingMessage
	instanceVariableNames: 'command'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPProtocol subclass: #FTPSession
	instanceVariableNames: 'controlSocket dataSocket ftpAddress localFileName FTPFileName userName password lastMessage state'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPCantOpenDataConnection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPClosingConection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPConectionClosed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPEnteringPassiveMode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPFileSize
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPLoginIncorrect
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPOpeningDataConnection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPPasswordRequired
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPPrintMessage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPQuitting
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPServerReady
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPTypeSetTo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPReceivedMessage subclass: #FTPUserLoggedIn
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPCdup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPCommandIncorrect
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPCwd
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPDel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPDir
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPGet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPHelp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPMkd
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPNlst
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPPut
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPQuit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPRawCommand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPRename
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSendingMessage subclass: #FTPRmd
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPSession subclass: #FTPSessionBrowser
	instanceVariableNames: 'fTPMessages fTPShell fTPDownloadingShell'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #FTPProtocolShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPProtocolShell subclass: #FTPDownloadingShell
	instanceVariableNames: 'sizeDownloadingPresenter sizeToDownloadPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FTPProtocolShell subclass: #FTPSessionBrowserShell
	instanceVariableNames: 'fTPCommandPresenter fTPMessagesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #FTPSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Socket subclass: #FTPControlSocket
	instanceVariableNames: 'dataReaded'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Socket subclass: #FTPDataSocket
	instanceVariableNames: 'downloadingShell byteArrayReceived bytesDownloaded bytesToDownload'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

NetWork guid: (GUID fromString: '{927452de-323b-45cb-bec3-a627ba4cf808}')!
NetWork comment: ''!
!NetWork categoriesForClass!Kernel-Objects! !
FTPProtocol guid: (GUID fromString: '{78903789-20f7-4b83-86da-c8e3da653445}')!
FTPProtocol comment: ''!
!FTPProtocol categoriesForClass!Kernel-Objects! !
!FTPProtocol class methodsFor!

new

	^super new initialize! !
!FTPProtocol class categoriesFor: #new!public! !

FTPReceivedMessage guid: (GUID fromString: '{a10f602d-7b3c-4b83-9f7e-d9cf62013462}')!
FTPReceivedMessage comment: 'Tranforma el mensaje recibido por el FTP de control en un Objeto de mensaje, para poder darle un respuesta.
'!
!FTPReceivedMessage categoriesForClass!Kernel-Objects! !
!FTPReceivedMessage methodsFor!

code

	^code.!

initialize
	hide := false!

message

	^message!

message: aByteArrayMessage
	message := aByteArrayMessage asString.
	code := (aByteArrayMessage copyFrom: 1 to: 3) asString asNumber!

processOn: anFtpSession

	hide ifFalse: [anFtpSession messageReceived: message]
! !
!FTPReceivedMessage categoriesFor: #code!public! !
!FTPReceivedMessage categoriesFor: #initialize!public! !
!FTPReceivedMessage categoriesFor: #message!public! !
!FTPReceivedMessage categoriesFor: #message:!public! !
!FTPReceivedMessage categoriesFor: #processOn:!public! !

!FTPReceivedMessage class methodsFor!

fromCode: aCode message: aByteArrayMessage
	| aMessageObj |
	aCode > 0 
		ifTrue: 
			[aMessageObj := self subclasses detect: [:each | each defaultCode = aCode]
						ifNone: [self].
			^aMessageObj newMessage: aByteArrayMessage].!

newMessage: aByteArrayMessage
	^super new message: aByteArrayMessage.!

onByteArray: aByteArray
	"Answer a new instance attached to the binary stream aStream."

	^self fromCode: (aByteArray copyFrom: 1 to: 3) asString asNumber message: aByteArray! !
!FTPReceivedMessage class categoriesFor: #fromCode:message:!public! !
!FTPReceivedMessage class categoriesFor: #newMessage:!private! !
!FTPReceivedMessage class categoriesFor: #onByteArray:!public! !

FTPSendingMessage guid: (GUID fromString: '{56deb692-a8a9-4437-b94b-a953b353449b}')!
FTPSendingMessage comment: 'Estos son todos los mensajes que se pueden mandar al FTP.'!
!FTPSendingMessage categoriesForClass!Kernel-Objects! !
!FTPSendingMessage methodsFor!

checkDataConnectionOn: aFTPSession
	aFTPSession isDataSocketConnect 
		ifFalse: 
			[aFTPSession sendPassiveModeRequest].
"			aFTPSession waitSocketSemaphore]"!

command: aStringCommand

	command := aStringCommand.!

evaluateParametersOn: aFtpSession
	command subStrings size - 1 = self numberOfParameters 
		ifFalse: 
			[FTPPrintMessage fromMessage: 'Cantidad de parámetros incorrecta' , Character cr asString 
						, Character lf asString
				on: aFtpSession.
			^false].
	^true!

processOn: aFtpSession
	"	aFtpSession sendRawCommand: command."

	FTPPrintMessage 
		fromMessage: 'Comando: ',  command , Character cr asString , Character lf asString
		on: aFtpSession.
! !
!FTPSendingMessage categoriesFor: #checkDataConnectionOn:!public! !
!FTPSendingMessage categoriesFor: #command:!public! !
!FTPSendingMessage categoriesFor: #evaluateParametersOn:!public! !
!FTPSendingMessage categoriesFor: #processOn:!public! !

!FTPSendingMessage class methodsFor!

fromStringCommand: aStringCommand
	| aMessageObj aCommand|
	aCommand := (aStringCommand subStrings at: 1) asLowercase.
	aCommand size > 1 
		ifTrue: 
			[aMessageObj := self subclasses detect: [:each | each defaultName = aCommand]
						ifNone: [FTPCommandIncorrect].
			^aMessageObj new command: aStringCommand].
!

helpMessage

	^''.
! !
!FTPSendingMessage class categoriesFor: #fromStringCommand:!public! !
!FTPSendingMessage class categoriesFor: #helpMessage!public! !

FTPSession guid: (GUID fromString: '{76e34882-a4f0-45d5-b7c0-f90773652c24}')!
FTPSession comment: ''!
!FTPSession categoriesForClass!Kernel-Objects! !
!FTPSession methodsFor!

cancelDownloading

	self sendRawCommand: 'ABOR'.
	self resetDataSocket.
!

close
	controlSocket isNil ifFalse: [controlSocket close].
	dataSocket isNil ifFalse: [dataSocket close]!

connectToPort: anIntegerPort address: aStringAddress userName: aUserName password: aPassword
	| anInternetAddress |
	ftpAddress := InternetAddress fromString: aStringAddress.
	userName := aUserName.
	password := aPassword.
	controlSocket := FTPControlSocket port: anIntegerPort address: ftpAddress.
	controlSocket 
		when: #dataRead
		send: #dataFromControlSocket
		to: self!

controlSocketData

	^controlSocket dataReaded.!

dataFromControlSocket
	"este mensaje se dispara al llegar información del control socket"

	self controlSocketData processOn: self!

dataSocketData

	^dataSocket dataReaded.!

enteringPassiveMode: aFTPStringMessage

	dataSocket := FTPDataSocket 
				port: (self getPortNumberFromPassiveResponse: aFTPStringMessage)
				address: ftpAddress.
	dataSocket
		when: #listReceived
			send: #listFromDataSocket
			to: self;
		when: #fileReceived
			send: #saveFile
			to: self!

eraseLoginInfo

	password := nil.
	userName := nil

	!

FTPFileName

	^FTPFileName !

getFTPFile: aStringFTPFileName localFile: aStringLocalFileName
	" obtener archivo, para obtener el archivo, primero pregunta el tamaño, una vez recibido el tamaño, se baja
	el archivo "

	state := #Receiving.
	localFileName := aStringLocalFileName.
	FTPFileName := aStringFTPFileName.
	self sendRawCommand: 'SIZE ' , FTPFileName!

getFTPFileOfSize: aSize

	dataSocket bytesToDownload: aSize.
	self sendRawCommand: 'RETR ', FTPFileName!

getPortNumberFromPassiveResponse: aString
	" cuando se recibe una respuesta al comando PASV del FTP, antre parentesis y separado con comas, se encuentra el puerto de datos "

	| aStringCollection |
	aStringCollection := aString subStrings: ','.
	^(aStringCollection at: 5) asNumber * 256 
		+ (((aStringCollection at: 6) subStrings: ')') at: 1) asNumber!

isDataSocketConnect
	dataSocket ifNil: [^false].
	^dataSocket isOpen!

lastMessage 

	^lastMessage !

listFromDataSocket
	"llega un List desde el socket de datos"

	(dataSocket byteArrayReceived asString subStrings: Character lf asString) 
		do: [:each | self messageReceived: each , Character cr asString , Character lf asString]!

messageReceived: message

	lastMessage := message!

onSocketDisconnected

	MessageBox notify: 'Conexión cerrada por el Host'.!

putLocalFile: aStringLocalFileName fTPFile: aStringFTPFileName

	state := #Sending.
	localFileName := aStringLocalFileName.
	FTPFileName := aStringFTPFileName.
	self sendRawCommand: 'stor ' , FTPFileName!

resetDataSocket
	dataSocket close.
	dataSocket := nil.!

saveFile
	| aFile |
	(File exists: localFileName) ifTrue: [File delete: localFileName].
	aFile := File 
				open: localFileName
				mode: #create
				check: true
				share: #exclusive.
	aFile write: dataSocket byteArrayReceived.
	aFile close.
	localFileName := localFileName := state := nil.

!

sendCommand: aStringCommand

	| aCommand |
	aCommand := FTPSendingMessage fromStringCommand: aStringCommand.
	^aCommand processOn: self

!

sendFile
	| aFile aByteArray |
	aFile := File 
				open: localFileName
				mode: #read
				check: false
				share: #read.
	aByteArray := ByteArray new: aFile size.
	aFile read: aByteArray.
	dataSocket sendByteArray: aByteArray.
	state := nil.
!

sendPassiveModeRequest

	" mando el comando para entrar en modo pasivo "
	self sendRawCommand: 'PASV'.!

sendPassword

	password ifNil: [password := Prompter prompt: 'Password:' caption: 'FTP'].
	password ifNil: [^self close].
	self sendRawCommand: 'pass ' , password!

sendRawCommand: anStringCommand
	controlSocket sendCommand: anStringCommand.
" en principio al mandar un comando, esperaba el semaforo hasta recibir respuesta, pero si el server no me reponde, la aplicación esperaria infinito, estuve probando a usar un TimeOut en el Semaphore pero me da error de primitiva"

"	self waitSocketSemaphore."

!

sendUserName
	userName ifNil: [userName := Prompter prompt: 'Usuario:' caption: 'FTP'].
	userName ifNil: [^self close].
	self sendRawCommand: 'user ' , userName!

state

	^state! !
!FTPSession categoriesFor: #cancelDownloading!public! !
!FTPSession categoriesFor: #close!public! !
!FTPSession categoriesFor: #connectToPort:address:userName:password:!private! !
!FTPSession categoriesFor: #controlSocketData!public! !
!FTPSession categoriesFor: #dataFromControlSocket!public! !
!FTPSession categoriesFor: #dataSocketData!public! !
!FTPSession categoriesFor: #enteringPassiveMode:!private! !
!FTPSession categoriesFor: #eraseLoginInfo!public! !
!FTPSession categoriesFor: #FTPFileName!public! !
!FTPSession categoriesFor: #getFTPFile:localFile:!public! !
!FTPSession categoriesFor: #getFTPFileOfSize:!public! !
!FTPSession categoriesFor: #getPortNumberFromPassiveResponse:!private! !
!FTPSession categoriesFor: #isDataSocketConnect!public! !
!FTPSession categoriesFor: #lastMessage!public! !
!FTPSession categoriesFor: #listFromDataSocket!public! !
!FTPSession categoriesFor: #messageReceived:!private! !
!FTPSession categoriesFor: #onSocketDisconnected!private! !
!FTPSession categoriesFor: #putLocalFile:fTPFile:!public! !
!FTPSession categoriesFor: #resetDataSocket!public! !
!FTPSession categoriesFor: #saveFile!private! !
!FTPSession categoriesFor: #sendCommand:!public! !
!FTPSession categoriesFor: #sendFile!public! !
!FTPSession categoriesFor: #sendPassiveModeRequest!private! !
!FTPSession categoriesFor: #sendPassword!public! !
!FTPSession categoriesFor: #sendRawCommand:!public! !
!FTPSession categoriesFor: #sendUserName!public! !
!FTPSession categoriesFor: #state!public! !

!FTPSession class methodsFor!

address: aStringAddress
	
	^self address: aStringAddress userName: nil password: nil
!

address: aStringAddress userName: aStringUserName password: aStringPassword
	
	^self port: self defaultPort address: aStringAddress userName: aStringUserName password: aStringPassword

!

defaultPort

	^21
!

port: anIntegerPort address: aStringAddress userName: aStringUserName password: aStringPassword
	
	^super new connectToPort: anIntegerPort address: aStringAddress userName: aStringUserName password: aStringPassword
! !
!FTPSession class categoriesFor: #address:!public! !
!FTPSession class categoriesFor: #address:userName:password:!public! !
!FTPSession class categoriesFor: #defaultPort!public! !
!FTPSession class categoriesFor: #port:address:userName:password:!public! !

FTPCantOpenDataConnection guid: (GUID fromString: '{3ea50c5e-05b5-4be3-a26d-cd0a565ac1d7}')!
FTPCantOpenDataConnection comment: ''!
!FTPCantOpenDataConnection categoriesForClass!Kernel-Objects! !
!FTPCantOpenDataConnection methodsFor!

processOn: anFtpSession

	anFtpSession resetDataSocket.! !
!FTPCantOpenDataConnection categoriesFor: #processOn:!public! !

!FTPCantOpenDataConnection class methodsFor!

defaultCode

	^425
! !
!FTPCantOpenDataConnection class categoriesFor: #defaultCode!public! !

FTPClosingConection guid: (GUID fromString: '{d5dd296a-8b34-4b4c-b435-e8b68b1125e3}')!
FTPClosingConection comment: ''!
!FTPClosingConection categoriesForClass!Kernel-Objects! !
!FTPClosingConection class methodsFor!

defaultCode

	^226
! !
!FTPClosingConection class categoriesFor: #defaultCode!public! !

FTPConectionClosed guid: (GUID fromString: '{8091b2ff-2367-430b-b1bb-40ae739c0046}')!
FTPConectionClosed comment: ''!
!FTPConectionClosed categoriesForClass!Kernel-Objects! !
!FTPConectionClosed class methodsFor!

defaultCode

	^426
! !
!FTPConectionClosed class categoriesFor: #defaultCode!public! !

FTPEnteringPassiveMode guid: (GUID fromString: '{a2e3b929-1c07-4d92-bf39-30cf6dba649e}')!
FTPEnteringPassiveMode comment: ''!
!FTPEnteringPassiveMode categoriesForClass!Kernel-Objects! !
!FTPEnteringPassiveMode methodsFor!

initialize

	hide := true.!

processOn: aFtpSession

	aFtpSession enteringPassiveMode: message.! !
!FTPEnteringPassiveMode categoriesFor: #initialize!public! !
!FTPEnteringPassiveMode categoriesFor: #processOn:!public! !

!FTPEnteringPassiveMode class methodsFor!

defaultCode

	^227
! !
!FTPEnteringPassiveMode class categoriesFor: #defaultCode!public! !

FTPFileSize guid: (GUID fromString: '{485fe292-5518-4763-89a2-dc57a1b1514c}')!
FTPFileSize comment: ''!
!FTPFileSize categoriesForClass!Kernel-Objects! !
!FTPFileSize methodsFor!

processOn: anFtpSession
	anFtpSession state = #Receiving 
		ifTrue: [anFtpSession getFTPFileOfSize: (message last: message size - 3) trimBlanks asNumber]
		ifFalse: [super processOn: anFtpSession]! !
!FTPFileSize categoriesFor: #processOn:!public! !

!FTPFileSize class methodsFor!

defaultCode

	^213

! !
!FTPFileSize class categoriesFor: #defaultCode!public! !

FTPLoginIncorrect guid: (GUID fromString: '{82acb9b0-cb74-4bd9-9b9f-8ad4a17e7310}')!
FTPLoginIncorrect comment: ''!
!FTPLoginIncorrect categoriesForClass!Kernel-Objects! !
!FTPLoginIncorrect methodsFor!

processOn: anFtpSession
	super processOn: anFtpSession.
	anFtpSession
		eraseLoginInfo;
		sendUserName! !
!FTPLoginIncorrect categoriesFor: #processOn:!public! !

!FTPLoginIncorrect class methodsFor!

defaultCode

	^530! !
!FTPLoginIncorrect class categoriesFor: #defaultCode!public! !

FTPOpeningDataConnection guid: (GUID fromString: '{6dc0e530-1a26-463b-bcbf-3bcf019117ca}')!
FTPOpeningDataConnection comment: ''!
!FTPOpeningDataConnection categoriesForClass!Kernel-Objects! !
!FTPOpeningDataConnection methodsFor!

processOn: anFtpSession


	anFtpSession state = #Sending ifTrue: [anFtpSession sendFile]! !
!FTPOpeningDataConnection categoriesFor: #processOn:!public! !

!FTPOpeningDataConnection class methodsFor!

defaultCode

	^150! !
!FTPOpeningDataConnection class categoriesFor: #defaultCode!public! !

FTPPasswordRequired guid: (GUID fromString: '{d958b26a-ade7-45ed-be0a-f611a9bc6d45}')!
FTPPasswordRequired comment: ''!
!FTPPasswordRequired categoriesForClass!Kernel-Objects! !
!FTPPasswordRequired methodsFor!

processOn: anFtpSession
	super processOn: anFtpSession.
	anFtpSession sendPassword.! !
!FTPPasswordRequired categoriesFor: #processOn:!public! !

!FTPPasswordRequired class methodsFor!

defaultCode

	^331! !
!FTPPasswordRequired class categoriesFor: #defaultCode!public! !

FTPPrintMessage guid: (GUID fromString: '{01d7b194-6f16-4c6a-8642-5e4ce08d3d59}')!
FTPPrintMessage comment: ''!
!FTPPrintMessage categoriesForClass!Kernel-Objects! !
!FTPPrintMessage methodsFor!

message: aByteArrayMessage
	message := aByteArrayMessage asString! !
!FTPPrintMessage categoriesFor: #message:!public! !

!FTPPrintMessage class methodsFor!

defaultCode

	^0
!

fromMessage: aStringErrorMessage on: aFtpSession

	^(super new message: aStringErrorMessage) processOn: aFtpSession! !
!FTPPrintMessage class categoriesFor: #defaultCode!public! !
!FTPPrintMessage class categoriesFor: #fromMessage:on:!public! !

FTPQuitting guid: (GUID fromString: '{2589bbe1-9325-4b7c-b6ac-5acd3ed92546}')!
FTPQuitting comment: ''!
!FTPQuitting categoriesForClass!Kernel-Objects! !
!FTPQuitting methodsFor!

processOn: anFtpSession
	super processOn: anFtpSession.
	anFtpSession close
! !
!FTPQuitting categoriesFor: #processOn:!public! !

!FTPQuitting class methodsFor!

defaultCode

	^221

! !
!FTPQuitting class categoriesFor: #defaultCode!public! !

FTPServerReady guid: (GUID fromString: '{0ac724a4-e1d7-4a63-a901-f54f93c528f1}')!
FTPServerReady comment: ''!
!FTPServerReady categoriesForClass!Kernel-Objects! !
!FTPServerReady methodsFor!

processOn: anFtpSession
	super processOn: anFtpSession.
	anFtpSession sendUserName! !
!FTPServerReady categoriesFor: #processOn:!public! !

!FTPServerReady class methodsFor!

defaultCode

	^220! !
!FTPServerReady class categoriesFor: #defaultCode!public! !

FTPTypeSetTo guid: (GUID fromString: '{7271c678-2e87-47a7-9760-acdc2553db95}')!
FTPTypeSetTo comment: ''!
!FTPTypeSetTo categoriesForClass!Kernel-Objects! !
!FTPTypeSetTo methodsFor!

processOn: anFtpSession

" no hace nada"! !
!FTPTypeSetTo categoriesFor: #processOn:!public! !

!FTPTypeSetTo class methodsFor!

defaultCode

	^200! !
!FTPTypeSetTo class categoriesFor: #defaultCode!public! !

FTPUserLoggedIn guid: (GUID fromString: '{dc778cbe-6ac5-49fc-8949-d5d8c8dbfaf1}')!
FTPUserLoggedIn comment: ''!
!FTPUserLoggedIn categoriesForClass!Kernel-Objects! !
!FTPUserLoggedIn methodsFor!

processOn: anFtpSession

	super processOn: anFtpSession.
	anFtpSession sendRawCommand: 'Type I'
! !
!FTPUserLoggedIn categoriesFor: #processOn:!public! !

!FTPUserLoggedIn class methodsFor!

defaultCode

	^230! !
!FTPUserLoggedIn class categoriesFor: #defaultCode!public! !

FTPCdup guid: (GUID fromString: '{c740d070-8c1b-4d4c-bad3-78bc2ca0682b}')!
FTPCdup comment: ''!
!FTPCdup categoriesForClass!Kernel-Objects! !
!FTPCdup methodsFor!

numberOfParameters

	^0!

processOn: aFtpSession
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) ifTrue: [aFtpSession sendRawCommand: 'cdup']! !
!FTPCdup categoriesFor: #numberOfParameters!public! !
!FTPCdup categoriesFor: #processOn:!public! !

!FTPCdup class methodsFor!

defaultName

	^'cd..'!

helpMessage

	^'CD.. (Sube un directorio)' , Character cr asString, Character lf asString.! !
!FTPCdup class categoriesFor: #defaultName!public! !
!FTPCdup class categoriesFor: #helpMessage!public! !

FTPCommandIncorrect guid: (GUID fromString: '{71c80f9e-7a41-4935-8deb-48cf2e41a1f3}')!
FTPCommandIncorrect comment: ''!
!FTPCommandIncorrect categoriesForClass!Kernel-Objects! !
!FTPCommandIncorrect methodsFor!

processOn: aFtpSession

	super processOn: aFtpSession.
	FTPPrintMessage 
		fromMessage: '- Comando Incorrecto' , Character cr asString , Character lf asString
		on: aFtpSession.
	^false! !
!FTPCommandIncorrect categoriesFor: #processOn:!public! !

!FTPCommandIncorrect class methodsFor!

defaultName

	^'commandIncorrect'! !
!FTPCommandIncorrect class categoriesFor: #defaultName!public! !

FTPCwd guid: (GUID fromString: '{75b3bf1c-d680-45ea-8ba9-92086bde2711}')!
FTPCwd comment: ''!
!FTPCwd categoriesForClass!Kernel-Objects! !
!FTPCwd methodsFor!

numberOfParameters

	^1!

processOn: aFtpSession
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) 
		ifTrue: [aFtpSession sendRawCommand: 'cwd ' , (command subStrings at: 2)]! !
!FTPCwd categoriesFor: #numberOfParameters!public! !
!FTPCwd categoriesFor: #processOn:!public! !

!FTPCwd class methodsFor!

defaultName

	^'cd'!

helpMessage

	^'CD <Nombre de Directorio>  (Cambiar el directorio actual)'  , Character cr asString, Character lf asString.
! !
!FTPCwd class categoriesFor: #defaultName!public! !
!FTPCwd class categoriesFor: #helpMessage!public! !

FTPDel guid: (GUID fromString: '{fd267311-ffca-4554-85bc-5cfafb146ae6}')!
FTPDel comment: ''!
!FTPDel categoriesForClass!Kernel-Objects! !
!FTPDel methodsFor!

numberOfParameters

	^1!

processOn: aFtpSession
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) 
		ifTrue: [aFtpSession sendRawCommand: 'dele ' , (command subStrings at: 2)]! !
!FTPDel categoriesFor: #numberOfParameters!public! !
!FTPDel categoriesFor: #processOn:!public! !

!FTPDel class methodsFor!

defaultName

	^'del'!

helpMessage

	^'DEL <Nombre de Archivo>  (Elimina el Archivo)'  , Character cr asString, Character lf asString.
! !
!FTPDel class categoriesFor: #defaultName!public! !
!FTPDel class categoriesFor: #helpMessage!public! !

FTPDir guid: (GUID fromString: '{01eaef75-b71a-4063-8da7-b7e72fda2c51}')!
FTPDir comment: ''!
!FTPDir categoriesForClass!Kernel-Objects! !
!FTPDir methodsFor!

processOn: aFtpSession
	| aCommand |
	super processOn: aFtpSession.
	self checkDataConnectionOn: aFtpSession.
	aCommand := command size > 3 
				ifTrue: ['list ' , (command rightString: command size - 4)]
				ifFalse: ['list'].
	aFtpSession sendRawCommand: aCommand! !
!FTPDir categoriesFor: #processOn:!public! !

!FTPDir class methodsFor!

defaultName

	^'dir'!

helpMessage

	^'DIR (Lista información sobre Archivos o Directorios)'  , Character cr asString, Character lf asString.
! !
!FTPDir class categoriesFor: #defaultName!public! !
!FTPDir class categoriesFor: #helpMessage!public! !

FTPGet guid: (GUID fromString: '{6d42c07c-172a-408b-881c-acbeb5762dd2}')!
FTPGet comment: ''!
!FTPGet categoriesForClass!Kernel-Objects! !
!FTPGet methodsFor!

numberOfParameters

	^2!

processOn: aFtpSession
	| aCommand |
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) 
		ifTrue: 
			[self checkDataConnectionOn: aFtpSession.
			aCommand := command subStrings.
			aFtpSession getFTPFile: (aCommand at: 2) localFile: (aCommand at: 3)]! !
!FTPGet categoriesFor: #numberOfParameters!public! !
!FTPGet categoriesFor: #processOn:!public! !

!FTPGet class methodsFor!

defaultName

	^'get'!

helpMessage

	^'GET <Archivo Remoto> <Archivo Local> (Obtiene un Archivo)'  , Character cr asString, Character lf asString.
! !
!FTPGet class categoriesFor: #defaultName!public! !
!FTPGet class categoriesFor: #helpMessage!public! !

FTPHelp guid: (GUID fromString: '{0abc5c51-b959-47ef-9d7d-b120497289de}')!
FTPHelp comment: ''!
!FTPHelp categoriesForClass!Kernel-Objects! !
!FTPHelp methodsFor!

processOn: aFtpSession
	| aStringMessage |
	super processOn: aFtpSession.
	aStringMessage := String new.
	self class superclass subclasses do: [:each | aStringMessage := aStringMessage , each helpMessage].
	FTPPrintMessage fromMessage: aStringMessage on: aFtpSession.
! !
!FTPHelp categoriesFor: #processOn:!public! !

!FTPHelp class methodsFor!

defaultName

	^'help'!

helpMessage

	^'HELP (Ayuda)'  , Character cr asString, Character lf asString.
! !
!FTPHelp class categoriesFor: #defaultName!public! !
!FTPHelp class categoriesFor: #helpMessage!public! !

FTPMkd guid: (GUID fromString: '{db8dae08-1010-4913-8ea9-897eb50c723e}')!
FTPMkd comment: ''!
!FTPMkd categoriesForClass!Kernel-Objects! !
!FTPMkd methodsFor!

numberOfParameters

	^1!

processOn: aFtpSession
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) ifTrue: [aFtpSession sendRawCommand: command]! !
!FTPMkd categoriesFor: #numberOfParameters!public! !
!FTPMkd categoriesFor: #processOn:!public! !

!FTPMkd class methodsFor!

defaultName

	^'mkd'!

helpMessage

	^'MKD <Nombre de Directorio> (Crea un Directorio Remoto)'  , Character cr asString, Character lf asString.
! !
!FTPMkd class categoriesFor: #defaultName!public! !
!FTPMkd class categoriesFor: #helpMessage!public! !

FTPNlst guid: (GUID fromString: '{ef88bb4a-59f3-4537-bb42-f01ee2b69593}')!
FTPNlst comment: ''!
!FTPNlst categoriesForClass!Kernel-Objects! !
!FTPNlst methodsFor!

processOn: aFtpSession
	super processOn: aFtpSession.
	self checkDataConnectionOn: aFtpSession.
	aFtpSession sendRawCommand: command! !
!FTPNlst categoriesFor: #processOn:!public! !

!FTPNlst class methodsFor!

defaultName

	^'nlst'!

helpMessage

	^'NLST (Lista los nombres de Archivos o Directorios)'  , Character cr asString, Character lf asString.
! !
!FTPNlst class categoriesFor: #defaultName!public! !
!FTPNlst class categoriesFor: #helpMessage!public! !

FTPPut guid: (GUID fromString: '{98af2089-7bdf-42f4-9c41-b33d22ddecaf}')!
FTPPut comment: ''!
!FTPPut categoriesForClass!Kernel-Objects! !
!FTPPut methodsFor!

numberOfParameters

	^2!

processOn: aFtpSession
	| aCommand |
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) 
		ifTrue: 
			[self checkDataConnectionOn: aFtpSession.
			aCommand := command subStrings.
			aFtpSession putLocalFile: (aCommand at: 2) fTPFile: (aCommand at: 3)]! !
!FTPPut categoriesFor: #numberOfParameters!public! !
!FTPPut categoriesFor: #processOn:!public! !

!FTPPut class methodsFor!

defaultName

	^'put'!

helpMessage

	^'PUT <Archivo Local> <Archivo Remoto> (Envia un Archivo al sitio remoto)'  , Character cr asString, Character lf asString.
! !
!FTPPut class categoriesFor: #defaultName!public! !
!FTPPut class categoriesFor: #helpMessage!public! !

FTPQuit guid: (GUID fromString: '{a175f34b-ebc4-44f9-9237-c2b3f463a225}')!
FTPQuit comment: ''!
!FTPQuit categoriesForClass!Kernel-Objects! !
!FTPQuit methodsFor!

processOn: aFtpSession
	super processOn: aFtpSession.
	aFtpSession sendRawCommand: 'quit'! !
!FTPQuit categoriesFor: #processOn:!public! !

!FTPQuit class methodsFor!

defaultName

	^'quit'!

helpMessage

	^'QUIT (Salir)'  , Character cr asString, Character lf asString.
! !
!FTPQuit class categoriesFor: #defaultName!public! !
!FTPQuit class categoriesFor: #helpMessage!public! !

FTPRawCommand guid: (GUID fromString: '{add64164-7bf1-47e8-ba9f-5ef2f4148d21}')!
FTPRawCommand comment: 'Este objeto es para mandar comandos al socket del FTP sin procesarlos. USAR CON PRECAUCION'!
!FTPRawCommand categoriesForClass!Kernel-Objects! !
!FTPRawCommand methodsFor!

processOn: aFtpSession
	super processOn: aFtpSession.
	aFtpSession sendRawCommand:  (command rightString: command size - 11).! !
!FTPRawCommand categoriesFor: #processOn:!public! !

!FTPRawCommand class methodsFor!

defaultName

	^'rawcommand'!

helpMessage

	^'RAWCOMMAND <aRawCommand>  (Enviar un comando FTP al puerto del FTP)'  , Character cr asString, Character lf asString.
! !
!FTPRawCommand class categoriesFor: #defaultName!public! !
!FTPRawCommand class categoriesFor: #helpMessage!public! !

FTPRename guid: (GUID fromString: '{775b0bae-289f-439b-9726-4c7e6613c9ad}')!
FTPRename comment: ''!
!FTPRename categoriesForClass!Kernel-Objects! !
!FTPRename methodsFor!

numberOfParameters

	^2!

processOn: aFtpSession
	| aCommand |
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) 
		ifTrue: 
			[aCommand := command subStrings.
			aFtpSession sendRawCommand: 'rnfr ' , (aCommand at: 2).
			aFtpSession sendRawCommand: 'rnto ' , (aCommand at: 3)]! !
!FTPRename categoriesFor: #numberOfParameters!public! !
!FTPRename categoriesFor: #processOn:!public! !

!FTPRename class methodsFor!

defaultName

	^'rename'!

helpMessage

	^'RENAME <Nombre Actual> <Nuevo Nombre>  (Renombra un Archivo o un Directorio)'  , Character cr asString, Character lf asString.
! !
!FTPRename class categoriesFor: #defaultName!public! !
!FTPRename class categoriesFor: #helpMessage!public! !

FTPRmd guid: (GUID fromString: '{909fc91d-8a75-48a8-86b6-af222efebad6}')!
FTPRmd comment: ''!
!FTPRmd categoriesForClass!Kernel-Objects! !
!FTPRmd methodsFor!

numberOfParameters

	^1!

processOn: aFtpSession
	super processOn: aFtpSession.
	(super evaluateParametersOn: aFtpSession) ifTrue: [
	aFtpSession sendRawCommand: command].! !
!FTPRmd categoriesFor: #numberOfParameters!public! !
!FTPRmd categoriesFor: #processOn:!public! !

!FTPRmd class methodsFor!

defaultName

	^'rmd'!

helpMessage

	^'RMD <Nombre de Directorio> (Elimina un Directorio Remoto)'  , Character cr asString, Character lf asString.
! !
!FTPRmd class categoriesFor: #defaultName!public! !
!FTPRmd class categoriesFor: #helpMessage!public! !

FTPSessionBrowser guid: (GUID fromString: '{67dcb51b-3757-454f-99f2-a8e85c93c95a}')!
FTPSessionBrowser comment: 'Este objeto es para el usuario final, manda los mensajes y recibe las respuesta del FTPClient.'!
!FTPSessionBrowser categoriesForClass!Kernel-Objects! !
!FTPSessionBrowser methodsFor!

close
	super close.
	fTPShell close.!

fTPMessages
	^fTPMessages!

fTPMessages: anObject
	fTPMessages := anObject.
	self trigger: #valueChanged !

getFTPFileOfSize: aSize
	" reescribo este método para iniciar el Tracing "

	fTPDownloadingShell := FTPDownloadingShell showOn: dataSocket.
	fTPDownloadingShell 
		when: #cancelDownloading
		send: #cancelDownloading
		to: self.
	dataSocket 
		when: #bytesReceived
		send: #refresh
		to: fTPDownloadingShell.
	^super getFTPFileOfSize: aSize!

initialize
	fTPShell := FTPSessionBrowserShell showOn: self.
	fTPMessages := 'Iniciando sesión de FTP...' , Character cr asString 
				, Character lf asString!

messageReceived: message
	
	super messageReceived: message.
	self fTPMessages: fTPMessages , message.
!

saveFile

	super saveFile.
	fTPDownloadingShell exit.! !
!FTPSessionBrowser categoriesFor: #close!public! !
!FTPSessionBrowser categoriesFor: #fTPMessages!accessing!private! !
!FTPSessionBrowser categoriesFor: #fTPMessages:!accessing!private! !
!FTPSessionBrowser categoriesFor: #getFTPFileOfSize:!public! !
!FTPSessionBrowser categoriesFor: #initialize!public! !
!FTPSessionBrowser categoriesFor: #messageReceived:!private! !
!FTPSessionBrowser categoriesFor: #saveFile!public! !

!FTPSessionBrowser class methodsFor!

show
	| aStringFTPAddress |
	aStringFTPAddress := Prompter prompt: 'Dirección FTP:' caption: 'FTP'.
	aStringFTPAddress notNil ifTrue: [^self address: aStringFTPAddress].! !
!FTPSessionBrowser class categoriesFor: #show!public! !

FTPProtocolShell guid: (GUID fromString: '{bf473903-27a5-4849-b8f7-2159c6edd3ad}')!
FTPProtocolShell comment: ''!
!FTPProtocolShell categoriesForClass!MVP-Presenters! !
FTPDownloadingShell guid: (GUID fromString: '{4e705e1e-2b89-43ff-9764-185780f2c7ea}')!
FTPDownloadingShell comment: ''!
!FTPDownloadingShell categoriesForClass!MVP-Presenters! !
!FTPDownloadingShell methodsFor!

cancelDownloading

	self trigger: #cancelDownloading.
	self exit.!

createComponents

	super createComponents.
	sizeDownloadingPresenter := self add: NumberPresenter new name: 'sizeDownloading'.
	sizeToDownloadPresenter := self add: NumberPresenter new name: 'sizeToDownload'.!

model: aFTPDataSocket
	super model: aFTPDataSocket.
	sizeToDownloadPresenter model: (aFTPDataSocket aspectValue: #bytesToDownload).
	sizeDownloadingPresenter model: (aFTPDataSocket aspectValue: #bytesDownloaded)!

onViewOpened
	self view extent: 245 @ 145.
	^super onViewOpened!

refresh
	sizeDownloadingPresenter view refreshContents.
	sizeToDownloadPresenter view refreshContents.
	self view refreshContents.


! !
!FTPDownloadingShell categoriesFor: #cancelDownloading!public! !
!FTPDownloadingShell categoriesFor: #createComponents!public! !
!FTPDownloadingShell categoriesFor: #model:!public! !
!FTPDownloadingShell categoriesFor: #onViewOpened!public! !
!FTPDownloadingShell categoriesFor: #refresh!public! !

!FTPDownloadingShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 196934 1 ##(Smalltalk.RGB) 27633061 nil 551 nil nil nil 416 nil 170 192 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140924416 1 528 nil 466 27633061 nil 7 nil nil nil 528 nil 8 4294903483 787206 ##(Smalltalk.NumberToText) nil 8 '' nil 3 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 211 21 786 241 51 528 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 10 0 0 0 225 0 0 0 35 0 0 0] 8 #() 786 193 193 nil 27 8 'sizeDownloading' 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140924416 1 928 nil 466 27633061 nil 7 nil nil nil 928 nil 8 4294903483 626 nil 8 '' nil 3 674 138 144 34 1 738 #createAt:extent: 34 2 786 211 101 786 241 51 928 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 50 0 0 0 225 0 0 0 75 0 0 0] 8 #() 896 nil 27 8 'sizeToDownload' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 674 138 144 34 3 738 #createAt:extent: 34 2 786 3839 21 786 751 321 416 738 #text: 34 1 8 'Donwloading...' 416 738 #updateMenuBar 8 #() 416 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 246 8 0 0 170 0 0 0] 34 5 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 1472 nil 466 27633061 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[237 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 786 193 193 nil 1472 nil 8 1954986209 852486 ##(Smalltalk.NullConverter) nil nil nil 674 138 144 34 2 738 #createAt:extent: 34 2 786 21 21 786 181 51 1472 738 #text: 34 1 8 'Downloading:' 1472 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 100 0 0 0 35 0 0 0] 8 #() 896 nil 27 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 1904 nil 466 27633061 nil 7 nil 1554 nil true 1586 8 #[237 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1632 nil 1904 nil 8 1954986209 1666 nil nil nil 674 138 144 34 2 738 #createAt:extent: 34 2 786 21 101 786 51 51 1904 738 #text: 34 1 8 'Of:' 1904 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 50 0 0 0 35 0 0 0 75 0 0 0] 8 #() 896 nil 27 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2272 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 7 nil nil nil 2272 nil 8 4294903577 1180998 4 ##(Smalltalk.CommandDescription) #cancelDownloading 8 'Cancel' 1 1 nil nil false nil nil nil 674 138 144 34 3 738 #createAt:extent: 34 2 786 291 171 786 171 61 2272 738 #isEnabled: 8 #(false) 2272 738 #text: 34 1 8 'Cancel' 2272 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 85 0 0 0 230 0 0 0 115 0 0 0] 8 #() 896 nil 29 528 928 896 nil 27 )! !
!FTPDownloadingShell class categoriesFor: #resource_Default_view!public!resources-views! !

FTPSessionBrowserShell guid: (GUID fromString: '{ad9835ae-3696-43e9-af76-2d3cdda61895}')!
FTPSessionBrowserShell comment: ''!
!FTPSessionBrowserShell categoriesForClass!MVP-Presenters! !
!FTPSessionBrowserShell methodsFor!

acceptNewCommand
	fTPCommandPresenter value notNil
		ifTrue: 
			[self model sendCommand: fTPCommandPresenter value.
			fTPCommandPresenter value: nil]!

changeFont
	fTPMessagesPresenter view font: Font choose!

close
	^self exit!

createComponents
	super createComponents.
	fTPCommandPresenter := self add: TextPresenter new name: 'fTPCommand'.
	fTPCommandPresenter
		when: #keyPressed:
		send: #keyPressed:
		to: self.
	fTPMessagesPresenter := self add: TextPresenter new name: 'fTPMessages'!

exit
	^self model close!

keyPressed: aKeyEvent
	aKeyEvent code = 13 ifTrue: [self acceptNewCommand]!

model: aFTPClientBrowser
	"| anAspect |"
	super model: aFTPClientBrowser.
	fTPMessagesPresenter model: (aFTPClientBrowser aspectValue: #fTPMessages).
	aFTPClientBrowser
		when: #valueChanged
		send: #refreshView
		to: self!

onViewOpened
	super onViewOpened.
	self view extent: 725 @ 390!

refreshView
	fTPMessagesPresenter view refreshContents scrollToEnd! !
!FTPSessionBrowserShell categoriesFor: #acceptNewCommand!public! !
!FTPSessionBrowserShell categoriesFor: #changeFont!public! !
!FTPSessionBrowserShell categoriesFor: #close!public! !
!FTPSessionBrowserShell categoriesFor: #createComponents!public! !
!FTPSessionBrowserShell categoriesFor: #exit!public! !
!FTPSessionBrowserShell categoriesFor: #keyPressed:!public! !
!FTPSessionBrowserShell categoriesFor: #model:!public! !
!FTPSessionBrowserShell categoriesFor: #onViewOpened!public! !
!FTPSessionBrowserShell categoriesFor: #refreshView!public! !

!FTPSessionBrowserShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 551 nil nil nil 416 nil 170 192 34 4 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 416 34 2 8 1143017796 1025 544 nil 466 8 4278190080 nil 7 nil nil nil 544 nil 8 4294903483 852486 ##(Smalltalk.NullConverter) nil nil 9 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 15 11 802 1001 331 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 5 0 0 0 251 1 0 0 170 0 0 0] 8 #() 802 193 193 nil 27 8 'fTPCommand' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 416 34 2 8 1143017796 1025 944 nil 466 624 nil 7 nil nil nil 944 nil 8 4294903483 658 nil nil 9 690 138 144 34 1 754 #createAt:extent: 34 2 802 15 351 802 1001 281 944 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 175 0 0 0 251 1 0 0 59 1 0 0] 8 #() 912 nil 27 8 'fTPMessages' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 3839 21 802 1051 831 416 754 #updateMenuBar 8 #() 416 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 140 9 0 0 169 1 0 0] 34 3 544 944 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 1424 nil nil nil 7 nil nil nil 1424 nil 8 4294903577 1180998 4 ##(Smalltalk.CommandDescription) #changeFont 8 'Change font' 1 1 nil nil false nil nil nil 690 138 144 34 3 754 #createAt:extent: 34 2 802 21 641 802 181 51 1424 754 #isEnabled: 8 #(false) 1424 754 #text: 34 1 8 'Change font' 1424 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 64 1 0 0 100 0 0 0 89 1 0 0] 8 #() 912 nil 29 912 nil 27 )! !
!FTPSessionBrowserShell class categoriesFor: #resource_Default_view!public!resources-views! !

FTPSessionManager guid: (GUID fromString: '{389b042a-6249-409a-9a62-ebfa08d26c59}')!
FTPSessionManager comment: ''!
!FTPSessionManager categoriesForClass!System-Support! !
!FTPSessionManager methodsFor!

main
	"Start up the sample application"

	self mainShellClass show! !
!FTPSessionManager categoriesFor: #main!operations-startup!public! !

!FTPSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^FTPSessionBrowser! !
!FTPSessionManager class categoriesFor: #mainShellClass!constants!public! !

FTPControlSocket guid: (GUID fromString: '{9c71f86d-bad6-416d-8ff5-2176645e208c}')!
FTPControlSocket comment: 'Este objeto es el que hace de Socket de control del FTP, todos los mensajes son enviados desde este objeto y todas las respuestas son recibidas por él.'!
!FTPControlSocket categoriesForClass!Kernel-Objects! !
!FTPControlSocket methodsFor!

dataReaded

	^dataReaded

	!

onAsyncRead
	readSemaphore signal.
	dataReaded := FTPReceivedMessage onByteArray: (SocketReadStream on: self) readPage collection.
	self trigger: #dataRead.
!

port: anIntegerPort address: anInternetAddress
	" conecto el socket de control "
	^(super port: anIntegerPort address: anInternetAddress) connect.
!

readSemaphore

	^readSemaphore.!

receive

	^ self receiveByteArrayPartial: 4096
!

sendCommand: anStringCommand

	self basicSendByteArray: anStringCommand  , Character cr asString, Character lf asString
! !
!FTPControlSocket categoriesFor: #dataReaded!public! !
!FTPControlSocket categoriesFor: #onAsyncRead!private! !
!FTPControlSocket categoriesFor: #port:address:!public! !
!FTPControlSocket categoriesFor: #readSemaphore!public! !
!FTPControlSocket categoriesFor: #receive!operations!public! !
!FTPControlSocket categoriesFor: #sendCommand:!private! !

FTPDataSocket guid: (GUID fromString: '{4eda04e9-33cc-4a3e-aaf2-ab697b55a32e}')!
FTPDataSocket comment: '-Por este objeto llegan y salen todos los paquetes de información desde el FTP. 
Puden ser tanto paquetes pertenecientes a archivos siendo bajados o subidos como también información generada por un comando LIST.
- Al llegar un paquete, si la cantidad de bytes para bajar (bytesToDownload) está en 0, el objeto interpreta que el paquete es
de un comando LIST. por lo que recibe el paquete y dispara el trigger #valueChanged.
- Si al llegar un paquete la cantidad de bytes para bajar es > 0, interpreta que esta bajando un archivo. Por lo que ejecuta el mensaje receiveByteArrayPartial: del socket, este mensaje va a leer el puerto hasta recibir la cantidad de bytes que se le indicó. Luego de bajados todos los bytes se dispara el trigger
'!
!FTPDataSocket categoriesForClass!Kernel-Objects! !
!FTPDataSocket methodsFor!

appendToArrayReceived: readingByteArray
	| aFromInteger |
	aFromInteger := bytesDownloaded.
	self bytesDownloaded: aFromInteger + readingByteArray size.
	byteArrayReceived 
		replaceFrom: aFromInteger + 1
		to: bytesDownloaded
		with: readingByteArray
		startingAt: 1.
!

byteArrayReceived

	^byteArrayReceived .!

bytesDownloaded

	^bytesDownloaded!

bytesDownloaded: aBytesCount

	bytesDownloaded := aBytesCount.
	self trigger: #bytesReceived.!

bytesToDownload
	^bytesToDownload!

bytesToDownload: anObject

	bytesToDownload := anObject!

dataReceive
	"recibe bytesToDownload  cantidad de bytes desde el socket"

	"		[bytesDownloaded < bytesToDownload]
		whileTrue: [self appendToArrayReceived: (self receiveByteArrayPartial: bytesToDownload )]."

	self appendToArrayReceived: (self receiveByteArrayPartial: bytesToDownload)!

evaluateReceive
	bytesToDownload = 0 
		ifTrue: [self listReceived]
		ifFalse: 
			[byteArrayReceived ifNil: [byteArrayReceived := ByteArray new: bytesToDownload].
			self dataReceive]!

initialize

	bytesToDownload := 0.
	bytesDownloaded := 0.
	^super initialize.

!

listReceived
	"Private - llega un list por el puerto de datos"

	byteArrayReceived := self receiveByteArrayPartial: self class defaultByteReaded.
	self trigger: #listReceived!

onAsyncClose
	" En las pruebas con un FTP en una LAN, a veces llegaba el mensaje de CLOSE antes de procesar los últimos paquetes, por eso, si al desconectarse todavía quedan datos por llegar, tiro un mensaje de lectura"
	bytesToDownload > 0 
		ifTrue: 
			[bytesDownloaded < bytesToDownload 
				ifTrue: [self dataReceive].
			self trigger: #fileReceived].
	super onAsyncClose.
	self close!

onAsyncRead

	readSemaphore signal.
	self evaluateReceive.

!

port: anIntegerPort address: anInternetAddress

	^(super port: anIntegerPort address: anInternetAddress) connect.
!

sendByteArray: aByteArray

	super sendByteArray: aByteArray.
	self close.
! !
!FTPDataSocket categoriesFor: #appendToArrayReceived:!operations!public! !
!FTPDataSocket categoriesFor: #byteArrayReceived!operations!public! !
!FTPDataSocket categoriesFor: #bytesDownloaded!operations!public! !
!FTPDataSocket categoriesFor: #bytesDownloaded:!operations!public! !
!FTPDataSocket categoriesFor: #bytesToDownload!accessing!public! !
!FTPDataSocket categoriesFor: #bytesToDownload:!accessing!public! !
!FTPDataSocket categoriesFor: #dataReceive!operations!public! !
!FTPDataSocket categoriesFor: #evaluateReceive!event handling!private! !
!FTPDataSocket categoriesFor: #initialize!public! !
!FTPDataSocket categoriesFor: #listReceived!operations!private! !
!FTPDataSocket categoriesFor: #onAsyncClose!private! !
!FTPDataSocket categoriesFor: #onAsyncRead!event handling!private! !
!FTPDataSocket categoriesFor: #port:address:!public! !
!FTPDataSocket categoriesFor: #sendByteArray:!public! !

!FTPDataSocket class methodsFor!

defaultByteReaded

	^4096.! !
!FTPDataSocket class categoriesFor: #defaultByteReaded!public! !

"Binary Globals"!

