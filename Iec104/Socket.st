"Create a ServerSocket on port 2048"
serverSocket := ServerSocket port: 2048.

"Accept a connection on this port - this call will 
block until a client connects. The socket that is
returned can then be used to talk to the client"
socketA := serverSocket accept.
Transcript show: 'Start server'; cr.
[socketA readStream hasInput] whileTrue: 
		[Transcript show: socketA readStream next.
		"socketA readStream next: 4.
		(socketA readStream next: 5) asString"]
socketA readStream hasInput.

socketA close.

"Identify the host and port of the server to connect to"
"socketB := Socket port: 2404 address: (InternetAddress fromIPString: '10.221.124.5'). "
socketB := Socket port: 2404 address: (InternetAddress fromIPString: '127.0.0.1'). 

"Connect to the server - this call will block until the connection is acknowledged"
socketB connect. 
Transcript show: 'Conected'; cr.

socketB writeStream nextPut: 0.
socketB writeStream nextPutAll: #[104 04 07 00 00 00]; flush.
socketB writeStream nextPutAll: 'hello'.
socketB writeStream flush.

socketB readStream hasInput.
[socketB readStream hasInput] whileTrue:
	[Transcript show: (socketB readStream next printStringRadix: 16); show: ' ' .]
Transcript  cr.
socketB close.

Transcript show: (10 printStringRadix: 16); cr.

