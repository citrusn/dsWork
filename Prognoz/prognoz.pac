| package |
package := Package name: 'prognoz'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #StDatabase;
	add: #StImageDatabase;
	add: #StListComponent;
	add: #StLoggedInComponent;
	add: #StLoginComponent;
	add: #StMenuComponent;
	add: #StMessageComponent;
	add: #StRegisterComponent;
	add: #StRootTask;
	add: #StSession;
	add: #StTask;
	add: #StTaskEditor;
	add: #StToDoLibrary;
	add: #StUser;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Contributions\rko\Seaside-D7.0\Grease\Grease-Core';
	add: '..\..\Contributions\rko\Seaside-D7.0\Seaside\Seaside-Component';
	add: '..\..\Contributions\rko\Seaside-D7.0\Seaside\Seaside-Core';
	add: '..\..\Contributions\rko\Seaside-D7.0\Seaside\Seaside-Session';
	yourself).

package!

"Class Definitions"!

Object subclass: #StDatabase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StTask
	instanceVariableNames: 'completed deadline taskDescription id taskName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StUser
	instanceVariableNames: 'id userName email tasks password'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAFileLibrary subclass: #StToDoLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StListComponent
	instanceVariableNames: 'items filterBlock sortBlock renderItemBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StLoggedInComponent
	instanceVariableNames: 'menuComponent listComponent taskEditor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StLoginComponent
	instanceVariableNames: 'email password messageComponent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StMenuComponent
	instanceVariableNames: 'entries'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StMessageComponent
	instanceVariableNames: 'messageType messageString wasShown'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StRegisterComponent
	instanceVariableNames: 'user repeatedPassword messageComponent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WAComponent subclass: #StTaskEditor
	instanceVariableNames: 'task'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WATask subclass: #StRootTask
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WASession subclass: #StSession
	instanceVariableNames: 'user database'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StDatabase subclass: #StImageDatabase
	instanceVariableNames: ''
	classVariableNames: 'Users'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StDatabase guid: (GUID fromString: '{9a91f9e9-3feb-4fb0-ad30-c530a29b6ca4}')!
StDatabase comment: ''!
!StDatabase categoriesForClass!Kernel-Objects! !
!StDatabase methodsFor!

connect
^ true!

connectionFailed
^ false!

disconnect
^ true! !
!StDatabase categoriesFor: #connect!public! !
!StDatabase categoriesFor: #connectionFailed!public! !
!StDatabase categoriesFor: #disconnect!public! !

StTask guid: (GUID fromString: '{78455b4d-6cd1-49d7-8a85-205262fd8c93}')!
StTask comment: ''!
!StTask categoriesForClass!Kernel-Objects! !
!StTask methodsFor!

completed
	^completed!

completed: anObject
	completed := anObject!

deadline
	^deadline!

deadline: anObject
	deadline := anObject!

hasBeenMissed
	^self completed not and: [self deadline < Date today]!

id
	^id!

id: anObject
	id := anObject!

initialize
	self
		deadline: Date tomorrow;
		taskName: 'имя задачи';
		completed: false!

isPending
	^self completed not and: [self deadline >= Date today]!

taskDescription
	^taskDescription!

taskDescription: anObject
	taskDescription := anObject!

taskName
	^taskName!

taskName: anObject
	taskName := anObject! !
!StTask categoriesFor: #completed!accessing!private! !
!StTask categoriesFor: #completed:!accessing!private! !
!StTask categoriesFor: #deadline!accessing!private! !
!StTask categoriesFor: #deadline:!accessing!private! !
!StTask categoriesFor: #hasBeenMissed!public! !
!StTask categoriesFor: #id!accessing!private! !
!StTask categoriesFor: #id:!accessing!private! !
!StTask categoriesFor: #initialize!public! !
!StTask categoriesFor: #isPending!public! !
!StTask categoriesFor: #taskDescription!accessing!private! !
!StTask categoriesFor: #taskDescription:!accessing!private! !
!StTask categoriesFor: #taskName!accessing!private! !
!StTask categoriesFor: #taskName:!accessing!private! !

!StTask class methodsFor!

new
	"Answer a new initialize instance of the receiver."

	^super new initialize! !
!StTask class categoriesFor: #new!public! !

StUser guid: (GUID fromString: '{afa592d8-a34f-4c61-9b69-33e6c62736df}')!
StUser comment: ''!
!StUser categoriesForClass!Kernel-Objects! !
!StUser methodsFor!

addTask: aTask
	^self tasks add: aTask!

email
	^email!

email: anObject
	email := anObject!

id
	^id!

id: anObject
	id := anObject!

initialize
	super initialize.
	self tasks: OrderedCollection new!

password
	^password!

password: anObject
	password := anObject!

tasks
	^tasks!

tasks: anObject
	tasks := anObject!

userName
	^userName!

userName: anObject
	userName := anObject! !
!StUser categoriesFor: #addTask:!public! !
!StUser categoriesFor: #email!accessing!public! !
!StUser categoriesFor: #email:!accessing!public! !
!StUser categoriesFor: #id!accessing!public! !
!StUser categoriesFor: #id:!accessing!public! !
!StUser categoriesFor: #initialize!public! !
!StUser categoriesFor: #password!accessing!public! !
!StUser categoriesFor: #password:!accessing!public! !
!StUser categoriesFor: #tasks!accessing!public! !
!StUser categoriesFor: #tasks:!accessing!public! !
!StUser categoriesFor: #userName!accessing!public! !
!StUser categoriesFor: #userName:!accessing!public! !

!StUser class methodsFor!

hashPassword: aString
	^aString isEmpty ifTrue: [0] ifFalse: [GRPlatform current secureHashFor: aString]!

new
	^super new
		initialize;
		yourself! !
!StUser class categoriesFor: #hashPassword:!public! !
!StUser class categoriesFor: #new!public! !

StToDoLibrary guid: (GUID fromString: '{b9e8c43c-45d9-4ca1-9e6d-dfa74cd7d34c}')!
StToDoLibrary comment: ''!
!StToDoLibrary categoriesForClass!Seaside-Core-Libraries! !
!StToDoLibrary methodsFor!

selectorsToInclude
	^#(#todoCss)!

todoCss
	^'
body {
	font: normal 100% Arial, Helvetica, Verdana, sans-serif;
}

.generic {
	font-family: Geneva, Arial, Helvetica, sans-serif;
	font-style: normal;
	font-weight: normal;
	font-size: 14px;
	color : #663300;
	padding: 20px;
	margin-top: 20px;
	margin-left: 20px;
	border: 1px solid #663300;
	width: 500px;
}

.lightbox {
	background-color:#FFFFFF;
}

.errorMessage {
	background-color: #ffb0b0;
	border: 1px solid #ee0000;
	padding: 5px;
	margin-bottom: 10px;
}

.infoMessage {
	background-color: #fbffcc;
	border: 1px solid #eda33a;
	padding: 5px;
	margin-bottom: 10px;
}

.copyright {
	font-family: Geneva, Arial, Helvetica, sans-serif;
	font-size: 10px;
	font-style: normal;
	font-weight: normal;
	color: #663300;
	text-align: left;
	margin-left: 20px;
}

a {
	color : #663300;
	text-decoration: underline;
}'! !
!StToDoLibrary categoriesFor: #selectorsToInclude!public! !
!StToDoLibrary categoriesFor: #todoCss!public! !

!StToDoLibrary class methodsFor!

applicationName
	^'ToDo Application'! !
!StToDoLibrary class categoriesFor: #applicationName!public! !

StListComponent guid: (GUID fromString: '{e03aeba8-011b-422b-8aa4-762764a26957}')!
StListComponent comment: ''!
!StListComponent categoriesForClass!Seaside-Component! !
!StListComponent methodsFor!

filterBlock
	^filterBlock!

filterBlock: anObject
	filterBlock := anObject!

items
	^items!

items: anObject
	items := anObject!

renderContentOn: html
	html table: 
			[(self sortBlock value: (self filterBlock value: self items))
				do: [:item | html tableRow: [self renderItemBlock value: item value: html]]]!

renderItemBlock
	^renderItemBlock!

renderItemBlock: anObject
	renderItemBlock := anObject!

sortBlock
	^sortBlock!

sortBlock: anObject
	sortBlock := anObject! !
!StListComponent categoriesFor: #filterBlock!accessing!public! !
!StListComponent categoriesFor: #filterBlock:!accessing!public! !
!StListComponent categoriesFor: #items!accessing!public! !
!StListComponent categoriesFor: #items:!accessing!public! !
!StListComponent categoriesFor: #renderContentOn:!public! !
!StListComponent categoriesFor: #renderItemBlock!accessing!public! !
!StListComponent categoriesFor: #renderItemBlock:!accessing!public! !
!StListComponent categoriesFor: #sortBlock!accessing!public! !
!StListComponent categoriesFor: #sortBlock:!accessing!public! !

StLoggedInComponent guid: (GUID fromString: '{d860d0a5-7abd-4a91-8513-9d502ff7dec6}')!
StLoggedInComponent comment: ''!
!StLoggedInComponent categoriesForClass!Seaside-Component! !
!StLoggedInComponent methodsFor!

children
	^Array with: self menuComponent with: self listComponent!

createNewTask
	(self call: self taskEditor newTask) ifFalse: [^self].
	self session database addTask: self taskEditor task toUser: self session user!

editTask: aTask
	self taskEditor task: aTask copy.
	(self call: self taskEditor) ifFalse: [^self].
	aTask copyFrom: self taskEditor task.
	self session database updateTask: aTask!

initialize
	super initialize.
	self initializeMenuComponent.
	self initializeListComponent.
	self taskEditor: StTaskEditor new.!

initializeListComponent
	self listComponent: StListComponent new.
	"self listComponent items: self testTasks."
	self listComponent
		sortBlock: [:items | items sort: [:a :b | a deadline < b deadline]];
		renderItemBlock: [:task :html | self renderTask: task asRowOn: html];
		items: self session user tasks.
	self showPendingTasks!

initializeMenuComponent
	self menuComponent: (StMenuComponent new
				addEntry: 'All' withAction: [self showAllTasks];
				addEntry: 'Completed' withAction: [self showCompletedTasks];
				addEntry: 'Pending' withAction: [self showPendingTasks];
				addEntry: 'Missed' withAction: [self showMissedTasks]; 
				addEntry: 'New Task' withAction: [self createNewTask];
				addEntry: 'Logout' withAction: [self session logout. self answer: true]; 
				yourself)!

listComponent
	^listComponent!

listComponent: anObject
	listComponent := anObject!

menuComponent
	^menuComponent!

menuComponent: anObject
	menuComponent := anObject!

renderContentOn: html
	"I render everything by calling html"

	html div
		class: 'generic';
		with: 
				[html heading: 'ToDo-List of ' , self session user userName.
				html div
					class: 'menu';
					with: self menuComponent.
				html div
					class: 'list';
					id: 'list';
					with: self listComponent]!

renderTask: aTask asRowOn: html
	html
		tableData: aTask deadline displayString;
		tableData: aTask taskName;
		tableData: aTask taskDescription;
		tableData: aTask completed displayString;
		tableData: [html anchor
					callback: [self editTask: aTask];
					with: 'edit']!

showAllTasks
	self listComponent filterBlock: [:items | items]!

showCompletedTasks
	self listComponent filterBlock: [:items | items select: [:item | item completed]]!

showMissedTasks
	self listComponent filterBlock: [:items | items select: [:item | item hasBeenMissed]]!

showPendingTasks
	self listComponent filterBlock: [:items | items select: [:item | item isPending]]!

taskEditor
	^taskEditor!

taskEditor: anObject
	taskEditor := anObject!

testTasks
	^OrderedCollection
		with: (StTask new
				deadline: Date yesterday;
				completed: false;
				taskName: 'Missed task')
		with: (StTask new
				deadline: Date tomorrow;
				completed: false;
				taskName: 'Pending task')
		with: (StTask new
				deadline: Date tomorrow;
				completed: true;
				taskName: 'Already completed task')! !
!StLoggedInComponent categoriesFor: #children!public! !
!StLoggedInComponent categoriesFor: #createNewTask!public! !
!StLoggedInComponent categoriesFor: #editTask:!public! !
!StLoggedInComponent categoriesFor: #initialize!public! !
!StLoggedInComponent categoriesFor: #initializeListComponent!public! !
!StLoggedInComponent categoriesFor: #initializeMenuComponent!initializing!public! !
!StLoggedInComponent categoriesFor: #listComponent!accessing!private! !
!StLoggedInComponent categoriesFor: #listComponent:!accessing!private! !
!StLoggedInComponent categoriesFor: #menuComponent!accessing!private! !
!StLoggedInComponent categoriesFor: #menuComponent:!accessing!private! !
!StLoggedInComponent categoriesFor: #renderContentOn:!public! !
!StLoggedInComponent categoriesFor: #renderTask:asRowOn:!public! !
!StLoggedInComponent categoriesFor: #showAllTasks!public! !
!StLoggedInComponent categoriesFor: #showCompletedTasks!public! !
!StLoggedInComponent categoriesFor: #showMissedTasks!public! !
!StLoggedInComponent categoriesFor: #showPendingTasks!public! !
!StLoggedInComponent categoriesFor: #taskEditor!accessing!private! !
!StLoggedInComponent categoriesFor: #taskEditor:!accessing!private! !
!StLoggedInComponent categoriesFor: #testTasks!public! !

!StLoggedInComponent class methodsFor!

canBeRoot
^ true! !
!StLoggedInComponent class categoriesFor: #canBeRoot!public! !

StLoginComponent guid: (GUID fromString: '{d37970d2-8002-473d-ba12-9aa6b3d06a8e}')!
StLoginComponent comment: ''!
!StLoginComponent categoriesForClass!Seaside-Component! !
!StLoginComponent methodsFor!

applicationName
	^StToDoLibrary applicationName!

email
	^email!

email: anObject
	email := anObject!

initialize
	super initialize.
	self messageComponent: StMessageComponent new.
	!

loginFailed
	self messageComponent errorMessage: 'Login failed.'!

messageComponent
	^messageComponent!

messageComponent: anObject
	messageComponent := anObject!

password
	^password!

password: anObject
	password := anObject!

registerUser
	self answer: #registerUser!

renderContentOn: html
	html div
		class: 'generic';
		with: 
				[html
					heading: self applicationName;
					render: self messageComponent;
					text: 'Please login with e-mail and password:';
					form: 
							[self
								renderTextInputOn: html;
								renderPasswordInputOn: html;
								renderLoginButtonOn: html;
								renderSignUpAnchorOn: html]]!

renderLoginButtonOn: html
	html submitButton
		callback: [self validateLogin];
		text: 'Login'!

renderPasswordInputOn: html
	html passwordInput
		callback: [:value | self password: (StUser hashPassword: value)];
		value: ''!

renderSignUpAnchorOn: html
	html paragraph with: 
			[html anchor
				callback: [self registerUser];
				with: [html text: 'Sign up for the ' , self applicationName]]!

renderTextInputOn: html
	html textInput
		on: #email of: self;
		value: ''.
	html space!

validateLogin
	| user |
	user := self session findUserByEmail: self email.
	(user notNil and: [user password = self password])
		ifTrue: [self answer: user]
		ifFalse: [self loginFailed]! !
!StLoginComponent categoriesFor: #applicationName!public! !
!StLoginComponent categoriesFor: #email!accessing!public! !
!StLoginComponent categoriesFor: #email:!accessing!public! !
!StLoginComponent categoriesFor: #initialize!public! !
!StLoginComponent categoriesFor: #loginFailed!public! !
!StLoginComponent categoriesFor: #messageComponent!accessing!public! !
!StLoginComponent categoriesFor: #messageComponent:!accessing!public! !
!StLoginComponent categoriesFor: #password!accessing!public! !
!StLoginComponent categoriesFor: #password:!accessing!public! !
!StLoginComponent categoriesFor: #registerUser!public! !
!StLoginComponent categoriesFor: #renderContentOn:!public! !
!StLoginComponent categoriesFor: #renderLoginButtonOn:!public! !
!StLoginComponent categoriesFor: #renderPasswordInputOn:!public! !
!StLoginComponent categoriesFor: #renderSignUpAnchorOn:!public! !
!StLoginComponent categoriesFor: #renderTextInputOn:!public! !
!StLoginComponent categoriesFor: #validateLogin!public! !

!StLoginComponent class methodsFor!

new
	^super new initialize! !
!StLoginComponent class categoriesFor: #new!public! !

StMenuComponent guid: (GUID fromString: '{eafb50c4-a7ca-4485-91bb-b89ead98c1ee}')!
StMenuComponent comment: ''!
!StMenuComponent categoriesForClass!Seaside-Component! !
!StMenuComponent methodsFor!

addEntry: aString withAction: aBlock
	^self entries add: aString -> aBlock!

entries
	^entries!

entries: anObject
	entries := anObject!

initialize
	super initialize.
	entries := OrderedCollection new!

renderContentOn: html
	self entries do: 
			[:entry |
			html anchor
				callback: entry value;
				with: entry key]
		separatedBy: [html space]! !
!StMenuComponent categoriesFor: #addEntry:withAction:!public! !
!StMenuComponent categoriesFor: #entries!accessing!private! !
!StMenuComponent categoriesFor: #entries:!accessing!private! !
!StMenuComponent categoriesFor: #initialize!public! !
!StMenuComponent categoriesFor: #renderContentOn:!public! !

StMessageComponent guid: (GUID fromString: '{2d42d46b-40df-425c-ac06-47154537278f}')!
StMessageComponent comment: ''!
!StMessageComponent categoriesForClass!Seaside-Component! !
!StMessageComponent methodsFor!

errorMessage: aString
	self
		messageString: aString;
		messageType: 'error';
		wasShown: false!

infoMessage: aString
	self
		messageString: aString;
		messageType: 'info';
		wasShown: false!

initialize
	super initialize.
	self wasShown: true!

messageString
	^messageString!

messageString: anObject
	messageString := anObject!

messageType
	^messageType!

messageType: anObject
	messageType := anObject!

renderContentOn: html
	self wasShown ifTrue: [^self].
	html div
		class: self messageType , 'Message';
		with: self messageString.
	self wasShown: true!

wasShown
	^wasShown!

wasShown: anObject
	wasShown := anObject! !
!StMessageComponent categoriesFor: #errorMessage:!public! !
!StMessageComponent categoriesFor: #infoMessage:!public! !
!StMessageComponent categoriesFor: #initialize!public! !
!StMessageComponent categoriesFor: #messageString!accessing!public! !
!StMessageComponent categoriesFor: #messageString:!accessing!public! !
!StMessageComponent categoriesFor: #messageType!accessing!public! !
!StMessageComponent categoriesFor: #messageType:!accessing!public! !
!StMessageComponent categoriesFor: #renderContentOn:!public! !
!StMessageComponent categoriesFor: #wasShown!accessing!public! !
!StMessageComponent categoriesFor: #wasShown:!accessing!public! !

!StMessageComponent class methodsFor!

new
	^super new
		initialize;
		yourself! !
!StMessageComponent class categoriesFor: #new!public! !

StRegisterComponent guid: (GUID fromString: '{4b3420b5-83a8-40e9-8636-1dcdc370f07d}')!
StRegisterComponent comment: ''!
!StRegisterComponent categoriesForClass!Seaside-Component! !
!StRegisterComponent methodsFor!

initialize
	super initialize.
	self messageComponent: StMessageComponent new.
	self user: StUser new!

messageComponent
	^messageComponent!

messageComponent: anObject 
		messageComponent := anObject!

registerUser	
	self user userName isEmptyOrNil
		ifTrue: [^self messageComponent infoMessage: 'Please choose a username!!'].

	self user email isEmptyOrNil
		ifTrue: [^self messageComponent infoMessage: 'Please enter your e-mail address!!'].

	(self session findUserByEmail: self user email)
		ifNotNil: [^self messageComponent errorMessage: 'The e-mail address is already registered!!'].
	self user password = 0 ifTrue: [^self messageComponent infoMessage: 'Please choose a password!!'].
	self user password = self repeatedPassword
		ifFalse: [^self messageComponent infoMessage: 'Your repeated password does not match!!'].
	
	self session database addUser: self user.
	self answer: self user!

renderCancelButtonOn: html
	html submitButton
		callback: [self answer: nil];
		text: 'Cancel'!

renderContentOn: html
	html div
		class: 'generic';
		with: 
				[html
					heading: 'Register';
					render: self messageComponent;
					form: 
							[html table: 
									[self
										renderUsernameTextInputOn: html;
										renderEmailTextInputOn: html;
										renderPasswordTextInputOn: html;
										renderRepeatedPasswordTextInputOn: html.
									html tableRow: 
											[html
												tableData;
												tableData: 
														[self renderSubmitButtonOn: html.
														html space.
														self renderCancelButtonOn: html]]]]]!

renderEmailTextInputOn: html
	html tableRow: 
			[html
				tableData: 'E-mail';
				tableData: 
						[html textInput
							callback: [:value | self user email: value];
							value: self user email]]!

renderPasswordTextInputOn: html
	html tableRow: 
			[html
				tableData: 'Password';
				tableData: [html passwordInput callback: [:value | self user password: (StUser hashPassword: value)]]]!

renderRepeatedPasswordTextInputOn: html
	html tableRow: 
			[html
				tableData: 'Repeat password';
				tableData: 
						[html passwordInput callback: [:value | self repeatedPassword: (StUser hashPassword: value)]]]!

renderSubmitButtonOn: html
	html submitButton
		callback: [self registerUser];
		text: 'Register'!

renderUsernameTextInputOn: html
	html tableRow: 
			[html
				tableData: 'Username';
				tableData: 
						[html textInput
							callback: [:value | self user userName: value];
							value: self user userName]]!

repeatedPassword
	^repeatedPassword!

repeatedPassword: anObject
	repeatedPassword := anObject!

user
	^user!

user: anObject
	user := anObject! !
!StRegisterComponent categoriesFor: #initialize!public! !
!StRegisterComponent categoriesFor: #messageComponent!accessing!public! !
!StRegisterComponent categoriesFor: #messageComponent:!accessing!public! !
!StRegisterComponent categoriesFor: #registerUser!public! !
!StRegisterComponent categoriesFor: #renderCancelButtonOn:!public! !
!StRegisterComponent categoriesFor: #renderContentOn:!public! !
!StRegisterComponent categoriesFor: #renderEmailTextInputOn:!public! !
!StRegisterComponent categoriesFor: #renderPasswordTextInputOn:!public! !
!StRegisterComponent categoriesFor: #renderRepeatedPasswordTextInputOn:!public! !
!StRegisterComponent categoriesFor: #renderSubmitButtonOn:!public! !
!StRegisterComponent categoriesFor: #renderUsernameTextInputOn:!public! !
!StRegisterComponent categoriesFor: #repeatedPassword!accessing!private! !
!StRegisterComponent categoriesFor: #repeatedPassword:!accessing!private! !
!StRegisterComponent categoriesFor: #user!accessing!public! !
!StRegisterComponent categoriesFor: #user:!accessing!public! !

StTaskEditor guid: (GUID fromString: '{72cbfcec-3e1d-4f1c-b571-8c1982af6bd3}')!
StTaskEditor comment: ''!
!StTaskEditor categoriesForClass!Seaside-Component! !
!StTaskEditor methodsFor!

newTask
	self task: StTask new!

renderButtonsOn: html
	html
		tableData;
		tableData: 
				[html submitButton
					callback: [self answer: true];
					value: 'Save'.
				html submitButton
					callback: [self answer: false];
					value: 'Cancel']!

renderCompletedSelectionOn: html
	html
		tableData: [html text: 'Completed: '];
		tableData: 
				[html select
					add: true;
					add: false;
					on: #completed of: self task;
					labels: [:value | value ifTrue: ['yes'] ifFalse: ['no']]]!

renderContentOn: html
	html div
		class: 'redmond';
		with: 
				[html heading: 'Editing task'.
				html form: 
						[html table: 
								[html
									tableRow: [self renderNameInputOn: html];
									tableRow: [self renderDescriptionInputOn: html];
									tableRow: [self renderDateInputOn: html];
									tableRow: [self renderCompletedSelectionOn: html];
									tableRow: [self renderButtonsOn: html]]]]!

renderDateInputOn: html
	html
		tableData: [html text: 'Deadline: '];
		tableData: [html dateInput on: #deadline of: self task]!

renderDescriptionInputOn: html
	html
		tableData: [html text: 'Description: '];
		tableData: [html textArea on: #taskDescription of: self task]!

renderNameInputOn: html
	html
		tableData: [html text: 'Name: '];
		tableData: [html textInput on: #taskName of: self task]!

task
	^task!

task: anObject
	task := anObject! !
!StTaskEditor categoriesFor: #newTask!public! !
!StTaskEditor categoriesFor: #renderButtonsOn:!public! !
!StTaskEditor categoriesFor: #renderCompletedSelectionOn:!public! !
!StTaskEditor categoriesFor: #renderContentOn:!public! !
!StTaskEditor categoriesFor: #renderDateInputOn:!public! !
!StTaskEditor categoriesFor: #renderDescriptionInputOn:!public! !
!StTaskEditor categoriesFor: #renderNameInputOn:!public! !
!StTaskEditor categoriesFor: #task!accessing!public! !
!StTaskEditor categoriesFor: #task:!accessing!public! !

StRootTask guid: (GUID fromString: '{1d2b3056-e6bd-40fc-b334-d93a98ab4a67}')!
StRootTask comment: ''!
!StRootTask categoriesForClass!Seaside-Component-Tasks! !
!StRootTask methodsFor!

go
	| loginAnswer user |
	loginAnswer := self call: StLoginComponent new.
	loginAnswer = #registerUser
		ifTrue: [user := self call: StRegisterComponent new]
		ifFalse: [user := loginAnswer].
	user
		ifNotNil: 
			[self session login: user.
			self call: StLoggedInComponent new]! !
!StRootTask categoriesFor: #go!public! !

!StRootTask class methodsFor!

canBeRoot
	^ true! !
!StRootTask class categoriesFor: #canBeRoot!public! !

StSession guid: (GUID fromString: '{65a6142e-8cd8-45dd-bb14-7b23e66504a0}')!
StSession comment: ''!
!StSession categoriesForClass!Seaside-Component! !
!StSession methodsFor!

database
	^database!

database: anObject
	database := anObject!

findUserByEmail: anEmail
	^self database findUserByEmail: anEmail!

initialize
	super initialize.
	self database: StImageDatabase new!

isLoggedIn
	^self user notNil!

login: aUser
	self user: aUser!

logout
	self user: nil!

responseForRequest: aRequest
	self database connectionFailed ifTrue: [^WAResponse new nextPutAll: 'No Database Connection'].
	^super responseForRequest: aRequest!

unregistered
	self database disconnect.
	super unregistered!

updateRoot: anHtmlRoot
	super updateRoot: anHtmlRoot.
	anHtmlRoot title: StToDoLibrary applicationName!

user
	^user!

user: anObject
	user := anObject! !
!StSession categoriesFor: #database!accessing!public! !
!StSession categoriesFor: #database:!accessing!public! !
!StSession categoriesFor: #findUserByEmail:!public! !
!StSession categoriesFor: #initialize!public! !
!StSession categoriesFor: #isLoggedIn!public! !
!StSession categoriesFor: #login:!public! !
!StSession categoriesFor: #logout!public! !
!StSession categoriesFor: #responseForRequest:!public! !
!StSession categoriesFor: #unregistered!public! !
!StSession categoriesFor: #updateRoot:!public! !
!StSession categoriesFor: #user!accessing!public! !
!StSession categoriesFor: #user:!accessing!public! !

StImageDatabase guid: (GUID fromString: '{9a9f9250-d947-4762-ad3f-cc5859edf87a}')!
StImageDatabase comment: 'self Users removeAtIndex: 1
'!
!StImageDatabase categoriesForClass!Kernel-Objects! !
!StImageDatabase methodsFor!

addTask: aTask toUser: aUser
	aUser addTask: aTask.
	self saveImage.
	^aTask!

addUser: aUser
	self class users add: aUser.
	self saveImage.
	^aUser!

findUserByEmail: anEmail
	^self class users detect: [:each | each email = anEmail] ifNone: [nil]!

saveImage
	"self class writeMutex critical: [self saveImageWithoutMonitor]."

	!

updateTask: aTask
	self saveImage! !
!StImageDatabase categoriesFor: #addTask:toUser:!public! !
!StImageDatabase categoriesFor: #addUser:!public! !
!StImageDatabase categoriesFor: #findUserByEmail:!public! !
!StImageDatabase categoriesFor: #saveImage!public! !
!StImageDatabase categoriesFor: #updateTask:!public! !

!StImageDatabase class methodsFor!

saveImageWithoutMonitor
	"SmalltalkImage current saveSession"

	!

users
	^Users ifNil: [Users := OrderedCollection new]!

Users
	^Users!

Users: anUsers
	Users := anUsers!

writeMutex
"^ WriteMutex ifNil: [WriteMutex := Monitor new]"! !
!StImageDatabase class categoriesFor: #saveImageWithoutMonitor!public! !
!StImageDatabase class categoriesFor: #users!public! !
!StImageDatabase class categoriesFor: #Users!public! !
!StImageDatabase class categoriesFor: #Users:!public! !
!StImageDatabase class categoriesFor: #writeMutex!public! !

"Binary Globals"!

