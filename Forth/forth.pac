| package |
package := Package name: 'forth'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Forth;
	add: #Stack;
	add: #WordHeader;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Contributions\ITC Gorisek\Dialect Abstraction Layer';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Forth
	instanceVariableNames: 'mem rs ds ip wp core entries'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Stack
	instanceVariableNames: 'anArray top'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WordHeader
	instanceVariableNames: 'address immediate'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Forth guid: (GUID fromString: '{69eef882-6ba8-471b-b300-0000c18fcfd6}')!
Forth comment: 'object[] Mem;     // Память программ
Stack<int> RS;    // Стек возвратов
Stack<object> DS; // Стек данных
int IP;           // Указатель инструкций
int WP;           // Указатель слова

delegate void CoreCall();
List<CoreCall> Core; // Хранилище примитивов'!
!Forth categoriesForClass!Kernel-Objects! !
!Forth methodsFor!

core
	^core!

core: anObject
	core := anObject!

doList
	rs push: ip.
	ip := wp + 1!

ds
	^ds!

ds: anObject
	ds := anObject!

exit
	ip := rs pop!

hello
	Transcript
		show: 'Hello in:' , Timestamp now displayString;
		cr!

initialize
	| opHello opExit opNext opDoList |
	rs := Stack new.
	ds := Stack new.
	mem := Array new: 1024.
	entries := LookupTable new.
	"Core = new List<CoreCall>();"
	core := OrderedCollection new.
	core add: [self next].
	core add: [self doList].
	core add: [self exit].
	core add: [self hello].
	opNext := 0+1.
	opDoList := 1+1.
	opExit := 2+1.
	opHello := 3+1.

	" core pointers "
	mem at: opNext put: opNext.
	mem at: opDoList put: opDoList.
	mem at: opExit put: opExit.
	mem at: opHello put: opHello.

	"program"
	mem at: 5 put: opDoList. 	"3) сохраняем адрес интерпретации IP = 9 на стеке возвратов, затем устанавливаем IP = WP + 1 = 5"
	mem at: 6 put: opHello. 	"4) выводим на экран сообщение"
	mem at: 7 put: opExit. 	"5) выходим из слова, восстанавливаем IP = 9 со стека возвратов"
	mem at: 8 put: opDoList. 	"1) точка входа в подпрограмму"
	mem at: 9 put: 5. 		"2) вызов подпрограммы по адресу 4, устанавливаем WP = 4"
	mem at: 10 put: opExit. 	"6) выходим из слова, восстанавливая IP = 0 со стека возвратов"
!

intParseFast: aStr
	" An optimized int parse method."

	| result |
	result := 0.
	aStr do: 
			[:each |
			(each between: $0 and: $9) ifFalse: [^result	"error"].
			result := 10 * result + (each codePoint - 48)].
	^result!

ip
	^ip!

ip: anObject
	ip := anObject!

isConstant: aWord
	^(aWord at: 1) isDigit
		or: [aWord size > 1 and: [((aWord at: 1) = $+ or: [(aWord at: 1) = $-]) and: [(aWord at: 2) isDigit]]]!

isWhite: aChar
	"\r\ n \t' contains: aChar asString"!

lookUp: aWord
    entries  at: aWord ifAbsent:  [^ nil].
    "return Entries[word].Last();"
  
  
!

mem
	^mem!

mem: anObject
	mem := anObject!

next
	[ip > 0] whileTrue: 
			[wp := mem at: ip.
			ip := ip + 1.
			(core at: (mem at: wp)) value]!

parseNumber: aStr
	| factor sign str |
	factor := 1.0.
	sign := 1.
	str := aStr.
	(aStr at: 1) = $- ifTrue: 
			[sign := -1.
			str := aStr rightString: aStr size - 1].
	(str at: 1) = $+ ifTrue: [str := aStr rightString: aStr size - 1].


	" for (var i = str.Length - 1; i >= 0; i--) {
    if (str[i] == '.') {
      str = str.Remove(i, 1);
      ^ (self intParseFast: str) * factor * sign
    }
    factor := factor * 0.1.
}"
	^(self intParseFast: str) * sign!

rs
	^rs!

rs: anObject
	rs := anObject!

start
	| entryPoint |
	entryPoint := 8.	" адрес точки входа"
	ip := 0.	"устанавливаем IP = 0, чтобы завершить выполнение программы по ее завершении"
	wp := entryPoint.	" устанавливаем WP = 7 в качестве адреса точки входа"
	self doList.	" выполняем команду начала интерпретации слова, сохраняя IP = 0 на стеке возвратов"
	self next!

wp
	^wp!

wp: anObject
	wp := anObject! !
!Forth categoriesFor: #core!accessing!private! !
!Forth categoriesFor: #core:!accessing!private! !
!Forth categoriesFor: #doList!public! !
!Forth categoriesFor: #ds!accessing!private! !
!Forth categoriesFor: #ds:!accessing!private! !
!Forth categoriesFor: #exit!public! !
!Forth categoriesFor: #hello!public! !
!Forth categoriesFor: #initialize!public! !
!Forth categoriesFor: #intParseFast:!accessing!public! !
!Forth categoriesFor: #ip!accessing!private! !
!Forth categoriesFor: #ip:!accessing!private! !
!Forth categoriesFor: #isConstant:!public! !
!Forth categoriesFor: #isWhite:!public! !
!Forth categoriesFor: #lookUp:!public! !
!Forth categoriesFor: #mem!accessing!private! !
!Forth categoriesFor: #mem:!accessing!private! !
!Forth categoriesFor: #next!public! !
!Forth categoriesFor: #parseNumber:!public! !
!Forth categoriesFor: #rs!accessing!private! !
!Forth categoriesFor: #rs:!accessing!private! !
!Forth categoriesFor: #start!public! !
!Forth categoriesFor: #wp!accessing!private! !
!Forth categoriesFor: #wp:!accessing!private! !

!Forth class methodsFor!

new
	^super new
		initialize;
		yourself! !
!Forth class categoriesFor: #new!public! !

Stack guid: (GUID fromString: '{059f3c15-97f7-40ef-b920-3a51258541dc}')!
Stack comment: ''!
!Stack categoriesForClass!Kernel-Objects! !
!Stack methodsFor!

pop
	| item |
	item := anArray at: top.
	top := top - 1.
	^item!

printOn: aStream
	aStream nextPutAll: 'Stack['.
	1 to: top
		do: 
			[:i |
			(anArray at: i) printOn: aStream.
			aStream space].
	aStream nextPutAll: ']'!

push: item
	top := top + 1.
	anArray at: top put: item!

setsize: n
   anArray := Array new: n.
    top := 0! !
!Stack categoriesFor: #pop!public! !
!Stack categoriesFor: #printOn:!public! !
!Stack categoriesFor: #push:!public! !
!Stack categoriesFor: #setsize:!public! !

!Stack class methodsFor!

new
	| s |
	s := super new.
	s setsize: 10.
	^s! !
!Stack class categoriesFor: #new!public! !

WordHeader guid: (GUID fromString: '{a397759d-77be-481b-8c92-003a5b8eb877}')!
WordHeader comment: ''!
!WordHeader categoriesForClass!Kernel-Objects! !
"Binary Globals"!

