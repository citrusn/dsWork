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
	add: '..\..\Contributions\ITC Gorisek\Dialect Abstraction Layer';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Forth
	instanceVariableNames: 'mem rs ds ip wp core entries hereP input isEvalMode lastWordHeader'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Stack
	instanceVariableNames: 'anArray top'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WordHeader
	instanceVariableNames: 'address immediate isEnable'
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
List<CoreCall> Core; // Хранилище примитивов
public Dictionary<string, List<WordHeader>> Entries;'!
!Forth categoriesForClass!Kernel-Objects! !
!Forth methodsFor!

addHeader: aWord
	self
		define: aWord
		entry: self here
		immediate: false!

addOp: anObject
	mem at: self here put: anObject.
	self here: self here + 1!

and
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: (b and: [a])!

beginDefWord
	self addHeader: (self readWord: input).
	self addOp: (self lookUp: 'doList') address.
	isEvalMode := false!

bl
   self write: ' ' !

branch
           ip := ip + (mem at: ip)!

comma
           self addOp: ds pop!

comment
            self skipComment: input params: ')'!

commentLine
            self skipComment: input params: String lineDelimiter!

constant	
	self addHeader: (self readWord: input).
	self addOp: (self lookUp: 'doList' )  address.
	self addOp: (self lookUp: 'doLit' )  address.
	self addOp:  ds pop.
	self addOp: (self lookUp: 'exit' )  address!

core
	^core!

core: anObject
	core := anObject!

cr
            self writeLine: ''!

dec
	| a |
	a := ds pop.
	ds push: a - 1!

define: aWord entry: intEntry immediate: aBool
	| set |
	set := entries at: aWord ifAbsentPut: [OrderedCollection new].
	lastWordHeader := WordHeader new
				address: intEntry;
				immediate: aBool;
				isEnable: true;
				yourself.
	set add: lastWordHeader!

defineWord: aWord params: aSubWords
	self addHeader: aWord.
	self addOp: (self lookUp: 'doList') address.
	aSubWords do: 
			[:w |
			| lookup |
			lookup := self lookUp: w.
			lookup
				ifNil: [self addOp: (self lookUp: w) address]
				ifNotNil: 
					[(self isConstant: w)
						ifTrue: 
							[self addOp: (self lookUp: 'doLit') address.
							self addOp: (self parseNumber: w)]
						ifFalse: [self writeLine: 'Unknown word ' , w]]].
	self addOp: (self lookUp: 'exit') address!

divide
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b // a!

doList
	rs push: ip.
	ip := wp + 1!

doLit
	ds push: ( mem at: ip).
	ip := ip + 1!

dot
            self write: (ds pop) displayString!

dots
	1 to: ds size
		do: [:i | (ds at: i) ifNotNil: [:value | self writeLine: value displayString ] ifNil: [self writeLine: 'null']]!

drop
	ds pop!

ds
	^ds!

ds: anObject
	ds := anObject!

dump
	self dumpFrom: 1 to: self here - 1!

dumpFrom: intStart to: intEnd
	self writeLine: ''.
	self writeLine: '----- MEMORY DUMP -----'.
	 intStart to: intEnd do:  [:i |
			| name ka  entryWord|
			ka := self searchKnowAddress: (mem at: i).
			name := '???'.
			entries
				keysAndValuesDo: [:eachKey :eachValue | eachValue do: [:wh | wh address == ka ifTrue: [name := eachKey]]].
			entries
				keysAndValuesDo: [:eachKey :eachValue | eachValue do: [:wh | wh address == i ifTrue: [entryWord := eachKey]]].
		entryWord isNilOrEmpty ifFalse: [self  writeLine: ''. self writeLine: entryWord displayString ].
		self writeLine: '[ ', i displayString, ' ]  -> ' , (mem at: i) displayString, ' : "', name, '"'.
]!

dup
	| a |
	a := ds peek.
	ds push: a!

endDefWord
	self addOp: (self lookUp: 'exit') address.
	isEvalMode := true.
	lastWordHeader isEnable: true!

entries
	^entries!

entries: anObject
	entries := anObject!

eql
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: (b == a)!

evalStream: aStream
	input := aStream.
	self interpreter!

evalString: aStr
	self evalStream: (ReadStream on: aStr)!

execute
	| address |
	address := ds pop.
	address < core size " eval core "
		ifTrue: [
			(core at: address) value " invoke core function "]
		ifFalse: [" eval word "
			"IP == 4."	" core word execute "
			wp := address.	"   set eval address "
			self doList.	" fake doList "
			self next	" run evaluator"]!

execute: anAddress
	[anAddress <= core size
		ifTrue: [" eval core "
			(core at: anAddress) value	" invoke core function "]
		ifFalse: [" eval word "
			ip := 0.	" set return address "
			wp := anAddress.	" set eval address "
			self doList.	" fake doList "
			self next	" run evaluator "] ]
	on: Error do: [:ex |  Transcript  show: ex description. rs clear. ds clear. ip := 0. wp := 0 ]!

exit
	ip := rs pop!

false
	ds push: false
!

getHereAddr
            ds push: hereP
        !

getMember
	^Error notYetImplemented!

getStaticMember
	^Error notYetImplemented!

getType
	| obj class|
	obj := ds pop.
	class := Smalltalk at: obj asSymbol.
	ds push:  class!

greater
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b > a!

greaterEql
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b >= a!

hello
	Transcript
		show: 'Hello in:' , Timestamp now displayString;
		cr!

here
	^mem at: hereP!

here: anObject
	mem at: hereP put: anObject!

immediate
            lastWordHeader immediate: true!

inc
	| a |
	a := ds pop.
	ds push: a +1!

initCore
	hereP := 1.
	" Core "
	self setCoreWord: 'nop' handler: [self nop] immediate: false.
	self setCoreWord: 'next' handler: [self next] immediate: false.
        self setCoreWord: 'doList' handler: [self doList] immediate: false.    
	self setCoreWord: 'exit' handler: [self exit] immediate: false.            
	self setCoreWord: 'execute' handler: [self execute ] immediate: false.
	self setCoreWord: 'doLit' handler: [self doLit] immediate: false.            
        self setCoreWord: ':' handler: [self beginDefWord ] immediate: false.
	self setCoreWord: ';' handler: [self endDefWord ] immediate: true.
	self setCoreWord: 'branch' handler: [self branch ] immediate: false.
	self setCoreWord: '0branch' handler: [self zbranch ] immediate: false.
	self setCoreWord: 'here' handler: [self getHereAddr ] immediate: false.
        self setCoreWord: 'quit' handler: [self quit ] immediate: false.    
        self setCoreWord: 'dump' handler: [self dump ] immediate: false.    
	self setCoreWord: 'words' handler: [self words ] immediate: false.            
	self setCoreWord: '''' handler: [self tick ] immediate: false.
	self setCoreWord: ',' handler: [self comma ] immediate: false.
	self setCoreWord: '[' handler: [self lbrac ] immediate: true.
	self setCoreWord: ']' handler: [self rbrac ] immediate: false.
	self setCoreWord: 'immediate' handler: [self immediate ] immediate: true.            
	" Mem "
	self setCoreWord: '!!' handler: [self  writeMem ] immediate: false.          
	self setCoreWord: '@' handler: [self readMem ] immediate: false.            
	self setCoreWord: 'variable' handler: [self  variable ] immediate: false.            
	self setCoreWord: 'constant' handler: [self constant ] immediate: false.              
        " RW "
	self setCoreWord: '.' handler: [self dot ] immediate: false.            
	self setCoreWord: '.s' handler: [self dots ] immediate: false.            
	self setCoreWord: 'cr' handler: [self cr ] immediate: false.            
	self setCoreWord: 'bl' handler: [self  bl] immediate: false.            
	self setCoreWord: 'word' handler: [self readWord ] immediate: true.
	self setCoreWord: 's"' handler: [self readString ] immediate: true. 
	self setCoreWord: 'key' handler: [self key ] immediate: false.                        
        " Comment "
	self setCoreWord: '(' handler: [self comment ] immediate: true .            
	self setCoreWord: '\\' handler: [self  commentLine ] immediate: true .                        
        " .net mem "
	self setCoreWord: 'null' handler: [self null ] immediate: false.                       
	self setCoreWord: 'new' handler: [self newInstance ] immediate: false.                       
	self setCoreWord: 'type' handler: [self getType ] immediate: false.                       
	self setCoreWord: 'm!!' handler: [self setMember ] immediate: false.                       
	self setCoreWord: 'm@' handler: [self getMember ] immediate: false.                       
	self setCoreWord: 'ms!!' handler: [self setStaticMember ] immediate: false.                       
	self setCoreWord: 'ms@' handler: [self getStaticMember ] immediate: false.                       
        " Boolean "
	self setCoreWord: 'true' handler: [self true  ] immediate: false.            
	self setCoreWord: 'false' handler: [self  false ] immediate: false.            
	self setCoreWord: 'and' handler: [self and ] immediate: false.            
	self setCoreWord: 'or' handler: [self or ] immediate: false.            
	self setCoreWord: 'xor' handler: [self xor ] immediate: false.            
	self setCoreWord: 'not' handler: [self not ] immediate: false.            
	self setCoreWord: 'invert' handler: [self invert ] immediate: false.                        
	self setCoreWord: '=' handler: [self eql ] immediate: false.            
	self setCoreWord: '<>' handler: [self notEql ] immediate: false.            
	self setCoreWord: '<' handler: [self  less ] immediate: false.            
	self setCoreWord: '>' handler: [self greater ] immediate: false.            
	self setCoreWord: '<=' handler: [self lessEql ] immediate: false.            
	self setCoreWord: '>=' handler: [self  greaterEql ] immediate: false.            
        " Math "
	self setCoreWord: '-' handler: [self minus ] immediate: false.
	self setCoreWord: '+' handler: [self plus ] immediate: false.
	self setCoreWord: '*' handler: [self multiply ] immediate: false.
	self setCoreWord: '/' handler: [self divide] immediate: false.            
	self setCoreWord: 'mod' handler: [self mod ] immediate: false.
	self setCoreWord: '1+' handler: [self inc] immediate: false.
	self setCoreWord: '1-' handler: [self dec] immediate: false.	
	" Stack "
	self setCoreWord: 'drop' handler: [self drop ] immediate: false.
	self setCoreWord: 'swap' handler: [self swap ] immediate: false.
	self setCoreWord: 'dup' handler: [self dup ] immediate: false.
	self setCoreWord: 'over' handler: [self over] immediate: false.
	self setCoreWord: 'rot' handler: [self rot ] immediate: false.
	self setCoreWord: 'nrot' handler: [self nrot ] immediate: false.
         
	self here: hereP + 1
	!

initExtra
            " Print variable "
            self evalString: ': ? @ . ;'.

            " Allocate n bytes "
            self evalString: ': allot here @ + here !! ;'.

            " control flow "
            self evalString: ': if immediate doLit [ ''  0branch , ] , here @ 0 , ;'.
            self evalString: ': then immediate dup here @ swap - swap !! ;'.
            self evalString: ': else immediate doLit [ '' branch , ] , here @ 0 , swap dup here @ swap - swap !! ;'.

            " loops "
            self evalString: ': begin immediate here @ ;'.
            self evalString: ': until immediate doLit [ '' 0branch , ] , here @ - , ;'.
            self evalString: ': again immediate doLit [ '' branch , ] , here @ - , ;'.
            self evalString: ': while immediate doLit [ '' 0branch , ] , here @ 0 , ;'.
            self evalString: ': repeat immediate doLit [ '' branch , ] , swap here @ - , dup here @ swap - swap !! ;'.
	" C like comment "
            self evalString: ': // immediate [ '' \\ , ] ;' .
	"test from book"
	self evalString: ': nip swap drop ;'!

initialize
	"| opHello opExit opNext opDoList |"
	rs := Stack new.
	ds := Stack new.
	mem := Array new: 1024.
	entries := LookupTable new.
	"Core = new List<CoreCall>();"
	core := OrderedCollection new.
	isEvalMode := true.
	self initCore.
	self initExtra.

	"core add: [self next].
	core add: [self doList].
	core add: [self exit].
	core add: [self hello].
	opNext := 0+1.
	opDoList := 1+1.
	opExit := 2+1.
	opHello := 3+1."

	" core pointers "
	"mem at: opNext put: opNext.
	mem at: opDoList put: opDoList.
	mem at: opExit put: opExit.
	mem at: opHello put: opHello."

	"program"
	"mem at: 5 put: opDoList. ""3) сохраняем адрес интерпретации IP = 9 на стеке возвратов, затем устанавливаем IP = WP + 1 = 5"
	"mem at: 6 put: opHello. "	"4) выводим на экран сообщение"
	"mem at: 7 put: opExit. "	"5) выходим из слова, восстанавливаем IP = 9 со стека возвратов"
	"mem at: 8 put: opDoList." "1) точка входа в подпрограмму"
	"mem at: 9 put: 5. 	"	"2) вызов подпрограммы по адресу 4, устанавливаем WP = 4"
	"mem at: 10 put: opExit. "	"6) выходим из слова, восстанавливая IP = 0 со стека возвратов"
!

interpreter
 |word lookup |
[true] whileTrue: [
	word := self readWord: input.
	word isNilOrEmpty ifTrue: [^self ].
	lookup := self lookUp: word.
	isEvalMode
	ifTrue: [ "режим исполнения"
		lookup 
			ifNotNil: [:value | self execute: lookup address] 
			ifNil: [ (self isConstant: word ) 
					ifTrue: [ ds push:  (self parseNumber: word)]
					ifFalse: [ ds clear.  self writeLine: 'The word: ' , word, ' is undefined' ]]] 
	ifFalse: [ " program mode "
		lookup 
			ifNotNil: [:value |  value immediate
							ifTrue: [self  execute: value address] 
							ifFalse: [self addOp: value address]]
			ifNil: [(self isConstant: word )
					ifTrue: [self addOp: (self lookUp: 'doLit' ) address. 
						    self addOp: (self parseNumber: word ) ] 
					ifFalse: [isEvalMode := true. ds clear. self writeLine: 'The word:' , word, ' is undefined'] ] ]
]!

intParseFast: aStr
	" An optimized int parse method."

	| result |
	result := 0.
	aStr do: [:each |			
			(each between: $0 and: $9) ifFalse: [^InvalidFormat signalWith: aStr	"error"].
			result := 10 * result + (each codePoint - 48)].
	^result!

invert
	| a |
	a := ds pop.
	ds push: a bitInvert "a~"!

ip
	^ip!

ip: anObject
	ip := anObject!

isConstant: aWord
	^(aWord first) isDigit
		or: [aWord size > 1 and: [(  '+-'  includes: aWord first ) and: [(aWord second) isDigit]]]!

isDigit: aChar
	^ aChar between: $0 and: $9
!

isEvalMode
	^isEvalMode!

isEvalMode: anObject
	isEvalMode := anObject!

isWhite: aChar
	"\r\ n \t"
	"^aChar codePoint  = 13 or:[ aChar codePoint  = 10 or: [aChar codePoint  = 9]]"
	^' \r\t\n' includes: aChar
!

key
	ds push: input
	"Input.Read()"!

lbrac
            isEvalMode := true!

less
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b < a!

lessEql
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b <= a!

lookUp: aWord
	| r |
	r := entries at: aWord ifAbsent: [nil] .
	^r ifNotNil: [:value | value last] ifNil: [nil]!

mem
	^mem!

mem: anObject
	mem := anObject!

minus
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b - a!

mod
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b % a
!

multiply
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b * a!

newInstance
	| class obj|
self halt.
	class := ds pop.
	obj := class new.
	ds push: obj!

next
	[ip > 0] whileTrue: 
			[wp := mem at: ip. " код программы состоит из адресов слов"
			ip := ip + 1. " следующая инструкция "
			(core at: (mem at: wp)) value]  "в этой же памяти находятся индексы слов/примитивов из core "!

nop!

not
	| a |
	a := ds pop.
	ds push: a not!

notEql
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b ~= a!

nrot
	| a b c |
	a := ds pop.
	b := ds pop.
	c := ds pop.
	ds push: a.
	ds push: c.
	ds push: b!

null
	ds push: nil!

or
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: (b or: [a])!

over
	ds push: (ds at: 2)!

parseNumber: aStr
	| factor sign str |
	factor := 1.0.
	sign := 1.
	str := aStr.
	(aStr first) = $-
		ifTrue: 
			[sign := -1.
			str := aStr rightString: aStr size - 1].
	(str first) = $+ ifTrue: [str := aStr rightString: aStr size - 1].
	str size to: 1 by: -1 do: 
			[:i | 
				(str at: i ) = $.	ifTrue:  
					[str := (str leftString: i-1) , (str rightString: str size - i).
					^(self intParseFast: str) * factor * sign].
			factor := factor * 0.1].
	^(self intParseFast: str) * sign!

plus
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: b + a!

quit
	^Error notYetImplemented!

rbrac
            isEvalMode := false!

readMem
	| address |
	address := ds pop.
	ds push: (mem at: address)!

readString
	| str  |
	str :=  self readString: input.
	isEvalMode 
		ifTrue: [ds push: str ]
		ifFalse: [  
			self addOp: ( self lookUp: 'doLit' ) address. 
			self addOp: str.
		]
!

readString: aStream
	| sb c |
	"self halt."
	sb := WriteStream on: String new.
	c := aStream next.
	[self isWhite: c] whileTrue: [c := aStream next].
	[aStream atEnd] whileFalse: 
			[sb nextPut: c.
			c := aStream next.
			c == $" ifTrue: [^sb contents]].
	^''!

readWord
	| str |
	str := self readWord: input.
	isEvalMode
		ifTrue: [ds push: str]
		ifFalse: 
			[self addOp: (self lookUp: 'doLit') address.
			self addOp: str]!

readWord: sr
	"| code sb |
	sb := ReadWriteStream on: String new.
	code := sr next.
	[(self isWhite: code) and: [code codePoint > 0]] whileTrue: [code = sr next].
	[sr atEnd not and: [(self isWhite: code) not and: [code codePoint > 0]]] whileTrue: 
			[sb nextPut: code.
			code := sr next].
	^sb contents"	
	^ sr nextWord
!

rot
	| a b  c|
	a := ds pop.
	b := ds pop.
	c := ds pop.
	ds push: b.	
	ds push: a.	
	ds push: c!

rs
	^rs!

rs: anObject
	rs := anObject!

searchKnowAddress: anInt
	| addresses ka |
	anInt  ifNil: [ ^-1 ].
	addresses := OrderedCollection new.
	entries valuesDo: [:each | each do: [:wh | addresses add: wh address]].
	addresses sort.
	ka := 0.
	addresses do: [:each | anInt >= each ifTrue: [ka := each] ifFalse: [^ka]]!

setCoreWord: aWord handler: aBlock immediate: aBool
	| address |
	address := core size + 1.
	core add: aBlock.  " set core handler "
	self
		define: aWord
		entry: address
		immediate: aBool. " set word entry "
	mem at: address put: address. "set core address "
	hereP := hereP + 1. "increase here"!

setMember
	^Error notYetImplemented!

setStaticMember
	^Error notYetImplemented!

skipComment: aStream params: aCharArray
	| c |
	c := aStream next.
	[(aCharArray includes: c) or: [aStream atEnd]] whileFalse: [c := aStream next]!

start
	| entryPoint |
	entryPoint := 8.	" адрес точки входа"
	ip := 0.	"устанавливаем IP = 0, чтобы завершить выполнение программы по ее завершении"
	wp := entryPoint.	" устанавливаем WP = 7 в качестве адреса точки входа"
	self doList.	" выполняем команду начала интерпретации слова, сохраняя IP = 0 на стеке возвратов"
	self next!

swap
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: a.
	ds push: b!

tick
	| address word |
	word := self readWord: input.
	address := (self lookUp: word) address.
	ds push: address!

true
	ds push: true
!

variable
	| here |
	here := self here.
	self here: self here +1.
	self addHeader: (self readWord: input).
	self addOp: (self lookUp: 'doList' )  address.
	self addOp: (self lookUp: 'doLit' )  address.
	self addOp:  here.
	self addOp: (self lookUp: 'exit' )  address!

words
	self writeLine: ''.
	entries keysDo: [:each | self write: each , ' '].
	self writeLine: ''!

wp
	^wp!

wp: anObject
	wp := anObject!

write: anObject 
	Transcript show: anObject!

writeLine: anObject
	self
		write: anObject;
		write: String lineDelimiter!

writeMem
	| data address |
	address := ds pop.
	data := ds pop.
	mem at: address put: data!

xor
	| a b |
	a := ds pop.
	b := ds pop.
	ds push: (b xor: [a])!

zbranch
	ip := ds pop
			ifFalse: [ip + (mem at: ip)] 
			ifTrue: [ip + 1]! !
!Forth categoriesFor: #addHeader:!helpers!public! !
!Forth categoriesFor: #addOp:!helpers!public! !
!Forth categoriesFor: #and!public! !
!Forth categoriesFor: #beginDefWord!public! !
!Forth categoriesFor: #bl!public! !
!Forth categoriesFor: #branch!public! !
!Forth categoriesFor: #comma!public! !
!Forth categoriesFor: #comment!public! !
!Forth categoriesFor: #commentLine!public! !
!Forth categoriesFor: #constant!public! !
!Forth categoriesFor: #core!accessing!private! !
!Forth categoriesFor: #core:!accessing!private! !
!Forth categoriesFor: #cr!public! !
!Forth categoriesFor: #dec!public! !
!Forth categoriesFor: #define:entry:immediate:!accessing!helpers!public! !
!Forth categoriesFor: #defineWord:params:!helpers!public! !
!Forth categoriesFor: #divide!public! !
!Forth categoriesFor: #doList!helpers!public! !
!Forth categoriesFor: #doLit!public! !
!Forth categoriesFor: #dot!public! !
!Forth categoriesFor: #dots!public! !
!Forth categoriesFor: #drop!public! !
!Forth categoriesFor: #ds!accessing!private! !
!Forth categoriesFor: #ds:!accessing!private! !
!Forth categoriesFor: #dump!accessing!public! !
!Forth categoriesFor: #dumpFrom:to:!accessing!public! !
!Forth categoriesFor: #dup!public! !
!Forth categoriesFor: #endDefWord!public! !
!Forth categoriesFor: #entries!accessing!private! !
!Forth categoriesFor: #entries:!accessing!private! !
!Forth categoriesFor: #eql!public! !
!Forth categoriesFor: #evalStream:!public! !
!Forth categoriesFor: #evalString:!public! !
!Forth categoriesFor: #execute!public! !
!Forth categoriesFor: #execute:!public! !
!Forth categoriesFor: #exit!public! !
!Forth categoriesFor: #false!public! !
!Forth categoriesFor: #getHereAddr!public! !
!Forth categoriesFor: #getMember!accessing!public! !
!Forth categoriesFor: #getStaticMember!accessing!public! !
!Forth categoriesFor: #getType!accessing!public! !
!Forth categoriesFor: #greater!public! !
!Forth categoriesFor: #greaterEql!public! !
!Forth categoriesFor: #hello!public! !
!Forth categoriesFor: #here!accessing!public! !
!Forth categoriesFor: #here:!accessing!public! !
!Forth categoriesFor: #immediate!public! !
!Forth categoriesFor: #inc!public! !
!Forth categoriesFor: #initCore!public! !
!Forth categoriesFor: #initExtra!public! !
!Forth categoriesFor: #initialize!public! !
!Forth categoriesFor: #interpreter!public! !
!Forth categoriesFor: #intParseFast:!accessing!public! !
!Forth categoriesFor: #invert!public! !
!Forth categoriesFor: #ip!accessing!private! !
!Forth categoriesFor: #ip:!accessing!private! !
!Forth categoriesFor: #isConstant:!public! !
!Forth categoriesFor: #isDigit:!public! !
!Forth categoriesFor: #isEvalMode!accessing!public! !
!Forth categoriesFor: #isEvalMode:!accessing!public! !
!Forth categoriesFor: #isWhite:!public! !
!Forth categoriesFor: #key!public! !
!Forth categoriesFor: #lbrac!public! !
!Forth categoriesFor: #less!public! !
!Forth categoriesFor: #lessEql!public! !
!Forth categoriesFor: #lookUp:!helpers!public! !
!Forth categoriesFor: #mem!accessing!private! !
!Forth categoriesFor: #mem:!accessing!private! !
!Forth categoriesFor: #minus!public! !
!Forth categoriesFor: #mod!public! !
!Forth categoriesFor: #multiply!public! !
!Forth categoriesFor: #newInstance!accessing!public! !
!Forth categoriesFor: #next!public! !
!Forth categoriesFor: #nop!public! !
!Forth categoriesFor: #not!public! !
!Forth categoriesFor: #notEql!public! !
!Forth categoriesFor: #nrot!public! !
!Forth categoriesFor: #null!public! !
!Forth categoriesFor: #or!public! !
!Forth categoriesFor: #over!accessing!public! !
!Forth categoriesFor: #parseNumber:!public! !
!Forth categoriesFor: #plus!public! !
!Forth categoriesFor: #quit!operations-shutdown!public! !
!Forth categoriesFor: #rbrac!public! !
!Forth categoriesFor: #readMem!public! !
!Forth categoriesFor: #readString!public! !
!Forth categoriesFor: #readString:!public! !
!Forth categoriesFor: #readWord!public! !
!Forth categoriesFor: #readWord:!public! !
!Forth categoriesFor: #rot!public! !
!Forth categoriesFor: #rs!accessing!private! !
!Forth categoriesFor: #rs:!accessing!private! !
!Forth categoriesFor: #searchKnowAddress:!public! !
!Forth categoriesFor: #setCoreWord:handler:immediate:!public! !
!Forth categoriesFor: #setMember!accessing!public! !
!Forth categoriesFor: #setStaticMember!accessing!public! !
!Forth categoriesFor: #skipComment:params:!public! !
!Forth categoriesFor: #start!public! !
!Forth categoriesFor: #swap!public! !
!Forth categoriesFor: #tick!public! !
!Forth categoriesFor: #true!public! !
!Forth categoriesFor: #variable!public! !
!Forth categoriesFor: #words!public! !
!Forth categoriesFor: #wp!accessing!private! !
!Forth categoriesFor: #wp:!accessing!private! !
!Forth categoriesFor: #write:!accessing!public! !
!Forth categoriesFor: #writeLine:!accessing!public! !
!Forth categoriesFor: #writeMem!public! !
!Forth categoriesFor: #xor!public! !
!Forth categoriesFor: #zbranch!public! !

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

at: anIndex
	^anArray at: top - anIndex + 1!

clear
	top := 0!

peek
	^anArray at: top!

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
    top := 0!

size 
	^ top! !
!Stack categoriesFor: #at:!public! !
!Stack categoriesFor: #clear!public! !
!Stack categoriesFor: #peek!public! !
!Stack categoriesFor: #pop!public! !
!Stack categoriesFor: #printOn:!public! !
!Stack categoriesFor: #push:!public! !
!Stack categoriesFor: #setsize:!public! !
!Stack categoriesFor: #size!public! !

!Stack class methodsFor!

new
	^super new
		setsize: 10;
		yourself! !
!Stack class categoriesFor: #new!public! !

WordHeader guid: (GUID fromString: '{a397759d-77be-481b-8c92-003a5b8eb877}')!
WordHeader comment: ''!
!WordHeader categoriesForClass!Kernel-Objects! !
!WordHeader methodsFor!

address
	^address!

address: anObject
	address := anObject!

immediate
	^immediate!

immediate: anObject
	immediate := anObject!

isEnable
	^isEnable!

isEnable: anObject
	isEnable := anObject!

printOn: target
	super printOn: target.
	target
		nextPut: $(;
		nextPutAll: 'addr = ';
		nextPutAll: address printString;
		nextPutAll: ', imm = ';
		nextPutAll: immediate printString;
		nextPutAll: ', en = ';
		nextPutAll: isEnable printString;
		nextPut: $)! !
!WordHeader categoriesFor: #address!accessing!public! !
!WordHeader categoriesFor: #address:!accessing!public! !
!WordHeader categoriesFor: #immediate!accessing!public! !
!WordHeader categoriesFor: #immediate:!accessing!public! !
!WordHeader categoriesFor: #isEnable!accessing!public! !
!WordHeader categoriesFor: #isEnable:!accessing!public! !
!WordHeader categoriesFor: #printOn:!public! !

"Binary Globals"!

