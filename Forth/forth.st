f := Forth new.
f isEvalMode.
f lbrac.

entryPoint := 8.
f ip: 0.
f wp: entryPoint.
f doList.
f next.

f words

f evalString: ' 5 3 - .' 
f evalString: '506 constant c  c .' .
f evalString: 'variable g g .' .

f evalString: ' 333 g !  g ?' 
f evalString: 'g @ g ?'

f searchKnowAddress: 56
f entries at: 'doList'  
f lookUp: 'create'  
f ds clear

f here 
f dump
f evalString: ': const create  '' doList  , '' doLit ,  ,  '' exit ,  ;'
f evalString: '34 const dd dd'

f evalString: ': var0 variable  11123 swap !  ;'
f evalString: 'var0 dd dd'
f evalString: ': allot here @ dup nrot + here ! ;'

f evalString: ' 3 allot'

f evalString: ': do 0 begin dup . cr 1+ dup 10 >= until ; '
f evalString: ' do '

f evalString: ': do immediate here @ ;'.
f evalString: ': loop  immediate 1+  over over <  doLit [ '' 0branch , ] , here @ - , ;'.

f evalString: ': test-loop 2  0  do  dup .  loop ; ' 
f evalString: ' test-loop ' 

BEGIN ... flag UNTIL
BEGIN ... flag WHILE ... REPEAT
BEGIN ... AGAIN


f evalString: ': again-tst begin  dup . cr again ; ' "бесконечный цикл"

f evalString: ': while-tst begin dup 0 > while 1 -  dup . cr repeat ; 
			s" start while-loop: " . 0 dup . cr while-tst s" end of loop" . cr '

f evalString: ': until-tst begin  dup . cr 1- dup 1 < until ; 
			s" start while-loop: " cr 100  while-tst s" end of loop" . cr '

f evalString: '  : ?full  if  101 else 202  then ; '
dolist  zbranch  5 dolit 101 branch 3  dolit 202 exit

f evalString: '  : ?full  if  105 then ; '
dolist zbranch 5 dolit 105  exit

f evalString: 'true ?full  '
f evalString: 'false ?full  '

f evalString: ': if immediate doLit [ ''  0branch , ] , here @ 0 , ;' 
dolist dolit  10 (zbranch)  comma gethereaddr readmem dolit 0 comma exit

f lookUp: 'else'  addr = 278
f evalString: ' : else immediate doLit [ '' branch , ] , here @ 0 , swap dup here @ swap - swap ! ; ' 

self evalString: ': then immediate dup here @ swap - swap ! ;'.
dolist dup gethereaddr readmem swap minus swap writemem exit

