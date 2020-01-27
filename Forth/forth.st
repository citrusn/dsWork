f := Forth new.
f isEvalMode.
f lbrac.

entryPoint := 8.
f ip: 0.
f wp: entryPoint.
f doList.
f next.

f here 
f dump 
f words

f evalString: ' 5 3 - .' 
f evalString: '506 constant c  c .' .
f evalString: 'variable g g .' .

f evalString: ' 333 g !  g ?' 
f evalString: 'g @ g ?'

f searchKnowAddress: 56
f entries at: '?full'  
f lookUp: 'while-tst'  
f ds clear

f evalString: 'word Color type new' 

f evalString: ': do 0 begin  1+ dup dup .  10 >= until ; '
f evalString: ' do '

 f evalString: ': while-tst begin dup 0 > while 1 - repeat ; 
			s" start while-loop: " . 100 dup . cr while-tst s" end of loop" . cr '

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
