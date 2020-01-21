f := Forth new.
f isEvalMode.

entryPoint := 8.
f ip: 0.
f wp: entryPoint.
f doList.
f next.

f initCore.
f initExtra.

f evalString: ' 100 503 + .' 
f evalString: '506 constant c  c .' .
f evalString: 'variable g g .' .

f dump. 
f evalString: 'c . . .'
f evalString: ' 333 g !  g ?' . 
f evalString: 'g @ g ?'.

f searchKnowAddress: 56
f lookUp: 'c' 

f here