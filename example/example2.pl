true.
fib(0,1).
fib(1,1).
fib(X,R) :- is(T,>(X,1)),T,is(X1,-(X,1)),is(X2,-(X,2)),fib(X1,F1),fib(X2,F2),is(R,+(F1,F2)).
numFree(0  ,            []).
numFree(Num, .(Free,Frees)) :- is(Q,>(Num,0)),Q,is(R,-(Num,1)),findall(X,true,Free),numFree(R,Frees).
