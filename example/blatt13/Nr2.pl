==(X,X).

%nur erstes vorkommen
delete1(_,[],[]).
delete1(X,[X|Xs],Xs).
delete1(X,[Y|Xs],[Y|Ys]) :- not(==(X,Y)),delete1(X,Xs,Ys).

%alle vorkommen
delete2(_,[],[]).
delete2(X,[X|Xs],Ys) :- delete2(X,Xs,Ys).
delete2(X,[Y|Xs],[Y|Ys]) :- not(==(X,Y)),delete2(X,Xs,Ys).
