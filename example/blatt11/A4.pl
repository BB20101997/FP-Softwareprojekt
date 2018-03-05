true.

boolean(false).
boolean(true).

and(X,Y,true) :- boolean(X),boolean(Y),X,Y.
and(X,Y,false) :- boolean(X),boolean(Y),\+(X,Y).

or(X,_,true) :- boolean(X),X.
or(_,X,true) :- boolean(X),X.
or(X,Y,false) :- bnot(X,true),bnot(Y,true).

bnot(X,true)  :- boolean(X),\+(X).
bnot(X,false) :- boolean(X),X.

ex1(X,Y,Z,RES) :- and(X,Y,V),or(V,Z,RES).
ex2(X,Y,Z,RES) :- and(X,Y,V1),and(Y,Z,V2),and(V2,Z,V3),or(V1,V3,RES).
ex3(X,Y,Z,RES) :- bnot(Y,NY),and(X,NY,V1),and(V1,Z,V2),or(V2,V3,RES),and(Z,Y,V4),or(V4,Z,V3).
