true.
boolean(true).
boolean(false).
invert(true,false).
invert(false,true).
and(true,true,true).
or(true,true,true).
or(true,false,true).
or(false,true,true).
or(false,false,false).
test(X):-boolean(X).
test(X):- X.
=(X,X).
