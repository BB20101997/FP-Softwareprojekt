true.
boolean(true).
boolean(false).
invert(true,false).
invert(false,true).
test(X):-boolean(X).
test(X):- X.
=(X,X).
