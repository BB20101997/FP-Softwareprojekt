mobile(fisch(W),W) :- is(true,>=(W,0)).
mobile(bruecke(L,R), G):- is(true,>(G,0)),is(GL,div(-(G,1),2)),mobile(L,GL), mobile(R,GL).
