% Vorgaben auf der Vorlesung Anfang

% Definition der Ehefrau-Ehemann-Relation:
ehemann(christine, heinz).
ehemann(maria,     fritz).
ehemann(monika,    herbert).
ehemann(angelika,  hubert).

% Definition der Kind-Mutter-Relation:
mutter(herbert, christine).
mutter(angelika,christine).
mutter(hubert,  maria).
mutter(susanne, monika).
mutter(norbert, monika).
mutter(andreas, angelika).

%vater(norbert,herbert) :- mutter(norbert,monika), ehemann(monika,herbert).
vater(Kind,Vater) :- mutter(Kind,Mutter), ehemann(Mutter,Vater).

grossvater(E,G) :- vater(E,V), vater(V,G).
grossvater(E,G) :- mutter(E,M), vater(M,G).

append([],L,L).
append([E|R],L,[E|RL]) :- append(R,L,RL).

last(L,E) :- append(_,[E],L).

member(E,L) :- append(_,[E|_],L).

delete(E,L,R) :- append(L1,[E|L2],L), append(L1,L2,R).

sublist(T,L) :- append(_,L2,L), append(T,_,L2).

% Vorgaben aus der Vorlesung Ende

% Aufgabe 2.1

maennlich(P) :- ehemann(_,P).
maennlich(P) :- vater(_,P).
maennlich(P) :- grossvater(_,P).
% weder eheman,vater noch grossvater aber männlich
maennlich(norbert).
maennlich(andreas).

% männliches geschwisterkind
bruder(P,B) :- geschwisterkind(P,B),maennlich(B).

geschwisterkind(P,G) :- vater(P,V),vater(G,V),mutter(P,M),mutter(G,M).

%bruder des Vaters oder der Mutter
onkel(K,O) :- vater(K,V),bruder(V,O).
onkel(K,O) :- vater(K,M),geschwisterkind(M,G),ehemann(G,O).
onkel(K,O) :- mutter(K,M),bruder(M,O).
onkel(K,O) :- mutter(K,M),geschwisterkind(M,G),ehemann(G,O).

%vorfahren sind die eltern oder die vorfahren der eltern
vorfahre(N,V) :- mutter(N,V).
vorfahre(N,V) :- mutter(N,T),vorfahre(T,V).
vorfahre(N,V) :- vater(N,V).
vorfahre(N,V) :- vater(N,T),vorfahre(T,V).

% Aufgabe 2.2

lockup(K,[pair(K,V)|_],V).
lockup(K,[_|KVs],V) :- lockup(K,KVs,V).

member2(E,[E|R]) :- member(E,R).
member2(E,[_|R]) :- member2(E,R).

reverse([],[]).
reverse([X|Xr],Ys) :- append(Yh,[X],Ys),reverse(Xr,Yh).

reverseAku(X,Y) :- reverseAku(X,Y,[]).
reverseAku([],Y,Y).
reverseAku([X|Xs],Y,T) :- reverseAku(Xs,Y,[X|T]).


