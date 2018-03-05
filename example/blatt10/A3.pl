kandidat(meier).
kandidat(mueller).
kandidat(schroeder).
kandidat(schulz).

vorstand(Vorsitz, Schriftfuehrer, Kassenwart) :- kandidat(Vorsitz),
						kandidat(Schriftfuehrer),
						kandidat(Kassenwart).

==(X,X).
\=(X,Y) :- not(==(X,Y)).

imvorstand(D,D,_,_).
imvorstand(D,_,D,_).
imvorstand(D,_,_,D).

%schulz vorsitzender oder müller nicht im vorstand
mvsv(V,S,K) :- \+(imvorstand(mueller,V,S,K)).
mvsv(schulz,_,_).

korrektervorstand(V, S, K) :- 	vorstand(V,S,K),
				\=(V,S), \=(V,K), \=(S,K), %jeder hat maximal eine Vorstandsposition(dreiköpfig)
				\+(imvorstand(mueller,V,S,K) , imvorstand(meier,V,S,K)), %nicht müller und meier
				\+(imvorstand(mueller,V,S,K) ,  not(==(V,schulz))), %müller nur unter vorsitz von schulz
				\+(imvorstand(schroeder,V,S,K) , not(imvorstand(meier,V,S,K))), %schröder nur mit meier
				\+(imvorstand(meier,V,S,K) , ==(S,schulz)), %nicht meier im vorstand beim schriftführer schulz
				\+(imvorstand(schulz,V,S,K), ==(V,schroeder)). %nicht schulz im vorstand beim vorsitzenden schröder


