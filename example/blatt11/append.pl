append([],L,L).
append([E|R],L,[E|RL]) :- append(R,L,RL).
