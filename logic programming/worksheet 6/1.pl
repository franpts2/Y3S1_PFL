% a)
map(_,[],[]).

map(Pred,[H1|T1],[H2|T2]):-
	% construct the goal Pred(H1,H2)
	G =.. [Pred,H1,H2],

	% execute the goal
	call(G),

	map(Pred,T1,T2).