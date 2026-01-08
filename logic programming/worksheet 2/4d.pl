square_rec(0,0).

square_rec(N,S):-
	N > 0,
	N1 is N-1,
	square_rec(N1,S1),
	S is S1 + N + N1.