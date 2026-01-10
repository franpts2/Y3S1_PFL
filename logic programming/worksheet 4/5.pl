% a)
print_n(0,_):- !.

print_n(N,S):-
	N1 is N - 1,
	format('~w',[S]),
	print_n(N1,S).