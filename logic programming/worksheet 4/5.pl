% a)
print_n(0,_):- !.

print_n(N,S):-
	N1 is N - 1,
	format('~w',[S]),
	print_n(N1,S).

% b)
print_text(Text,Symbol,Padding):-
	write(Symbol),
	print_n(Padding,' '),
	format('~s',[Text]), 		% ~s handles the list of ASCII codes
	print_n(Padding,' '),
	write(Symbol).
