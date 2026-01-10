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

% c)
print_banner(Text,Symbol,Padding):-
	length(Text,Len),
	Width is 1 + Padding + Len + Padding + 1,
	print_n(Width,Symbol), nl,
	print_text(Text,Symbol,Padding), nl,
	print_n(Width,Symbol).