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

% d)
read_number(X):-
	read_number_aux(0,X).

read_number_aux(Acc,X):-
    peek_code(Code),
    (Code == 10 ->      % check if Code is Line Feed (10)
        get_code(_),    % consume line feed
        X = Acc         % unified final result
    ;
        get_code(C),
        Digit is C - 48, % convert ASCII to int
        Acc1 is Acc*10 + Digit,
        read_number_aux(Acc1,X)
    ).