% a)
print_full_list([]):-
	write('[]'), !.

print_full_list(L):-
	write('['),
	print_full_list_aux(L).

print_full_list_aux([X]):- 
	write(X),
	write(']'),
	!.

print_full_list_aux([H|T]):-
	write(H),
	write(', '),
	print_full_list_aux(T).

% b)
print_list(L):-
	length(L,Len),
	Len < 12,
	print_full_list(L), !.

print_list(L):-
	write('['),
	print_first_three(L),
	write(',..., '),
	print_middle_three(L),
	write(',..., '),
	print_last_three(L),
    write(']'), nl.

print_first_three(L):-
    length(Prefix,3),
    append(Prefix,_,L),

    print_3elements(Prefix).

print_middle_three(L):-
    length(L,TotalLen),
    Skip is (TotalLen - 2) // 2,

    length(Prefix,Skip),
    append(Prefix,MiddleAndSuffix,L),

    append(MiddleThree,_,MiddleAndSuffix),
    length(MiddleThree,3),

    print_3elements(MiddleThree).

print_last_three(L):-
    length(L,TotalLen),
    Skip is TotalLen - 3,

    length(Suffix,3),
    length(SkipPrefix,Skip),
    append(SkipPrefix,Suffix,L),

    print_3elements(Suffix).

print_3elements([A, B, C]) :-
    format('~w, ~w, ~w', [A, B, C]).

% c)
print_matrix([]):- !.

print_matrix([H|T]):-
	print_full_list(H), nl,
	print_matrix(T).

% d)
:- use_module(library(lists)).

print_n(0,_):- !.

print_n(N,S):-
	N1 is N - 1,
	format('~w',[S]),
	print_n(N1,S).

n_digits(N, NDigits):-
	number_codes(N,Codes),
	length(Codes,NDigits).

print_numbered_matrix(M):-
	length(M,NLines),
	n_digits(NLines,NDigitsLastLine),

	print_numbered_matrix_aux(M,1,NDigitsLastLine).

print_numbered_matrix_aux([],_,_):- !.

print_numbered_matrix_aux([H|T],NLine,NDigitsLastLine):-
	n_digits(NLine,LenNLine),
	Padding is NDigitsLastLine - LenNLine,

	print_n(Padding,' '),
	format('~d ', [NLine]),
	print_full_list(H), nl,

	NL1 is NLine + 1,
	print_numbered_matrix_aux(T,NL1,NDigitsLastLine).

% e)
print_list_custom([],S,_,E):-
	write(S), write(E), !.

print_list_custom(L,S,Sep,E):-
	write(S),
	print_list_custom_aux(L,S,Sep,E).

print_list_custom_aux([X],_,_,E):-
	write(X), write(E), !.

print_list_custom_aux([H|T],S,Sep,E):-
	write(H),
	write(Sep),
	print_list_custom_aux(T,S,Sep,E).