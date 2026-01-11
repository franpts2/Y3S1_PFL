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