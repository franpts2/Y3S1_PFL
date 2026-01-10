% a)
is_ordered([]).
is_ordered([_]).

is_ordered([H1,H2|T]):-
	H1 =< H2,
	is_ordered([H2|T]).