% a)
is_ordered([]).
is_ordered([_]).

is_ordered([H1,H2|T]):-
	H1 =< H2,
	is_ordered([H2|T]).

% b)
insert_ordered(Value,[],[Value]).

insert_ordered(Value,[H|T],[Value,H|T]):-
	Value =< H, !.

insert_ordered(Value,[H|T],[H|Rest]):-
	Value > H,
	insert_ordered(Value,T,Rest).

% c)
insert_sort([],[]).

insert_sort([H|T],List2):-
	insert_sort(T,SortedT),
	insert_ordered(H,SortedT,List2).
	