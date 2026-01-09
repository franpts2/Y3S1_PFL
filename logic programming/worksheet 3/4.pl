% a)
list_append([],[],[]).

list_append([],L2,L2).

list_append([H1|T1],L2,[H1|Rest]):-
	list_append(T1,L2,Rest).

