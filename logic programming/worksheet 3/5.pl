% a)
list_to(N,List):-
	list_to_aux(N,[],List).

list_to_aux(0,Acc,Acc).

list_to_aux(N,Acc,List):-
	N > 0,
	N1 is N - 1,
	list_to_aux(N1,[N|Acc],List).