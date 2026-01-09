% a)
list_to(N,List):-
	list_to_aux(N,[],List).

list_to_aux(0,Acc,Acc).

list_to_aux(N,Acc,List):-
	N > 0,
	N1 is N - 1,
	list_to_aux(N1,[N|Acc],List).

% b)
list_from_to(Inf,Sup,List):-
	list_from_to_aux(Inf,Sup,[Sup],List).

list_from_to_aux(Inf,Inf,Acc,Acc).

list_from_to_aux(Inf,Sup,Acc,List):-
	Sup > Inf,
	Sup1 is Sup-1,
	list_from_to_aux(Inf,Sup1,[Sup1|Acc],List).

% c)
list_from_to_step(Inf,Sup,_,[]):-
    Inf > Sup.

list_from_to_step(Inf,Sup,Step,[Inf|T]):-
    Inf =< Sup,
    Inf1 is Inf + Step;
    list_from_to_step(Inf1,Sup,Step,T).