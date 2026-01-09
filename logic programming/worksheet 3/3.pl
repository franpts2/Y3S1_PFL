% a)
invert(List1,List2):-
	invert_aux(List1,[],List2).

invert_aux([],Acc,Acc).

invert_aux([H | T],Acc,List2):-
	invert_aux(T,[H|Acc],List2).

% b)
del_one(Elem,[Elem|T],T). % Head = Elem

del_one(Elem,[H|T],[H|Rest]):-
	Elem \= H,
	del_one(Elem,T,Rest).

% c)
del_all(_,[],[]).

del_all(Elem,[Elem|T],List2):-
    del_all(Elem,T,List2).

del_all(Elem,[H|T],[H|Rest]):-
	Elem \= H,
	del_all(Elem,T,Rest).

    

