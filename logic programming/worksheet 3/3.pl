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

    
% d)
del_all_list([],List1,List1).

del_all_list([H|T],List1,List2):-
    del_all(H,List1,Temp),
    del_all_list(T,Temp,List2).
    
% e)
del_dups([],[]).

del_dups([H|T],[H|Rest]):-
    del_all(H,T,CleanT),
    del_dups(CleanT,Rest).

% bonus: [a,b,b,a] -> [b,a]
del_dups_last([],[]).

del_dups_last([H|T],List2):- % H is in tail (dup) - skip it
    memberchk(H,T),
    del_dups(T,List2).

del_dups_last([H|T],[H|Rest]):- % H is not in tail (dup) - keep it
    \+ memberchk(H,T),
    del_dups(T,Rest).