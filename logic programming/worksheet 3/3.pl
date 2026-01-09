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

% f)
list_perm([],[]).

list_perm([H|T],L2):-
    my_select(H,L2,Rest), % find H in L2 and get remaining els Rest
    list_perm(T,Rest).

my_select(Elem,[Elem|T],T). % Head = Elem -> remainder is tail

my_select(Elem,[H|T],[H|Rest]):- % Head\=Elem -> keep head and recurse on tail
    my_select(Elem,T,Rest). 

% g)
replicate(0,_,[]).

replicate(Amount,Elem,[Elem|T]):-
    Amount > 0,
    NewAmount is Amount - 1,
    replicate(NewAmount,Elem,T).

% h)
intersperse(_,[],[]).

intersperse(_,[X],[X]).

intersperse(Elem,[H1, H2 |T],[H1, Elem |Rest]):-
    intersperse(Elem,[H2|T],Rest).

% i)
insert_elem(_,[],Elem,[Elem]).

insert_elem(0,T,Elem,[Elem|T]).

insert_elem(Index,[H|T],Elem,[H|T2]):-
    Index > 0,
    I is Index - 1,
    insert_elem(I,T,Elem,T2).

