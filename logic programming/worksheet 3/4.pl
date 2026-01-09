% a)
list_append([],[],[]).

list_append([],L2,L2).

list_append([H1|T1],L2,[H1|Rest]):-
	list_append(T1,L2,Rest).

% b)
list_member(Elem,List):-
	append(_,[Elem|_],List).

% c)
list_last(List,Last):-
	append(_,[Last],List).

% d)
list_nth(N,List,Elem):-
	length(Prefix,N),
	append(Prefix,[Elem|_],List).

% e)
lists_append([],[]).

lists_append([H|T],List):-
	lists_append(T,RestFlat), % flattens the lists
	append(H,RestFlat,List). % backtracks 

% f)
list_del(List,Elem,Res):-
	append(Bef,[Elem|After],List), % split: List = Bef + [Elem|After]
	append(Bef,After,Res).

% g)
list_before(First,Second,List):-
	append(BefSecond,[Second|_],List),
	append(_,[First|_],BefSecond). 

% h)
list_replace_one(X,Y,List1,List2):-
	append(Bef,[X|After],List1),
	append(Bef,[Y|After],List2).

% j)
list_repeated(X,List):-
	append(_,[X|AfterFirstX],List),
	append(_,[X|_],AfterFirstX).

% k)
list_shift_rotate(List1,N,List2):-
	length(Prefix,N),
	append(Prefix,Suffix,List1),
	append(Suffix,Prefix,List2).