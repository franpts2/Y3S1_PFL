% a)
list_append([],[],[]).

list_append([],L2,L2).

list_append([H1|T1],L2,[H1|Rest]):-
	list_append(T1,L2,Rest).

% b)
list_member(Elem,List):-
	list_append(_,[Elem|_],List).

% c)
list_last(List,Last):-
	list_append(_,[Last],List).

% d)
list_nth(N,List,Elem):-
	length(Prefix,N),
	append(Prefix,[Elem|_],List).

% e)
lists_append([],[]).

lists_append([H|T],List):-
	lists_append(T,RestFlat), % flattens the lists
	list_append(H,RestFlat,List). % backtracks 
