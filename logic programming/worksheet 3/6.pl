% a)
:-use_module(library(lists)).

rle(List1,List2):-
	rle_aux(List1,[],List2).

rle_aux([],Acc,List2):-
	my_reverse(Acc,List2).

rle_aux([H|T],Acc,List2):-
	group(=(H),[H|T],Prefix,Rest),
	length(Prefix,N),
	rle_aux(Rest,[H-N|Acc],List2).

my_reverse(List1,List2):-
	my_reverse_aux(List1,[],List2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,List2):-
	my_reverse_aux(T,[H|Acc],List2).

% b)
repeat(Elem,N,List):-
	repeat_aux(Elem,N,[],List).

repeat_aux(_,0,Acc,Acc).

repeat_aux(Elem,N,Acc,List):-
	N > 0,
	N1 is N-1,
	repeat_aux(Elem,N1,[Elem|Acc],List).

lists_append([],[]).

lists_append([H|T],List):-
	lists_append(T,RestFlat), % flattens the lists
	append(H,RestFlat,List). % backtracks 

un_rle(List1,List2):-
	un_rle_aux(List1,[],List2).

un_rle_aux([],Acc,List2):-
	lists_append(Acc,Temp),
	my_reverse(Temp,List2).

un_rle_aux([E-N|T],Acc,List2):-
	repeat(E,N,Temp),
	un_rle_aux(T,[Temp|Acc],List2).

/* natural recursion
% Base Case: Nothing left to expand
un_rle([], []).

% Recursive Step:
un_rle([E-N | T], List2) :-
    repeat(E, N, Expanded),       % 1. Create [E, E, E...]
    un_rle(T, RestExpanded),      % 2. Recursively expand the rest
    append(Expanded, RestExpanded, List2). % 3. Join them together
*/