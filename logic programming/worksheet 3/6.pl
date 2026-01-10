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
