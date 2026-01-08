% a)
list_size([],0).

list_size([_|T],Size):-
	list_size(T,Size1),
	Size is Size1+1.

% b)
list_sum(List,Sum):-
	list_sum_aux(List,0,Sum).

list_sum_aux([],Acc,Acc).

list_sum_aux([H|T],Acc,Sum):-
	Acc1 is Acc+H,
	list_sum_aux(T,Acc1,Sum).

% c)
list_prod(List,Prod):-
    list_prod_aux(List,1,Prod).

list_prod_aux([],Acc,Acc).
list_prod_aux([H|T],Acc,Prod):-
    Acc1 is Acc*H,
    list_prod_aux(T,Acc1,Prod).

