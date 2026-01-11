% a)
print_full_list([]):-
	write('[]'), !.

print_full_list(L):-
	write('['),
	print_full_list_aux(L).

print_full_list_aux([X]):- 
	write(X),
	write(']'),
	!.

print_full_list_aux([H|T]):-
	write(H),
	write(', '),
	print_full_list_aux(T).