not(X) :- X, !, fail.
not(_X).

can_unify(A,B):-
	not(not(A=B)).

unifiable([],_,[]).

% H unifies with Term
unifiable([H|T],Term,[H|Rest]):-
	can_unify(H,Term), !,
	unifiable(T,Term,Rest).

unifiable([_|T],Term,List2):-
	unifiable(T,Term,List2).