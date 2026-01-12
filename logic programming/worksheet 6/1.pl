% a)
map(_,[],[]).

map(Pred,[H1|T1],[H2|T2]):-
	% construct the goal Pred(H1,H2)
	G =.. [Pred,H1,H2],

	% execute the goal
	call(G),

	map(Pred,T1,T2).

% b)
fold(_,FinalValue,[],FinalValue).

fold(Pred,StartValue,[H|T],FinalValue):- % StartValue behaves as Acc
	call(Pred,StartValue,H,NewAcc), % Pred(Acc,H,NewAcc)
	fold(Pred,NewAcc,T,FinalValue).

sum(A, B, S):- S is A+B.

% c)
separate([],_,[],[]).

separate([H|T],Pred,[H|RestYes],No):-
	call(Pred,H), !,
	separate(T,Pred,RestYes,No).

separate([H|T],Pred,Yes,[H|RestNo]):-
	separate(T,Pred,Yes,RestNo).

even(X):- 0 =:= X mod 2.