/*
Board: [A,B,C,D,E,F] (each of A, B, C, D, E, F is one of green, yellow, blue, orange, white & black)
*/

% solve(+Constraints,-Board)



% anywhere(X, Board): X can go anywhere


% next_to(X, Y, Board): X must be next to Y
next_to(X,X,_).

next_to(X,Y,[A,B,C,D,E,F]):-
	consecutive(X,Y,[A,B,C,D,E,F]).

next_to(X,Y,[A,B,C,D,E,F]):-
	consecutive(Y,X,[A,B,C,D,E,F]).

% aux: consecutive - verifies if the first two args occur consecutively in the list provided in the 3rd arg
consecutive(X,Y,Board):-
	append(_Prefix,[X,Y|_Suffix],Board).


% one_space(X, Y, Board): X must be one space apart from Y
one_space(X,X,_).

one_space(X,Y,[A,B,C,D,E,F]):-
	interspaced(X,Y,[A,B,C,D,E,F]).

one_space(X,Y,[A,B,C,D,E,F]):-
	interspaced(Y,X,[A,B,C,D,E,F]).

interspaced(X,Y,[A,B,C,D,E,F]):-
	append(_,[X,_,Y|_],[A,B,C,D,E,F]).

% across(X, Y, Board): X must be across from Y


% same_edge(X, Y, Board): X must be on the same edge as Y


% position(X, L, Board): X must be in one of the positions given in the list L


