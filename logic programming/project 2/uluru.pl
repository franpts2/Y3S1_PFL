:- use_module(library(lists)).

/*
Board: [A,B,C,D,E,F] (each of A, B, C, D, E, F is one of green, yellow, blue, orange, white & black)
*/
colors([green, yellow, blue, orange, white, black]).

% solve(+Constraints,-Board)
solve([],Board).
 
solve(Constraints,Board):-
	Board = [A,B,C,D,E,F],

	% generate permutation
	colors(Colors),
	permutation(Colors,Board),

	check_constraints(Constraints,Board).

% aux: check constraints recursively
check_constraints([],_Board).

check_constraints([C|T],Board):-
	call(C,Board),
	check_constraints(T,Board).


% best_score(+Constraints, -Score)
best_score(Constraints,Score):-
	% generate permutation
	colors(Colors),
	findall(CurScore,(
		permutation(Colors,Board),
		calculate_score(Constraints,Board,CurScore)
	),AllScores),

	max_list(AllScores,Score).

% aux: check constraints (and subtract points) recursively
calculate_score([],_Board,0).

calculate_score([C|T],Board,Score):-
	call(C,Board), !,
	calculate_score(T,Board,RemainingScore),
	Score is 0 + RemainingScore.

% constraint diesnt succeed
calculate_score([_|T],Board,Score):-
	calculate_score(T,Board,RemainingScore),
	Score is -1 + RemainingScore.

not(X) :- X, !, fail. 
not(_X).

% max_list(+List, -Max)
max_list([H|T], Max) :-
    max_list_helper(T, H, Max).

% max_list_helper(+RemainingList, +CurrentMax, -FinalMax)
max_list_helper([], CurrentMax, CurrentMax).

max_list_helper([H|T], CurrentMax, Max) :-
    H > CurrentMax, !,
    max_list_helper(T, H, Max).

max_list_helper([_|T], CurrentMax, Max) :-
    max_list_helper(T, CurrentMax, Max).

% anywhere(X, Board): X can go anywhere
anywhere(_X,_Board).

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

% across(X, Y, Board): X must be across from Y (\= edges)
across(X,X,_).

across(X,Y,[A,B,_C,D,E,F]):-
	(member(X,[A,B]), member(Y,[D,E,F]))
	;
	(member(X,[D,E,F]), member(Y,[A,B])).

% same_edge(X, Y, Board): X must be on the same edge as Y
% edges: [A,B] & [D,E,F]
same_edge(X,X,_).

% both in edge [A,B]
same_edge(X,Y,[A,B,_C,_D,_E,_F]):-
	member(X,[A,B]),
	member(Y,[A,B]).

% both in edge [D,E,F]
same_edge(X,Y,[_A,_B,_C,D,E,F]):-
	member(X,[D,E,F]),
	member(Y,[D,E,F]).

% position(X, L, Board): X must be in one of the positions given in the list L
position(X,L,Board):-
	nth1(Index, Board, X),
	member(Index, L).

% TEST EXS - ?- example(1, _E), solve(_E, Solutions)
%% 12 solutions
example(1, [ next_to(white,orange),
			next_to(black,black),
			across(yellow,orange),
			next_to(green,yellow),
			position(blue,[1,2,6]),
			across(yellow,blue) ]).

%% 1 solution
example(2, [ across(white,yellow),
			position(black,[1,4]),
			position(yellow,[1,5]),
			next_to(green, blue),
			same_edge(blue,yellow),
			one_space(orange,black) ]).

%% no solutions (5 constraints are satisfiable)
example(3, [ across(white,yellow),
			position(black,[1,4]),
			position(yellow,[1,5]),
			same_edge(green, black),
			same_edge(blue,yellow),
			one_space(orange,black) ]).

%% same as above, different order of constraints
example(4, [ position(yellow,[1,5]),
			one_space(orange,black),
			same_edge(green, black),
			same_edge(blue,yellow),
			position(black,[1,4]),
			across(white,yellow) ]).