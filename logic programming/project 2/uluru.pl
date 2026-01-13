:- use_module(library(lists)).

% =============================================================================
% 1. Core Logic (Part 1)
% =============================================================================

% The board layout consists of 6 slots: [A, B, C, D, E, F]
% Slots A, B and D, E, F form the two edges; C is the connector
colors([green, yellow, blue, orange, white, black]).

% solve(+Constraints, -Board)
% Succeeds if a board permutation satisfies all provided constraints
solve(Constraints, Board) :-
    Board = [_A, _B, _C, _D, _E, _F],
    colors(Colors),
    permutation(Colors, Board),
    check_constraints(Constraints, Board).

% Base case: no more constraints to verify
check_constraints([], _Board).

% Recursive case: verify each constraint using call/2
check_constraints([C|T], Board) :-
    call(C, Board),
    check_constraints(T, Board).

% =============================================================================
% 2. Scoring & Optimization (Part 2)
% =============================================================================

% best_score(+Constraints, -Score)
% Finds the maximum possible score (0 is best, -1 per violation)
best_score(Constraints, Score) :-
    colors(Colors),
    findall(CurScore, (
        permutation(Colors, Board),
        calculate_score(Constraints, Board, CurScore)
    ), AllScores),
    max_list(AllScores, Score).

% calculate_score(+Constraints, +Board, -Score)
% Base case: score starts at 0
calculate_score([], _Board, 0).

% Success case: Constraint satisfied (0 points)
calculate_score([C|T], Board, Score) :-
    call(C, Board), !,
    calculate_score(T, Board, RemainingScore),
    Score is 0 + RemainingScore.

% Failure case: Constraint violated (-1 point)
calculate_score([_|T], Board, Score) :-
    calculate_score(T, Board, RemainingScore),
    Score is -1 + RemainingScore.

% =============================================================================
% 3. Constraint Definitions
% =============================================================================

% anywhere(X, Board): Always succeeds
anywhere(_X, _Board).

% next_to(X, Y, Board): X and Y are adjacent (excluding A and F)
next_to(X, X, _Board).
next_to(X, Y, Board) :- consecutive(X, Y, Board).
next_to(X, Y, Board) :- consecutive(Y, X, Board).

% one_space(X, Y, Board): Exactly one space between X and Y
one_space(X, X, _Board).
one_space(X, Y, Board) :- interspaced(X, Y, Board).
one_space(X, Y, Board) :- interspaced(Y, X, Board).

% across(X, Y, Board): X is on edge {A,B} and Y is on {D,E,F} (or vice-versa)
across(X, X, _Board).

across(X, Y, [A, B, _C, D, E, F]) :-
    (member(X, [A, B]), member(Y, [D, E, F])) ;
    (member(X, [D, E, F]), member(Y, [A, B])).

% same_edge(X, Y, Board): Both tokens on {A,B} or both on {D,E,F}
same_edge(X, X, _Board).

same_edge(X, Y, [A, B, _C, _D, _E, _F]) :- 
	member(X, [A, B]), member(Y, [A, B]).

same_edge(X, Y, [_A, _B, _C, D, E, F]) :- 
	member(X, [D, E, F]), member(Y, [D, E, F]).

% position(X, L, Board): X is at one of the 1-based indices in L
position(X, L, Board) :-
    nth1(Index, Board, X),
    member(Index, L).

% =============================================================================
% 4. Helper Predicates
% =============================================================================

% consecutive/3: Checks if X and Y are adjacent in the list
consecutive(X, Y, Board) :- append(_, [X, Y|_], Board).

% interspaced/3: Checks if X and Y have exactly one element between them
interspaced(X, Y, Board) :- append(_, [X, _, Y|_], Board).

% max_list/2: Finds the largest number in a list
max_list([H|T], Max) :- max_list_helper(T, H, Max).
max_list_helper([], Max, Max).
max_list_helper([H|T], CurrMax, Max) :- H > CurrMax, !, max_list_helper(T, H, Max).
max_list_helper([_|T], CurrMax, Max) :- max_list_helper(T, CurrMax, Max).

% =============================================================================
% 5. Example Puzzles
% =============================================================================

% Example 1: 12 solutions
example(1, [next_to(white, orange), next_to(black, black), across(yellow, orange), 
            next_to(green, yellow), position(blue, [1, 2, 6]), across(yellow, blue)]).

% Example 2: 1 solution
example(2, [across(white, yellow), position(black, [1, 4]), position(yellow, [1, 5]), 
            next_to(green, blue), same_edge(blue, yellow), one_space(orange, black)]).

% Example 3: No solutions (Best score: -1)
example(3, [across(white, yellow), position(black, [1, 4]), position(yellow, [1, 5]), 
            same_edge(green, black), same_edge(blue, yellow), one_space(orange, black)]).

% Example 4: same as above, different order of constraints
example(4, [ position(yellow,[1,5]), one_space(orange,black), same_edge(green, black),
			same_edge(blue,yellow), position(black,[1,4]), across(white,yellow) ]).