not(X) :- X, !, fail.
not(_X).

my_reverse(List1,List2):-
    my_reverse_aux(List1,[],List2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,List2):-
    my_reverse_aux(T,[H|Acc],List2).

% state(M_Left,C_Left,BoatLoc) 
initial(state(3, 3, left)).
final(state(0, 0, right)).

move(OldState, NewState) :- 
    valid_move(OldState, NewState, _Label).

% on_boat(M,C)
on_boat(1,0). % 1 M, 0 C
on_boat(2,0). % 2 M, 0 C
on_boat(0,1). % 0 M, 1 C
on_boat(0,2). % 0 M, 2 C
on_boat(1,1). % 1 M, 1 C

is_safe(M,C):-
	% left side
	(M =:= 0; M >= C),
	
	% right side 
	M_Right is 3 - M,
	C_Right is 3 - C,
	(M_Right =:= 0; M_Right >= C_Right).

% move(OldState,NewState,Label)

% boat moves left -> right
valid_move(state(ML1,CL1,left),state(ML2,CL2,right),move(MB, CB, right)):-
	on_boat(MB,CB),
	ML1 >= MB, CL1 >= CB, % ensure there are enough ppl on left
	ML2 is ML1-MB,
	CL2 is CL1-CB,
	is_safe(ML2,CL2).

% boat moves right -> left
valid_move(state(ML1,CL1,right),state(ML2,CL2,left),move(MB, CB, left)):-
	on_boat(MB,CB),
	% calculate who is currently on the right side
    MR1 is 3 - ML1, 
    CR1 is 3 - CL1,
    % ensure those people are available to board the boat from the right
    MR1 >= MB, CR1 >= CB, 
    % When they arrive on the left, the left side counts INCREASE
    ML2 is ML1 + MB,
    CL2 is CL1 + CB,
    is_safe(ML2, CL2).

play(CurState,Path,Path):-
	final(CurState),!.

play(CurState,Path,States):-
	move(CurState,NextState),
	not(member(NextState,Path)),
	play(NextState,[NextState|Path],States).

missionaires_and_cannibals(Moves):-
	initial(Init),
	play(Init,[Init],States),
	my_reverse(States,Moves).

% -----------------------------------------------------

% BONUS
% Find the shortest path by gradually increasing the allowed length
shortest_missionaries_and_cannibals(Moves) :-
    initial(Init),
	setof(Len-Path,(
		play(Init,[Init],Path),
		length(Path,Len)
	),[_ShortestLen-States|_]),
	my_reverse(States,Moves).

% BONUS
play_readable :- 
    initial(Init),
    play(Init, [Init], States),
    my_reverse(States, Path),
    nl, write('--- Solution Found ---'), nl,
    print_path(Path),
    nl, write('Press ; for next solution or Enter to stop.'), nl.

print_path([state(M, C, Loc)]) :-
    format('Final State: ~w Missionaries, ~w Cannibals on the Left. Boat is at the ~w.~n', [M, C, Loc]).

print_path([state(M1, C1, Loc1), state(M2, C2, Loc2) | Rest]) :-
    % Calculate how many people moved
    DM is abs(M1 - M2),
    DC is abs(C1 - C2),
    format('From (~w, ~w, ~w): Move ~w M and ~w C to the ~w.~n', [M1, C1, Loc1, DM, DC, Loc2]),
    print_path([state(M2, C2, Loc2) | Rest]).