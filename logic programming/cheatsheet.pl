% INDEX
/*
- PROJECT IMPLEMENTATION
    - Scoring & Optimization
    - Constraint Definitions
    - Helper Predicates
    - Example Puzzles
    - EXTRA CONSTRAINTS - PROJECT
        - DISTANCE-BASED CONSTRAINTS
        - POSITION-SPECIFIC CONSTRAINTS
        - RELATIVE ORDERING CONSTRAINTS
        - EDGE AND GROUP CONSTRAINTS
        - MULTIPLE TOKEN CONSTRAINTS
        - DIAGONAL/ACROSS VARIATIONS
        - SEQUENTIAL/PATTERN CONSTRAINTS
        - EXCLUSION/NEGATIVE CONSTRAINTS
        - CONDITIONAL CONSTRAINTS
        - COUNTING CONSTRAINTS
        - SET-BASED CONSTRAINTS
    - PART 2 DIF POINTS - PROJECT

- THEORY ANNOTATIONS
    - 1. LISTS
    - 2. CUT, I/O, AND USEFUL PREDICATES
    - 3. COLLECTING SOLUTIONS, GRAPHS, AND TREES
    - 4. META-PROGRAMMING, OPERATORS & MODELS
    - 5. UNIFICATION & EXECUTION MODEL (LOGIC PROG)

- SOLVED EXS
    - 1. RECURSION & ARITHMETIC
    - 2. RECURSION & ARITHMETIC & LISTS
    - 3. IO
    - 4. MAX OF SMTH W BACKTRACKING
    - 5. GRAPHS, PATHFINDING & STATE-SPACE SEARCH
    - 6. UNIFICATION & META-LOGIC

- REPLICAS
- SET-LIKE LIST OPERATIONS
- GENERAL BASE CONVERSIONS (DECIMAL <=> BASE 2-36)
- GRID, MATRIX & ADVANCED LIST PATTERNS
- DIFFERENCE LISTS (DL) - O(1) PERFORMANCE
- ADVANCED GRAPH & GRID LOGIC
*/

% =============================================================================
% PROJECT IMPLEMENTATION
% =============================================================================

:-use_module(library(lists)).

% The board layout consists of 6 slots: [A, B, C, D, E, F]
% Slots A, B and D, E, F form the two edges; C is the connector
colors([green, yellow, blue, orange, white, black]).

% solve(+Constraints, -Board)
% Succeeds if a board permutation satisfies all provided constraints
solve(Constraints, Board):-
    Board = [_A, _B, _C, _D, _E, _F],
    colors(Colors),
    permutation(Colors, Board),
    check_constraints(Constraints, Board).

% Base case: no more constraints to verify
check_constraints([], _Board).

% Recursive case: verify each constraint using call/2
check_constraints([C|T], Board):-
    call(C, Board),
    check_constraints(T, Board).

% =============================================================================
% Scoring & Optimization
% =============================================================================

% best_score(+Constraints, -Score)
% Finds the maximum possible score (0 is best, -1 per violation)
best_score(Constraints, Score):-
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
% Constraint Definitions
% =============================================================================

% anywhere(X, Board): Always succeeds
anywhere(_X, _Board).

% next_to(X, Y, Board): X and Y are adjacent (excluding A and F)
next_to(X, X, _Board).
next_to(X, Y, Board):-consecutive(X, Y, Board).
next_to(X, Y, Board):-consecutive(Y, X, Board).

% one_space(X, Y, Board): Exactly one space between X and Y
one_space(X, X, _Board).
one_space(X, Y, Board):-interspaced(X, Y, Board).
one_space(X, Y, Board):-interspaced(Y, X, Board).

% across(X, Y, Board): X is on edge {A,B} and Y is on {D,E,F} (or vice-versa)
across(X, X, _Board).
across(X, Y, [A, B, _C, D, E, F]):-
    (member(X, [A, B]), member(Y, [D, E, F])) ;
    (member(X, [D, E, F]), member(Y, [A, B])).

% same_edge(X, Y, Board): Both tokens on {A,B} or both on {D,E,F}
same_edge(X, X, _Board).
same_edge(X, Y, [A, B, _C, _D, _E, _F]):-
	member(X, [A, B]), member(Y, [A, B]).
same_edge(X, Y, [_A, _B, _C, D, E, F]):-
	member(X, [D, E, F]), member(Y, [D, E, F]).

% position(X, L, Board): X is at one of the 1-based indices in L
position(X, L, Board):-
    nth1(Index, Board, X),
    member(Index, L).

% =============================================================================
% Helper Predicates
% =============================================================================

consecutive(X, Y, Board):-append(_, [X, Y|_], Board).

interspaced(X, Y, Board):-append(_, [X, _, Y|_], Board).

max_list([H|T], Max):-max_list_helper(T, H, Max).
max_list_helper([], Max, Max).
max_list_helper([H|T], CurrMax, Max):-H > CurrMax, !, max_list_helper(T, H, Max).
max_list_helper([_|T], CurrMax, Max):-max_list_helper(T, CurrMax, Max).

% =============================================================================
% Example Puzzles
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


% =============================================================================
% THEORY ANNOTATIONS
% =============================================================================

/*
# 1. LISTS

Use this to display full list (prevent truncation):
?- functor_on_list(Smth, L), write_term(L, [max_depth(0)]).

## Syntax and Internal Representation
- .(Head, Tail) : Internal dot functor representation. [1,2,3] = .(1, .(2, .(3, []))).
- Strings : Lists of ASCII codes. "Hello" = [72, 101, 108, 108, 111].
-:-use_module(library(lists)). : Required for many list operations.

## Fundamental Built-in Predicates
- is_list(?Term) : Check if term is a list.
- length(?List, ?Size) : Relates list to size. Can generate lists.
- member(?Elem, ?List) : Non-deterministic (backtracks).
- memberchk(?Elem, ?List) : Deterministic (first match only).
- append(?L1, ?L2, ?L3) : Join or decompose lists.
- reverse(?List, ?Reversed) : Reverse order.

## List Selection and Sub-lists
- nth0(?Index, ?List, ?Elem): Accesses an element using 0-based indexing.
- nth1(?Index, ?List, ?Elem): Accesses an element using 1-based indexing.
- nth0(?Index, ?List, ?Elem, ?Rest): Retrieves/replaces an element and identifies the remaining Rest list.

- nth0(I, L, E) - Get element E at 0-based position I. (nth0(2, [a,b,c,d], X). -> X = c.)
- nth0(I, L, E known) - Search: Find index I of element E. (nth0(I, [a,b,c], b). -> I = 1.)
- nth0(I, L, E, R) - Delete/Extract: Get element E and the list R without it. (nth0(2, [a,b,c,d], X, R). -> X=c, R=[a,b,d].)
- nth0(I, L, E, R) - Add: Insert element E at index I into list R to get L. (nth0(2, X, c, [a,b,d]). -> X = [a,b,c,d].)
- nth0(I, L, E, R) - Replace: Extract element, then insert a new one at the same index. (nth0(1, [a,b,c], X, R), nth0(1, S, f, R). -> S=[a,f,c].)

- select(?X, ?XList, ?Y, ?YList) : Finds an occurrence of X in XList, replaces it with Y, and produces YList.
- delete(+List, +ToDel, -Result) : Deletes all occurrences of a specific element from a list.
- delete(+List, +ToDel, +Count, -Result): Deletes a specific Count of occurrences of an element.
- last(?Init, ?Last, ?List) : Identifies the Last element and the Init prefix (the rest of the list)
- segment(?List, ?Segment) : Succeeds when Segment is a contiguous subsequence of List
- sublist(+List, ?Part, ?Before, ?Length, ?After) : Extracts a contiguous Part of a list with a specific Length and context.

## Sorting and Transformations
- sort(+List, -SortedList): Sorts a proper list and removes duplicate elements.
- keysort(+PairList, -SortedList): Sorts a list of Key-Value pairs by the key; original order is retained for identical keys.
- transpose(?Matrix, ?Transposed): Converts matrix rows into columns and vice-versa.
- remove_dups(+List, ?PrunedList): Removes duplicate elements from a list.
- permutation(?List, ?Permutation): Generates all possible permutations of a list through backtracking.
- rotate_list(+Amount, ?List, ?Rotated): Cyclically shifts a list by a specified number of positions.
- append(+ListOfLists, -List): Concatenates a list of lists into a single list.

## Math and Aggregation

- sumlist(+ListOfNumbers, ?Sum): Calculates the total sum of all numbers in a list.
- max_member(?Max, +List): Identifies the largest element in the list.
- min_member(?Min, +List): Identifies the smallest element in the list.
- max_member(:Comp, ?Max, +List): Finds the maximum element using a custom comparison predicate.
- min_member(:Comp, ?Min, +List): Finds the minimum element using a custom comparison predicate.

## Higher-Order Predicates
- maplist(:Pred, +List): Succeeds if the predicate succeeds for every element in the list.
- maplist(:Pred, +L1, ?L2): Applies a predicate to transform each element of L1 into L2.
- map_product(:Pred, +Xs, +Ys, ?List): Applies a predicate to the Cartesian product of two lists.
- scanlist(:Pred, +List, ?Start, ?Final): Performs a foldl operation to reduce a list to a final value.
- cumlist(:Pred, +List, ?Start, ?ResultList): Similar to scanlist but provides a list of all intermediate accumulation steps.
- some(:Pred, +List): Succeeds if at least one element in the list satisfies the predicate.
- include(:Pred, +List, ?Filtered): Filters a list to keep only elements that satisfy the predicate.
- exclude(:Pred, +List, ?Filtered): Filters a list to remove elements that satisfy the predicate.
- group(:Pred, +List, ?Front, ?Back): Splits a list into a Front group where the predicate succeeds and a Back remainder.


# 2. CUT, I/O, AND USEFUL PREDICATES

## The Cut Operator (!)
- Purpose: Prunes search tree, prevents backtracking.
- Red Cut: Changes the logic/results of the program; removing it changes the output.
- Green Cut: Only improves efficiency; removing it does not change the set of solutions.

## Negation and Conditionals
- Negation as Failure (NAF): 
	not(X):-X, !, fail. 
	not(_X).
- If-Then-Else:
	- via Cut:
  	if_then_else(If, Then, Else):-If, !, Then.
  	if_then_else(If, Then, Else):-Else.
	- via NAF:
	ite(If, Then, Else):-If, Then.
  	ite(If, Then, Else):-not(If), Else.

## FORALL
my_forall(Cond,Action):-
	not((Cond,not(Action))).

## Input / Output (I/O)
not undone by backtracking.
- read(X): Read term (needs period '.').
- write(X): Print term.
- format(String, Args): 
  - ~w (write), ~d (decimal), ~n (newline), ~s (string/codes), ~f (floating point n)
- nl: New line.
- skip_line: Clear buffer.

- get_char(C) / put_char(C): Handle single characters.
- get_code(N) / put_code(N): Handle ASCII codes.
- peek_char(C) / peek_code(N): Look at the next input without consuming it.

- see(File) / seen: Redirect input from a file / Close file.
- tell(File) / told: Redirect output  CHEATSHEETto a file / Close file.

## Useful Utilities
- repeat: Always succeeds. Used for repeat-until loops.
- between(L, U, X):  Generates or tests if X integer between L and U.

-> library(random)
- random(L, U, V): Generates a random number in a range.
- random_member(X, List): Selects a random element.
- random_permutation(L1, L2): Randomly shuffles a list.


# 3. COLLECTING SOLUTIONS, GRAPHS, AND TREES

## Collecting Solutions
- findall(Term, Goal, List): Collects all. Empty list if none.
- bagof(Term, Goal, List): Groups by uninstantiated vars. Fails if none.
- setof(Term, Goal, List): Like bagof, but sorted and no duplicates.

Existential Quantifier (^): Used in bagof or setof to ignore specific variables 
							  for grouping (when the list appears in groups they are needed!)

## Graphs and Search (DFS/BFS)

-> DFS with Cycle Prevention & Path:
connects_dfs(S, F, Path):-
    connects_dfs(S, F, [S], ReversedPath),
    reverse(ReversedPath, Path).

connects_dfs(F, F, Path, Path). 

connects_dfs(S, F, T, Path):-
    connected(S, N),
    not(memberchk(N, T)),
    connects_dfs(N, F, [N|T], Path).

-> BFS w Path:
connects_bfs(S, F, Path):-
    connects_bfs([[S]], F, Path).

connects_bfs([[F|T] | _], F, Path):-
    reverse([F|T], Path). % Reverse to get S -> ... -> F order

connects_bfs([[S|T] | R], F, Path):-
    findall([N, S | T], 
        (connected(S, N), not(member(N, [S|T]))), 
        NewPaths),
    append(R, NewPaths, NextQueue),
    connects_bfs(NextQueue, F, Path).

## Binary Trees
- Structure: node(Value, Left, Right) or null.
- Member:
    tree_member(Val, node(Val, _L, _R) ).

    tree_member(Val, node(V, L, _R) ):-
	    [Val < V,] tree_member(Val, L).

    tree_member(Val, node(V, _L, R) ):-
	    [Val > V,] tree_member(Val, R).

- List all members
    tree_list( null, [] ).

    tree_list( node(Val, L, R), List ):-
	    tree_list(L, Left),
	    tree_list(R, Right),
	    append(Left, [Val|Right], List).

- Ordered Check:
    tree_is_ordered(Tree):-
        tree_list(Tree, List),
        sort(List, List).

- Insert Element:
    tree_insert( null, V, node(V, null, null) ).

    tree_insert( node(V, L, R), V, node(V, L, R) ).

    tree_insert( node(V, L, R), Val, node(V, NL, R) ):-
    	Val < V, tree_insert( L, Val, NL).

    tree_insert( node(V, L, R), Val, node(V, L, NR) ):-
	    Val > V, tree_insert( R, Val, NR).

- Size:
	tree_size(null,0).

	tree_size(node(_,Left,Right), Size):-
    	tree_size(Left,SL),
    	tree_size(Right,SR),
    	Size is 1 + SL + SR.

- Height:
    tree_height(null, 0).
    tree_height(node(_, L, R), H):-
        tree_height(L, HL), tree_height(R, HR),
        H is 1 + max(HL, HR).

- Balanced Check: A tree is balanced if the height difference between subtrees is ≤1.
    tree_is_balanced(null).

    tree_is_balanced(node(Val, L, R)):-
	    tree_is_balanced(L),
	    tree_is_balanced(R),
	    tree_height(L, HL),
	    tree_height(R, HR),
    	abs(HL-HR) =< 1.

- Map:  Tree  and  NewTree  are  trees  of  the same shape, and Pred(X, Y) is true for every X in Tree and corresponding Y in NewTree.
	tree_map(_,null,null).

	tree_map(Pred,node(V1,L1,R1),node(V2,L2,R2)):-
    	call(Pred,V1,V2), % Pred(V1,V2)
    	tree_map(Pred,L1,L2),
    	tree_map(Pred,R1,R2).

- Value at Level (flexible):
	tree_value_at_level(Tree,Value,Level):-nonvar(Value),var(Level), !,(find_level(Tree,Value,0,Level) -> true; Level = -1).
	tree_value_at_level(Tree,Value,Level):-find_level(Tree,Value,0,Level).
	
	find_level(node(Value,_,_),Value,L,L). % found at cur node
	find_level(node(_,Left,_),Value,L,FoundL):-L1 is L+1,find_level(Left,Value,L1,FoundL).
	find_level(node(_,_,Right),Value,L,FoundL):-L1 is L+1,find_level(Right,Value,L1,FoundL).

## Generic Puzzle Solver

initial(InitialState).
final(State):-winning_condition(State).
move(OldState, NewState):-valid_move(OldState,NewState).

play(CurrState,Path,Path):-
    final(CurState), !.
play(CurrSt, Path, States):-
	move(CurrSt, Next),
	not( member(Next, Path) ),
	play(Next, [Next|Path], States).

play:-
	initial(Init),
	play(Init, [Init], States),
	reverse(States, Plays),
	write(Plays).

### Shortest Path (using setof)

To find the shortest path, collect all paths and sort them by length.

play:-
	initial(Init),
	setof(
		Length-Path,
		( play(Init, [Init], Path),
    		length(Path, Length) ),
		[_ShortestLength-States|_]
	),
	reverse(States, Path),
	write(Path).
    

# 4. META-PROGRAMMING, OPERATORS & MODELS

## Type Checking
- var(A)-True if uninstantiated variable.
- nonvar(A)-True if NOT a variable (atom, number, or compound).
- ground(A)-True if contains no uninstantiated variables in any substructure.
- atom(A)-True if Prolog atom.
- number(A)-True if integer or float.
- atomic(A)-True if atom or number.
- compound(A)-True if compound term (e.g., f(x)).

## Term Manipulation
- functor(+Term,?Name,?Arity) or functor(?Term,+Name,+Arity):
  - if Term is instantiated, returns the name and arity of the term
  - if Term is not instantiated, creates a new term with given name and arity

- arg(+Index,+Term,?Arg): instantiates Arg with the argument in the Nth position (starts at index 1).

- +Term =.. ?[Name|Args] or ?Term =.. +[Name|Args] (Univ):
  - given a term, returns a list w/ name and args of the term
  - given a proper list, creates a new term w/ name and args as specified by the contents in the list

- call(Goal, ...): Executes a goal. Can be used for higher-order programming.

## Operators
- op(Precedence, Type, Name).
- Precedence: 1 (highest priority) to 1200 (lowest).
- Types:
	- Infix: 
		- xfx (non-assoc)-Both operands must have strictly lower precedence than the operator.
		- xfy (right-assoc)-The left operand can have equal or lower precedence; the right must be lower. 
		- yfx (left-assoc)-The right operand can have equal or lower precedence; the left must be lower. 
	- Prefix: fx, fy.
    - Postfix: xf, yf.

## Computational models
- emulate DFAs/NFAs
accept(Str):-
	initial(State),
	accept(Str, State).

accept([], State):-
	final(State).

accept([S|Ss], State):-
	delta(State, S, NState),
	accept(Ss, NState).

- emulate PDAs
accept(Str):- initial(State), accept(Str, State, []).

accept([], State, []):- final(State).

accept([S|Ss], State, Stack):-
	delta(State, S, Stack, NewState, NewStack),
	accept(Ss, NewState, NewStack).

- emulate TMs
tm(Str):- 
	initial(State),
	append(Str, [empty], StrEmpty),
	tm([empty], StrEmpty, State).

tm(Left, [S|Right], State):-
	delta(State, Left, [S|Right], NewState, NewLeft, NewRight),!,
	tm(NewLeft, NewRight, NewState).

tm(_, _, State):- final(State).

- emulate CFGs
accept(Str):- s(Str).

s([]).
s([X]).
s([X|SX]):- append(S, [X], SX), s(S).

# 5. UNIFICATION & EXECUTION MODEL (LOGIC PROG)

## UNIFICATION (Matching Terms)
Two terms unify if they are identical or can become identical through variable 
substitution.

- Ground Term: No variables (fully instantiated).
- Unground Term: Contains variables.
- Substitution (θ): A set of pairs {Var = Term} applied to a term.
- MGU (Most General Unifier): The substitution that compromises variables as 
  little as possible (the most general result).
- Occurs Check: A safety check to see if a variable exists inside the term it 
  is being unified with (prevents infinite loops like X = f(X)).
  - standard Prolog skips this for efficiency.
  - use: unify_with_occurs_check(X, Y) for safety.

### Unification Rules:
1. Constants unify only if identical.
2. Variable + Term: Variable is instantiated to that term.
3. Variable + Variable: They become bound to each other.
4. Compound Terms: Unify if Functor and Arity match AND all arguments unify.

## PROLOG EXECUTION MODEL
Prolog uses a Depth-First Search (DFS) traversal of the search tree.

- Resolvent: A stack (list) of goals still to be processed.
- Goal Selection: Left-to-right (the leftmost goal in the resolvent is chosen).
- Clause Selection: Top-to-bottom (clauses are tried in the order they appear in the file).
- Backtracking: When a path fails, Prolog returns to the last choice point to 
  try the next available clause.

### Search Trees:
- Root: The initial query.
- Nodes: Current resolvents.
- Edges: Substitutions from matching a goal to a clause head.
- Leaves: Success (empty resolvent) or Failure (no matching clauses).

### Completeness & Efficiency:
- DFS is NOT complete: It can get stuck in infinite branches (e.g., left recursion) 
  even if a solution exists elsewhere.
- Clause Order: Changes the order of answers found.
- Goal Order: Changes the size of the search tree; can affect termination.
- Heuristic: "Fail as fast as possible." Place tests/guards and goals with more 
  ground terms or fewer solutions first to prune the tree early.

## MULTI-DIRECTIONAL POWER OF NTH0/1
Standard built-ins are often multi-directional, meaning they work differently 
depending on which parts are "ground" (known) vs "var" (unknown).

- nth0(Index, List, Elem): 
    - (i, i, v) -> Get: Elem at Index.
    - (v, i, i) -> Search: Find Index of Elem.
- nth0(Index, List, Elem, Rest): 
    - (i, i, v, v) -> Extract: Get Elem and List without it.
    - (i, v, i, i) -> Add: Insert Elem at Index into Rest to get List.
    - (i, i, i, v) -> Replace: Combined extract/add logic.
*/

% =============================================================================
% SOLVED EXS
% =============================================================================

/*
# 1. RECURSION & ARITHMETIC

## Arithmetic Recursion (Standard vs. Tail)

### Factorial
- Standard: factorial(0,1). factorial(N,F):-N>0, N1 is N-1, factorial(N1,F1), F is F1*N.
- Tail: factorial(N,F):-fact_aux(N,1,F). fact_aux(0,Acc,Acc). fact_aux(N,Acc,F):-N>0, N1 is N-1, Acc1 is Acc*N, fact_aux(N1,Acc1,F).

### Fibonacci
- Standard (Exponential complexity): fib(0,0). fib(1,1). fib(N,F):-N>1, N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.
- Tail (Linear complexity): fib(N,F):-fib(N,0,1,F). fib(0,A,_,A). fib(N,A,B,F):-N>0, N1 is N-1, Next is A+B, fib(N1,B,Next,F).

### Sum
- Standard: sum_rec(0,0). sum_rec(N,Sum):-N > 0,N1 is N-1,sum_rec(N1,Sum1),Sum is Sum1 + N. 
- Tail: sum_rec(N,Sum):-sum_rec(N,Sum,0). sum_rec(0,Sum,Sum). sum_rec(N,Sum,Acc):-N > 0,N1 is N-1,Acc1 is Acc+N,sum_rec(N1,Sum,Acc1).

### Power
- Standard: pow_rec(_,0,1). pow_rec(X,1,X). pow_rec(X,Y,P):-Y > 1,Y1 is Y-1,pow_rec(X,Y1,P1),P is P1*X.
- Tail: pow(X,Y,P):-pow(X,Y,P,1). pow(_,0,Acc,Acc). pow(X,Y,P,Acc):-Y>0, Y1 is Y-1, Acc1 is Acc*X, pow(X,Y1,P,Acc1).

## Square w/out mult
square_rec(0,0). square_rec(N,S):-N > 0, N1 is N-1, square_rec(N1,S1), S is S1 + N + N1.

### Collatz
collatz(1,0).
% n even
collatz(N,S):-N > 1,N mod 2 =:= 0,N1 is N//2,collatz(N1,S1),S is S1 + 1.
% n odd
collatz(N,S):-N > 1,N mod 2 =\= 0,N1 is 3*N+1,collatz(N1,S1),S is S1 + 1.

### is Prime
is_prime(2).
is_prime(X):-X > 2,check_no_factors(X,2).
check_no_factors(X,X).
check_no_factors(X,D):-D < X,X mod D =\= 0,D1 is D+1,check_no_factors(X,D1).

### GCD and LCM
- GCD (Euclidean): gcd(X,0,X). gcd(X,Y,G):-Y>0, Z is X mod Y, gcd(Y,Z,G).
- LCM: lcm(X,Y,M):-gcd(X,Y,G), M is (X*Y) div G.

## Graphs and Hierarchies (Ancestry Example)
- Ancestor (Transitive Closure):
    ancestor(X,Y):-parent(X,Y).
    ancestor(X,Y):-parent(X,Z), ancestor(Z,Y).
- "Not Jay's descendant": 
    desc(X,gloria), \+ desc(X,jay).

## Date and Comparison Logic
- Split Date (YYYY-MM-DD): split_date(YR-M-D, YR, M, D).
- Before(Date1, Date2):
    before(D1, D2):-split_date(D1,Y1,M1,D1), split_date(D2,Y2,M2,D2),
    (Y1<Y2 ; (Y1=Y2, M1<M2) ; (Y1=Y2, M1=M2, D1<D2)).
- Oldest Person: oldest(X):-born(X,_), \+ (born(Y,_), Y \= X, older(Y,X,Y)).


# 2. RECURSION & ARITHMETIC & LISTS

## Core List Manipulation
- Invert/Reverse: inv(L, R):-inv_aux(L, [], R). inv_aux([], A, A). inv_aux([H|T], A, R):-inv_aux(T, [H|A], R).
- Count Ocurr of El in list:
	count(Elem,List,N):-count(Elem,List,0,N). 
	count(_,[],Acc,Acc).
	count(Elem,[Elem|T],Acc,N):-Acc1 is Acc+1,count(Elem,T,Acc1,N).
	count(Elem,[H|T],Acc,N):-Elem \= H,count(Elem,T,Acc,N).
- Delete One vs All vs From List:
    - del_one(E, [E|T], T). del_one(E, [H|T], [H|R]):-E \= H, del_one(E, T, R).
    - del_all(_, [], []). del_all(E, [E|T], R):-del_all(E, T, R). del_all(E, [H|T], [H|R]):-E \= H, del_all(E, T, R).
	- del_all_list([],L1,L1). del_all_list([H|T],L1,L2):-del_all(H,L1,Temp),del_all_list(T,Temp,L2).
- Remove Duplicates:
    - Keep First: del_dups([], []). del_dups([H|T], [H|R]):-del_all(H, T, C), del_dups(C, R).
    - Keep Last: del_dups_last([], []). del_dups_last([H|T], R):-memberchk(H, T), !, del_dups_last(T, R). del_dups_last([H|T], [H|R]):-del_dups_last(T, R).
- Insert and Delete in Index:
	- insert_elem(_,[],Elem,[Elem]). insert_elem(0,T,Elem,[Elem|T]). insert_elem(I,[H|T],Elem,[H|T2]):-I > 0,I1 is I - 1,insert_elem(I1,T,Elem,T2).
	- delete_elem(_,[],_,[]). delete_elem(0,[H|T],H,T). delete_elem(I,[H|T],Elem,[H|Rest]):-I > 0,I1 is I - 1,delete_elem(I1,T,Elem,Rest).
- Replace: replace([],_,_,_,[]). replace([H|T],0,H,New,[New|T]). replace([H|T],I,Old,New,[H|Rest]):-I > 0,I1 is I - 1,replace(T,I1,Old,New,Rest).
- Intersperse: inter(_, [X], [X]). inter(E, [H1,H2|T], [H1,E|R]):-inter(E, [H2|T], R).
- Append: list_append([],[],[]). list_append([],L2,L2). list_append([H1|T1],L2,[H1|Rest]):-list_append(T1,L2,Rest).
- Repeat: repeat(Elem,N,List):-repeat_aux(Elem,N,[],List). repeat_aux(_,0,Acc,Acc). repeat_aux(Elem,N,Acc,List):-N > 0,N1 is N-1,repeat_aux(Elem,N1,[Elem|Acc],List).

## Positioning & Substitution (append)
- Nth Element (via append): nth(N, L, E):-length(P, N), append(P, [E|_], L).
- Replace One: replace_one(X, Y, L1, L2):-append(B, [X|A], L1), append(B, [Y|A], L2).
- Shift/Rotate: rotate(L1, N, L2):-length(P, N), append(P, S, L1), append(S, P, L2).
- is Member: list_member(Elem,List):-append(_,[Elem|_],List).
- Last El: list_last(List,Last):-append(_,[Last],List).
- Flatten List of Lists: lists_append([],[]). lists_append([H|T],List):-lists_append(T,RestFlat),append(H,RestFlat,List).
- delete: list_del(List,Elem,Res):-append(Bef,[Elem|After],List),append(Bef,After,Res).
- Before: list_before(First,Second,List):-append(BefSecond,[Second|_],List),append(_,[First|_],BefSecond). 
- is Repeated: list_repeated(X,List):-append(_,[X|AfterFirstX],List),append(_,[X|_],AfterFirstX).

## List Numbers
- Generate list to N: list_to(N,List):-list_to_aux(N,[],List). list_to_aux(0,Acc,Acc). list_to_aux(N,Acc,List):-N > 0,N1 is N - 1,list_to_aux(N1,[N|Acc],List).
- ... from Inf to Sup: list_from_to(Inf,Sup,List):-list_from_to_aux(Inf,Sup,[Sup],List). list_from_to_aux(Inf,Inf,Acc,Acc). list_from_to_aux(Inf,Sup,Acc,List):-Sup > Inf,Sup1 is Sup-1,list_from_to_aux(Inf,Sup1,[Sup1|Acc],List).
	- handles Inf > Sup: list_from_to_mod(Inf,Sup,List):-Inf > Sup, !,list_from_to(Sup,Inf,Temp),my_reverse(Temp,List).list_from_to_mod(Inf,Sup,List):-list_from_to(Inf,Sup,List).
- ... from Inf to Sup w/ Step: list_from_to_step(Inf,Sup,_,[]):-Inf > Sup. list_from_to_step(Inf,Sup,Step,[Inf|T]):-Inf =< Sup,Inf1 is Inf + Step,list_from_to_step(Inf1,Sup,Step,T).
	- handles Inf > Sup:list_from_to_step_mod(Inf,Sup,Step,[Inf|T]):-Inf > Sup, !,Inf1 is Inf - Step,list_from_to_step_mod(Inf1,Sup,Step,T). list_from_to_step_mod(Inf,Sup,_,[]):-Inf < Sup, !. list_from_to_step_mod(Inf,Sup,Step,List):-list_from_to_step(Inf,Sup,Step,List).
- List of Primes: primes(N,List):-list_to(N,Temp),include(is_prime, Temp, List).
- List of Fibonaccis: fibs(N,List):-list_from_to(0,N,Indices),maplist(fibonacci,Indices,List).

## Sorting
- Is Ordered: is_ord([]). is_ord([_]). is_ord([H1,H2|T]):-H1 =< H2, is_ord([H2|T]).
- Insert Sort: 
    i_sort([], []). i_sort([H|T], S):-i_sort(T, St), i_ord_ins(H, St, S).
    i_ord_ins(V, [], [V]). i_ord_ins(V, [H|T], [V,H|T]):-V =< H, !.
    i_ord_ins(V, [H|T], [H|R]):-V > H, i_ord_ins(V, T, R).

## Advanced List Patterns
- RLE (Run-Length Encoding): 
    rle(L1, L2):-group(=, L1, Groups), maplist(group_to_pair, Groups, L2). group_to_pair([H|T], H-N):-length([H|T], N).
	or
	rle(L1,L2):-rle_aux(L1,[],L2). rle_aux([],Acc,L2):-my_reverse(Acc,L2). rle_aux([H|T],Acc,L2):-group(=(H),[H|T],Prefix,Rest),length(Prefix,N),rle_aux(Rest,[H-N|Acc],L2).
- UNRLE: un_rle(L1,L2):-un_rle_aux(L1,[],L2). un_rle_aux([],Acc,L2):-lists_append(Acc,Temp),my_reverse(Temp,L2). un_rle_aux([E-N|T],Acc,L2):-repeat(E,N,Temp),un_rle_aux(T,[Temp|Acc],L2).
- Pascal Triangle:
	combination(N,R,Res):-factorial(N,FactN),factorial(R,FactR),NR is N - R,factorial(NR,FactNR),Res is FactN//(FactR * FactNR).
	pascal_line(N,Res):-pascal_line_aux(N,0,[],Res). pascal_line_aux(N,R,Acc,Acc):-R > N, !. pascal_line_aux(N,R,Acc,Line):-R =< N,R1 is R+1,combination(N,R,C),pascal_line_aux(N,R1,[C|Acc],Line).
	pascal(N,Lines):-pascal_aux(N,[],Lines).pascal_aux(0,Acc,Acc).pascal_aux(N,Acc,Lines):-N > 0,N1 is N - 1,pascal_line(N,LineN),pascal_aux(N1,[LineN|Acc],Lines).


# 3. IO

## Custom Printing & Formatting
- Printing Lists/Matrices:
    - Full list [a, b, c]: 
		print_full_list([]):-write('[]'), !. print_full_list(L):-write('['),print_full_list_aux(L).
		print_full_list_aux([X]):-write(X),write(']'),!. print_full_list_aux([H|T]):-write(H),write(', '),print_full_list_aux(T).
	- Full List but w custom separators, etc:
		print_list_custom([],S,_,E):-write(S), write(E), !. print_list_custom(L,S,Sep,E):- write(S),print_list_custom_aux(L,S,Sep,E).
		print_list_custom_aux([X],_,_,E):-write(X), write(E), !. print_list_custom_aux([H|T],S,Sep,E):- write(H),write(Sep),print_list_custom_aux(T,S,Sep,E).
	- List [a, b, c, ..., d, e, f, ..., g, h, i]:
		print_list(L):-length(L,Len),Len < 12,print_full_list(L), !. print_list(L):-write('['),print_first_three(L),write(',..., '),print_middle_three(L),write(',..., '),print_last_three(L),write(']'), nl.
		print_first_three(L):-length(Prefix,3),append(Prefix,_,L),print_3elements(Prefix).
		print_middle_three(L):-length(L,TotalLen),Skip is (TotalLen - 2) // 2,length(Prefix,Skip),append(Prefix,MiddleAndSuffix,L),append(MiddleThree,_,MiddleAndSuffix),length(MiddleThree,3),print_3elements(MiddleThree).
		print_last_three(L):-length(L,TotalLen),Skip is TotalLen - 3,length(Suffix,3),length(SkipPrefix,Skip),append(SkipPrefix,Suffix,L),print_3elements(Suffix).
		print_3elements([A, B, C]):-format('~w, ~w, ~w', [A, B, C]).
	- Matrix: print_matrix([]):- !. print_matrix([H|T]):-print_full_list(H),nl,print_matrix(T). 
    - Numbered Matrix: 
		n_digits(N, NDigits):-number_codes(N,Codes),length(Codes,NDigits).
		print_numbered_matrix(M):-length(M,NLines),n_digits(NLines,NDigitsLastLine),print_numbered_matrix_aux(M,1,NDigitsLastLine).
		print_numbered_matrix_aux([],_,_):- !. print_numbered_matrix_aux([H|T],NLine,NDigitsLastLine):-n_digits(NLine,LenNLine),Padding is NDigitsLastLine - LenNLine,print_n(Padding,' '),format('~d ', [NLine]),print_full_list(H), nl,NL1 is NLine + 1,print_numbered_matrix_aux(T,NL1,NDigitsLastLine).
- Recursive Char Printing: 
    print_n(0, _). print_n(N, S):-N > 0, write(S), N1 is N - 1, print_n(N1, S).

## String & Number Input (Side-effect based)
- Read Number (Accumulator):
    read_num(X):-read_num_aux(0, X).
    read_num_aux(Acc, X):-peek_code(C), (C == 10 -> get_code(_), X = Acc ; 
    get_code(Code), Digit is Code - 48, Acc1 is Acc * 10 + Digit, read_num_aux(Acc1, X)).
- Read String (ASCII codes):
    read_str(S):-read_str_aux([], S).
    read_str_aux(Acc, S):-get_code(C), (C == 10 -> reverse(Acc, S) ; read_str_aux([C|Acc], S)).
- Read Atom:
	read_atom(Atom) :-read_string(Codes),atom_codes(Atom, Codes).

## Input Validation Loops
- Repeat-Until Pattern: 
    get_valid(Min, Max, Val):-repeat, format('Range ~d-~d: ', [Min, Max]), read_num(X),
    (between(Min, Max, X) -> Val = X, ! ; write('Invalid!'), nl, fail).

% 4. MAX OF SMTH W BACKTRACKING
most_diversified(Company):-
	setof(NumCities-Company,Cities^(
		cities_by_company(Company,Cities),
		length(Cities,NumCities)
		),L),
	my_reverse(L,[MaxVal-AnyComp|Rest]),
	member(MaxVal-Company,[MaxVal-AnyComp|Rest]). % for backtracking

% 5. GRAPHS, PATHFINDING & STATE-SPACE SEARCH

## Find Flights & Pathfinding (DFS vs BFS)
- Depth-First Search (DFS): Good for finding ANY path.
    find_flights_dfs(Dest,Dest,_,[]).

	find_flights_dfs(Cur,Dest,Visited,[Code|Rest]):-
		flight(Cur,Next,_,Code,_,_),
		not(memberchk(Next,Visited)),
		find_flights_dfs(Next,Dest,[Next|Visited],Rest).

	find_flights(Origin,Destination,Flights):-
		find_flights_dfs(Origin,Destination,[Origin],Flights).

- Breadth-First Search (BFS): Guaranteed SHORTEST path by number of edges.
    find_flights_bfs(Origin,Destination,Flights):-bfs_queue([[Origin]],Destination,Flights).

	% base case: 1st path in queue reaches destination 
	bfs_queue([[Dest|T]|_],Dest,Path):-extract_codes_and_reverse([Dest|T],Path). % reverse to get S -> ... -> F order 

	bfs_queue([[Cur|T]|R],Dest,Path):-
		findall([Next,Code,Cur|T],Comp^Hr^Dur^(
			flight(Cur,Next,Comp,Code,Hr,Dur),
			not(memberchk(Next,T))
		),NewPaths),
		append(R,NewPaths,NextQueue),
		bfs_queue(NextQueue,Dest,Path).

	extract_codes_and_reverse(PathWCities,FinalCodes):-
		extract_codes(PathWCities,CodesRev),
		my_reverse(CodesRev,FinalCodes).

	% resulting list is like [City,Code,City,Code,...]
	extract_codes([],[]). extract_codes([_City],[]).
	extract_codes([_City,Code|T],[Code|Rest]):-extract_codes(T,Rest).

- Find Least Stops: Collect all BFS paths and filter for the minimum length.
    find_flights_least_stops(Origin,Dest,ListOfFlights):-
	findall(F,(
		find_flights_bfs(Origin,Dest,F)
	),[Shortest|RestFlights]),

	length(Shortest,MinLen),

	findall(F,(
		member(F,[Shortest|RestFlights]),
		length(F,MinLen)
	),ListOfFlights).

- Find Path w Predefined Stops:
	find_flights_stops(Origin,Destination,Stops,ListFlights):-
		setof(F,(
			find_flights_stops_aux(Origin,Destination,Stops,F)
		),ListFlights).

	find_flights_stops_aux(Origin,Dest,[],ListFlights):-find_flights_bfs(Origin,Dest,ListFlights).

	find_flights_stops_aux(Origin,Destination,[Stop|OtherStops],ListFlights):-
		find_flights_bfs(Origin,Stop,Flight1),
		find_flights_stops_aux(Stop,Destination,OtherStops,RestFlights),
		append(Flight1,RestFlights,ListFlights).

- Can A reach B? Checks if there is a path from A to B using only nodes in the Allowed list
	can_reach(A,B,Allowed):-
		dfs_reach(A,B,[A],Allowed).

	dfs_reach(Dest,Dest,_,_).

	dfs_reach(Cur,Dest,Visited,Allowed):-
		flight(Cur,Next,_,_,_,_),
		member(Next,Allowed),
		not(member(Next,Visited)),
		dfs_reach(Next,Dest,[Next|Visited],Allowed).

## Advanced Graph Theory
- Cycles:
	find_circular_trip(MaxSize,Origin,Cycle):-
		(find_cycle_dfs(Origin,Origin,[Origin],MaxSize,Cycle)).

	% cycle found! neighbour is gray
	find_cycle_dfs(Cur,Origin,_,MaxSize,[Code]):-
		MaxSize >= 1,
		flight(Cur,Origin,_,Code,_,_).

	% rec case. neighbour is white
	find_cycle_dfs(Cur,Origin,GrayNodes,MaxSize,[Code|Rest]):-
		MaxSize > 1,
		flight(Cur,Next,_,Code,_,_),
		Next \= Origin,
		not(member(Next,GrayNodes)), % node is white -> we explore it
		NewMax is MaxSize - 1,
		find_cycle_dfs(Next,Origin,[Next|GrayNodes],NewMax,Rest).
	
	find_circular_trips(MaxSize,Origin,Cycles):-
		setof(C,(find_circular_trip(MaxSize,Origin,C)),Cycles).

- Strongly Connected (All nodes reach all nodes):
    strongly_connected(ListOfNodes) :-
    	my_forall(member(U, ListOfNodes), 
        	my_forall(member(V, ListOfNodes), 
        	    can_reach(U, V, ListOfNodes))).

- Bridges (Critical Edges): An edge is a bridge if removing it makes the destination unreachable from the origin.
    bridges(ListOfBridges):-
		get_all_nodes(Allowed),

		findall(Code,Comp^Hr^Dur^Allowed^(
			flight(Origin,Dest,Comp,Code,Hr,Dur),
			not(can_reach(Dest,Origin,Allowed))
		),ListOfBridges).

- Tarjan's Logic (SCCs): Grouping nodes that are mutually reachable. 
    strongly_connected_components(Components):-
		get_all_nodes(Nodes),
		tarjan(Nodes,Nodes,[],Components).

	tarjan([], _, Acc, Acc).
	tarjan([Node|Rest],AllNodes,Acc,Final):-
		% if Node is not yet assigned to an SCC in Acc, find its component
		( (member(Sub,Acc), member(Node,Sub)) ->
			tarjan(Rest,AllNodes,Acc,Final)
		;
			find_scc(Node,AllNodes,NewSCC),
			append(Acc,[NewSCC],MidAcc),
			tarjan(Rest,AllNodes,MidAcc,Final)
		).

	find_scc(V,Nodes,SCC):-
		findall(U,(member(U,Nodes),can_reach(V,U,Nodes)),FromV),
		findall(X,(member(X,FromV),can_reach(X,V,Nodes)),SCCWDups),
		sort(SCCWDups,SCC).


# 6. UNIFICATION & META-LOGIC
- Can Unify: \+ \+ (A = B). (Tests if two terms can be unified without actually binding variables).
- Unifiable List: Filters a list for terms that can unify with a specific pattern.
    unifiable([], _, []).
    unifiable([H|T], Term, [H|Rest]):-\+ \+ (H = Term), !, unifiable(T, Term, Rest).
    unifiable([_|T], Term, Rest):-unifiable(T, Term, Rest).
*/

% =============================================================================
% REPLICAS
% =============================================================================

/*
# FUNCTOR, ARG & UNIV
- Functor
	% functor(+Term,?Name,?Arity)
	my_functor(Term,Name,Arity):-
		nonvar(Term), !,
		Term =.. [Name | Args],
		length(Args,Arity).

	% functor(?Term,+Name,+Arity)
	my_functor(Term,Name,Arity):-
		var(Term),
		length(Args,Arity),
		Term =.. [Name | Args].

- Arg (arg(+Index,+Term,?Arg))
	my_arg(Index,Term,Arg):-
		Term =.. [_ | Args],
		nth1(Index,Args,Arg).

- Univ (=..)
	% +Term =.. ?[Name|Args]
	univ(Term,[Name|Args]):-
		nonvar(Term), !,
		functor(Term,Name,Arity),
		get_args(1,Arity,Term,Args).

	get_args(I,Arity,_,[]):- I > Arity, !.
	get_args(I,Arity,Term,[Arg|Rest]):-
		arg(I,Term,Arg),
		I1 is I + 1,
		get_args(I1,Arity,Term,Rest).

	% ?Term =.. +[Name|Args]
	univ(Term,[Name|Args]):-
		var(Term),
		length(Args,Arity),
		functor(Term,Name,Arity),
		set_args(1,Arity,Term,Args).

	set_args(I, Arity, _, []):-I > Arity, !.
	set_args(I, Arity, Term, [Arg | Rest]) :-
		arg(I, Term, Arg),
    	Next is I + 1,
    	set_args(Next, Arity, Term, Rest).

# MAPLIST, SCANLIST & GROUP

- maplist
	map(_,[],[]).
	map(Pred,[H1|T1],[H2|T2]):-G =.. [Pred,H1,H2],call(G),map(Pred,T1,T2).

- scanlist
	fold(_,FinalValue,[],FinalValue).
	fold(Pred,StartValue,[H|T],FinalValue):-call(Pred,StartValue,H,NewAcc),fold(Pred,NewAcc,T,FinalValue).

- group
	take_while(_,[],[],[]).
	take_while(Pred,[H|T],[H|RestFront],Back):-call(Pred,H), !,take_while(Pred,T,RestFront,Back).
	take_while(_,Back,[],Back). % Pred fails -> rest is Back

- separate(not a replica): separate(+List, +Pred, -Yes, -No), which receives a list and a predicate, returning in Yes and No the elements X of List that make Pred(X) true or false, respectively.
	separate([],_,[],[]).
	separate([H|T],Pred,[H|RestYes],No):-call(Pred,H), !,separate(T,Pred,RestYes,No).
	separate([H|T],Pred,Yes,[H|RestNo]):-separate(T,Pred,Yes,RestNo).

# FINDALL,SETOF,BAGOF
Requires an accumulator to avoid infinite loops and collect results.

- Main: collect_all(Results) :- collector_rec([], Results).
- Rec:  collector_rec(Acc, Res) :- match_condition(X), \+ member(X, Acc), !, collector_rec([X|Acc], Res).
- Base: collector_rec(Acc, Acc).

# IS LIST
% Checks if the term is a proper list
my_is_list([]).
my_is_list([_|T]) :- my_is_list(T).

# MEMBER
my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

# MEMBERCHK: 
Deterministic (stops at the first match)
my_memberchk(X, [X|_]) :- !.
my_memberchk(X, [_|T]) :- my_memberchk(X, T).

# APPEND
my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

# LENGTH
my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.

# REVERSE
my_reverse(L, R) :- my_reverse_acc(L, [], R).
my_reverse_acc([], A, A).
my_reverse_acc([H|T], A, R) :- my_reverse_acc(T, [H|A], R).

# NTH1
my_nth1(1, [H|_], H) :- !.
my_nth1(N, [_|T], X) :- N > 1, N1 is N - 1, my_nth1(N1, T, X).

% NTH0: 0-based indexing
my_nth0(0, [H|_], H).
my_nth0(N, [_|T], E) :- N > 0, N1 is N - 1, my_nth0(N1, T, E).

% NTH0/4
Extract/Replace/Insert
my_nth0_4(0, [H|T], H, T).
my_nth0_4(N, [H|T], E, [H|Rest]) :- N > 0, N1 is N - 1, my_nth0_4(N1, T, E, Rest).

% FILTRAR (FILTER / INCLUDE)
% Mantém apenas elementos que satisfazem uma condição.
% Ex: my_filter(Lista, ListaPares).
my_filter([], []).
my_filter([H|T], [H|R]) :-
    % COLOCAR AQUI A CONDIÇÃO (Ex: H > 0, ou uma chamada a predicado)
    check_condition(H), !, 
    my_filter(T, R).
my_filter([_|T], R) :-
    my_filter(T, R).
% Predicado auxiliar genérico para o exemplo acima
check_condition(X) :- integer(X), X mod 2 =:= 0. % Exemplo: é par

% MAPEAR (MAP / TRANSFORM)
% Aplica uma transformação a todos os elementos.
% Ex: [1,2] -> [2,4].
my_map([], []).
my_map([H|T], [NewH|NewT]) :-
    % COLOCAR AQUI A TRANSFORMAÇÃO
    transform(H, NewH), 
    my_map(T, NewT).
transform(X, Y) :- Y is X * 2. % Exemplo: duplicar

# SELECT
Replaces first occurrence of X with Y
my_select(X, [X|T], Y, [Y|T]).
my_select(X, [H|T], Y, [H|R]) :- my_select(X, T, Y, R).

# DELETE
Remove all occurrences of Elem
my_delete([], _, []).
my_delete([E|T], E, R) :- !, my_delete(T, E, R).
my_delete([H|T], E, [H|R]) :- my_delete(T, E, R).

# LAST
Returns last element and the prefix
my_last([H|T], Last, Init) :- my_last_aux(T, H, Last, Init).
my_last_aux([], Last, Last, []).
my_last_aux([H|T], Prev, Last, [Prev|Rest]) :- my_last_aux(T, H, Last, Rest).

# SEGMENT
Contiguous subsequence
my_segment(List, Seg) :- my_append(_, Rest, List), my_append(Seg, _, Rest).

# SORT: Insertion sort that removes duplicates
my_sort([], []).
my_sort([H|T], Sorted) :- my_sort(T, ST), my_insert_unique(H, ST, Sorted).
my_insert_unique(X, [], [X]).
my_insert_unique(X, [X|T], [X|T]) :- !. % Duplicate removed
my_insert_unique(X, [H|T], [X,H|T]) :- X < H, !.
my_insert_unique(X, [H|T], [H|R]) :- X > H, my_insert_unique(X, T, R).

# TRANSPOSE: Matrix rows to columns
my_transpose([], []).
my_transpose([[]|_], []) :- !.
my_transpose(Mat, [Col|Rest]) :-
    extract_first_col(Mat, Col, RemainingMat),
    my_transpose(RemainingMat, Rest).

extract_first_col([], [], []).
extract_first_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- extract_first_col(Rows, Hs, Ts).

# PERMUTATION
my_permutation([], []).
my_permutation(L, [H|T]) :- my_select_any(H, L, Rest), my_permutation(Rest, T).
my_select_any(H, [H|T], T).
my_select_any(X, [H|T], [H|R]) :- my_select_any(X, T, R).

# ROTATE_LIST
my_rotate_list(Amount, List, Rotated) :-
    my_length(List, Len),
    Pos is Amount mod Len,
    my_append(Prefix, Suffix, List),
    my_length(Prefix, Pos), !,
    my_append(Suffix, Prefix, Rotated).

# SUMLIST
my_sumlist(L, S) :- my_sumlist_acc(L, 0, S).
my_sumlist_acc([], S, S).
my_sumlist_acc([H|T], Acc, S) :- NA is Acc + H, my_sumlist_acc(T, NA, S).

# MAX_MEMBER
my_max_member([H|T], Max) :- my_max_acc(T, H, Max).
my_max_acc([], M, M).
my_max_acc([H|T], CurM, Max) :- H > CurM, !, my_max_acc(T, H, Max).
my_max_acc([_|T], CurM, Max) :- my_max_acc(T, CurM, Max).

# EXCLUDE
Keeps elements where Pred fails
my_exclude(_, [], []).
my_exclude(P, [H|T], R) :- G =.. [P, H], call(G), !, my_exclude(P, T, R).
my_exclude(P, [H|T], [H|R]) :- my_exclude(P, T, R).

# SOME
True if at least one satisfies Pred
my_some(P, [H|_]) :- G =.. [P, H], call(G), !.
my_some(P, [_|T]) :- my_some(P, T).

# CUMLIST
Intermediate accumulation steps
my_cumlist(_, [], V, [V]).
my_cumlist(P, [H|T], V, [V|Rest]) :- 
    G =.. [P, V, H, NV], call(G), 
    my_cumlist(P, T, NV, Rest).
*/

% =============================================================================
% SET-LIKE LIST OPERATIONS
% =============================================================================

/*
## Set Operations
- Union: Combines two lists, avoiding duplicates.
    union([], L, L).
    union([H|T], L, R):-memberchk(H, L), !, union(T, L, R).
    union([H|T], L, [H|R]):-union(T, L, R).

- Intersection: Elements present in BOTH lists.
    intersect([], _, []).
    intersect([H|T], L, [H|R]):-memberchk(H, L), !, intersect(T, L, R).
    intersect([_|T], L, R):-intersect(T, L, R).

- Difference (L1 \ L2): Elements in L1 that are NOT in L2.
    diff([], _, []).
    diff([H|T], L, R):-memberchk(H, L), !, diff(T, L, R).
    diff([H|T], L, [H|R]):-diff(T, L, R).

## Membership & Subsets
- Subset: Checks if all elements of L1 are in L2.
    subset([], _).
    subset([H|T], L):-memberchk(H, L), subset(T, L).

- Disjoint: Succeeds if L1 and L2 share NO common elements.
    disjoint([], _).
    disjoint([H|T], L):-\+ memberchk(H, L), disjoint(T, L).

## Advanced Variants
- Symmetric Difference: Elements in either L1 or L2, but NOT both.
    sym_diff(L1, L2, R):-diff(L1, L2, D1), diff(L2, L1, D2), append(D1, D2, R).

- Accumulator Union (Tail Recursive): 
    union_tail(L1, L2, R):-union_aux(L1, L2, R).
    union_aux([], Acc, Acc).
    union_aux([H|T], Acc, R):-memberchk(H, Acc), !, union_aux(T, Acc, R).
    union_aux([H|T], Acc, R):-union_aux(T, [H|Acc], R).

## Dictionary using incomplete lists
lookup(Key, [ Key-Value | Dic ], Value).
lookup(Key, [ K-V | Dic ], Value):-
	Key \= K,
	lookup(Key, Dic, Value).
*/

% =============================================================================
% GENERAL BASE CONVERSIONS (DECIMAL <=> BASE 2-36)
% =============================================================================

/*
## Digits Reference
- Supports bases 2 through 36 using 0-9 and a-z.
base_digits([0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

## Decimal to Base N
- Uses successive division: N mod Base for the digit, N // Base for the next step.
- The accumulator [Digit|Acc] handles the reversal of digits automatically.

dec_to_base(0, _, [0]).
dec_to_base(N, B, Res):-
    N > 0, 
    dec_to_base_aux(N, B, [], Res).

dec_to_base_aux(0, _, Acc, Acc).
dec_to_base_aux(N, B, Acc, Res) :-
    N > 0,
    Rem is N mod B,
    NextN is N // B,
    base_digits(Digits),
    nth0(Rem, Digits, Digit),
    dec_to_base_aux(NextN, B, [Digit|Acc], Res).

## Base N to Decimal
- Uses polynomial expansion (Horner's Method): Acc * Base + DigitValue.
- Iterates through the list from most significant to least significant digit.

base_to_dec(Digits, B, N):-
    base_to_dec_aux(Digits, B, 0, N).

base_to_dec_aux([], _, Acc, Acc).
base_to_dec_aux([H|T], B, Acc, N) :-
    base_digits(Digits),
    nth0(Val, Digits, H),
    NewAcc is Acc * B + Val,
    base_to_dec_aux(T, B, NewAcc, N).

*/

% =============================================================================
% GRID, MATRIX & ADVANCED LIST PATTERNS
% =============================================================================

/*
## 2D Grid / Matrix Operations
Assumes Matrix is a list of lists. Indices are 1-based.

- Get Val: matrix_get(R, C, Mat, V):-nth1(R, Mat, Row), nth1(C, Row, V).
- Replace Val: matrix_set(R, C, Mat, V, New) :-
    nth1(R, Mat, OldRow, RestRows), nth1(C, OldRow, _, RestCols),
    nth1(C, NewRow, V, RestCols), nth1(R, New, NewRow, RestRows).
- Get Column: column(C, Mat, Col):-maplist(nth1(C), Mat, Col).

## Packing & Cumulative Math
- Pack Duplicates: [a,a,b,c,c] -> [[a,a],[b],[c,c]]
    pack([], []).
    pack([H|T], [[H|R]|Rest]):-take_p(H, T, R, S), pack(S, Rest).
    take_p(H, [H|T], [H|R], S):-!, take_p(H, T, R, S).
    take_p(_, S, [], S).

- Cumulative Sum: [1,2,3] -> [1,3,6]
    cum_sum(L, R):-cum_sum_aux(L, 0, R).
    cum_sum_aux([], _, []).
    cum_sum_aux([H|T], Acc, [NA|Rest]):-NA is Acc + H, cum_sum_aux(T, NA, Rest).

## Key-Value (Association) Lists
Useful for DB-style lookups without findall

- Get: kv_get(K, [K-V|_], V):-!. kv_get(K, [_|T], V):-kv_get(K, T, V).
- Update/Add:
    kv_upd(K, V, [], [K-V]).
    kv_upd(K, V, [K-_|T], [K-V|T]):-!.
    kv_upd(K, V, [H|T], [H|R]):-kv_upd(K, V, T, R).

## Custom Sorting (Schwartzian Transform)
- Sort by Attribute (e.g., sort sublists by length):
    sort_by_len(L, Sorted) :-
        maplist(add_len, L, Pairs), keysort(Pairs, SPairs), maplist(rem_len, SPairs, Sorted).
    add_len(L, Len-L):-length(L, Len).
    rem_len(_-L, L).
*/

% =============================================================================
% DIFFERENCE LISTS (DL) - O(1) PERFORMANCE
% =============================================================================

/*
Notation: L-T (L = Head, T = Uninstantiated Tail). 
Provides O(1) access to the end of the list.

% --- O(1) Append ---
% Standard append is O(N); DL append is O(1).
% Example: dl_append([a,b|T1]-T1, [c,d|T2]-T2, R). -> R = [a,b,c,d|T2]-T2.
dl_append(A-B, B-C, A-C).

% --- O(N) Flatten (Standard is O(N^2)) ---
% Builds the flat list without repeated traversals.
flatten_dl([], T-T).
flatten_dl([H|T], Res-Tail) :-
    is_list(H), !,
    flatten_dl(H, Res-Mid),
    flatten_dl(T, Mid-Tail).
flatten_dl([H|T], [H|Mid]-Tail) :-
    flatten_dl(T, Mid-Tail).

% Wrapper: flatten_opt(Nested, Flat) :- flatten_dl(Nested, Flat-[]).

% --- O(N) Tree to List (Fast) ---
% Converts tree to list in a single pass.
% Example: tree_to_dl(node(5, node(3,null,null), node(8,null,null)), L-[]).
tree_to_dl(null, T-T).
tree_to_dl(node(V, L, R), Res-Tail) :-
    tree_to_dl(L, Res-[V|Mid]),
    tree_to_dl(R, Mid-Tail).

% --- O(1) Queue Operations ---
% enqueue(+Elem, +OldQ, -NewQ)
enqueue(E, Head-[E|NewTail], Head-NewTail).

% dequeue(-Elem, +OldQ, -NewQ)
dequeue(E, [E|NewHead]-Tail, NewHead-Tail).

% --- Conversion Helpers ---
Normal List to DL: list_to_dl(L, L-[]).
DL to Normal List: dl_to_list(L-[], L).
*/

% =============================================================================
% ADVANCED GRAPH & GRID LOGIC
% =============================================================================

/*
## Node Degrees (Centrality)
- degree(N, D) :- findall(X, connected(N, X), L), length(L, D).
- most_conn(N) :- setof(D-Node, degree(Node, D), L), last(L, _-N).

## Matrix Rotations
- rotate_90_cw(M, R) :- reverse(M, Rev), transpose(Rev, R).

## Coordinate Labyrinth (State = X/Y)
- move_grid(X/Y, NX/Y, R, _) :- X < R, NX is X + 1, \+ wall(NX/Y).
- move_grid(X/Y, X/NY, _, C) :- Y < C, NY is Y + 1, \+ wall(X/NY).

- Grid Search (State = (X,Y)):
    move_grid((X,Y), (NX, Y)):-NX is X + 1, \+ wall(NX, Y).
    move_grid((X,Y), (X, NY)):-NY is Y + 1, \+ wall(X, NY). % add X-1, Y-1 for full movement
*/

% =============================================================================
% COMBINATORIAL SEARCH
% =============================================================================

/*
## Picking K elements (Combinations)
- pick_k(0, _, []).
- pick_k(K, [H|T], [H|R]) :- K > 0, K1 is K - 1, pick_k(K1, T, R).
- pick_k(K, [_|T], R) :- K > 0, pick_k(K, T, R).

## Power Set (All Subsets)
- subsets([], [[]]).
- subsets([H|T], S) :- subsets(T, R), maplist(add_to_front(H), R, WH), append(R, WH, S).
- add_to_front(H, L, [H|L]).
*/

% =============================================================================
% EXTRA CONSTRAINTS - PROJECT
% =============================================================================

/*
% DISTANCE-BASED CONSTRAINTS

%% two_spaces(+X, +Y, +Board)
%  Succeeds if X and Y are exactly two spaces apart on the Board (positions like A-D, B-E, C-F).
%  Succeeds trivially if X = Y.
two_spaces(X, X, _).
two_spaces(X, Y, Board) :- append(_Prefix, [X, _, _, Y | _Suffix], Board).
two_spaces(X, Y, Board) :- append(_Prefix, [Y, _, _, X | _Suffix], Board).

%% not_next_to(+X, +Y, +Board)
%  Succeeds if X is NOT adjacent to Y on the Board.
%  Succeeds trivially if X = Y.
not_next_to(X, X, _).
not_next_to(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    Distance is abs(Ix - Iy),
    Distance > 1.

%% at_least_n_away(+X, +Y, +N, +Board)
%  Succeeds if X is at least N positions away from Y.
%  Succeeds trivially if X = Y and N = 0.
at_least_n_away(X, X, 0, _).
at_least_n_away(X, Y, N, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    Distance is abs(Ix - Iy),
    Distance >= N.

% POSITION-SPECIFIC CONSTRAINTS

%% at_position(+X, +Pos, +Board)
%  Succeeds if X is at the exact position Pos on the Board.
at_position(X, Pos, Board) :-
    nth1(Pos, Board, X).

%% not_at_position(+X, +Pos, +Board)
%  Succeeds if X is NOT at position Pos on the Board.
not_at_position(X, Pos, Board) :-
    nth1(Pos, Board, Color),
    X \= Color.

%% first_position(+X, +Board)
%  Succeeds if X is at position 1 (leftmost position).
first_position(X, Board) :-
    nth1(1, Board, X).

%% last_position(+X, +Board)
%  Succeeds if X is at position 6 (rightmost position).
last_position(X, Board) :-
    nth1(6, Board, X).

%% middle_positions(+X, +Board)
%  Succeeds if X is in the center area (positions 3 or 4).
middle_positions(X, Board) :-
    nth1(Ix, Board, X),
    member(Ix, [3, 4]).

%% corner_position(+X, +Board)
%  Succeeds if X is at a corner/extreme position (1 or 6).
corner_position(X, Board) :-
    nth1(Ix, Board, X),
    member(Ix, [1, 6]).

% RELATIVE ORDERING CONSTRAINTS

%% left_of(+X, +Y, +Board)
%  Succeeds if X is to the left of Y (lower position number).
%  Succeeds trivially if X = Y.
left_of(X, X, _).
left_of(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    Ix < Iy.

%% right_of(+X, +Y, +Board)
%  Succeeds if X is to the right of Y (higher position number).
%  Succeeds trivially if X = Y.
right_of(X, X, _).
right_of(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    Ix > Iy.

%% between(+X, +Y, +Z, +Board)
%  Succeeds if X is positioned between Y and Z.
%  Succeeds trivially if X = Y or X = Z.
between(X, Y, _, Board) :- X = Y, member(X, Board).
between(X, _, Z, Board) :- X = Z, member(X, Board).
between(X, Y, Z, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    nth1(Iz, Board, Z),
    X \= Y,
    X \= Z,
    ((Iy < Ix, Ix < Iz) ; (Iz < Ix, Ix < Iy)).

% EDGE AND GROUP CONSTRAINTS

%% different_edges(+X, +Y, +Board)
%  Succeeds if X and Y are on different edges.
%  Fails trivially if X = Y.
different_edges(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    \+ ((member(Ix, [1,2]), member(Iy, [1,2])) ;
        (member(Ix, [4,5,6]), member(Iy, [4,5,6]))).

%% isolated(+X, +Board)
%  Succeeds if X is alone on its edge (no other colors on the same edge).
%  This means X is at position 3 (the bridge), which is always isolated.
%  Note: For edges [1,2] or [4,5,6], being "alone" means all other positions on that edge contain X itself.
isolated(X, Board) :-
    nth1(Ix, Board, X),
    Ix = 3.  % Position 3 is always isolated (bridge between edges)

% MULTIPLE TOKEN CONSTRAINTS

%% adjacent_to_any(+X, +List, +Board)
%  Succeeds if X is adjacent to at least one color from List.
adjacent_to_any(X, List, Board) :-
    nth1(Ix, Board, X),
    member(Y, List),
    nth1(Iy, Board, Y),
    X \= Y,
    Distance is abs(Ix - Iy),
    Distance =:= 1.

%% adjacent_to_all(+X, +List, +Board)
%  Succeeds if X is adjacent to all colors in List.
%  Only realistically possible for small lists (1-2 colors) given the board size.
adjacent_to_all(_, [], _).
adjacent_to_all(X, [Y|Rest], Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    Distance is abs(Ix - Iy),
    Distance =:= 1,
    adjacent_to_all(X, Rest, Board).

%% not_adjacent_to_any(+X, +List, +Board)
%  Succeeds if X is NOT adjacent to any color from List.
not_adjacent_to_any(X, List, Board) :-
    nth1(Ix, Board, X),
    \+ (
        member(Y, List),
        nth1(Iy, Board, Y),
        X \= Y,
        Distance is abs(Ix - Iy),
        Distance =:= 1
    ).

%% across_from_any(+X, +List, +Board)
%  Succeeds if X is across from at least one color in List.
across_from_any(X, List, Board) :-
    nth1(Ix, Board, X),
    member(Y, List),
    nth1(Iy, Board, Y),
    X \= Y,
    is_across(Ix, Iy).

% DIAGONAL/ACROSS VARIATIONS

%% not_across(+X, +Y, +Board)
%  Succeeds if X and Y are NOT across from each other.
%  Succeeds trivially if X = Y.
not_across(X, X, _).
not_across(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    \+ is_across(Ix, Iy).

% SEQUENTIAL/PATTERN CONSTRAINTS

%% immediate_left(+X, +Y, +Board)
%  Succeeds if X is immediately to the left of Y (consecutive, X before Y).
immediate_left(X, Y, Board) :-
    append(_Prefix, [X, Y | _Suffix], Board),
    X \= Y.

%% immediate_right(+X, +Y, +Board)
%  Succeeds if X is immediately to the right of Y (consecutive, Y before X).
immediate_right(X, Y, Board) :-
    append(_Prefix, [Y, X | _Suffix], Board),
    X \= Y.

%% sequence(+List, +Board)
%  Succeeds if colors in List appear in that exact order on Board (can have gaps).
sequence([], _).
sequence([X], Board) :-
    member(X, Board).
sequence([X, Y | Rest], Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    Ix < Iy,
    sequence([Y | Rest], Board).

%% consecutive_sequence(+List, +Board)
%  Succeeds if colors in List appear consecutively in that exact order on Board.
consecutive_sequence(List, Board) :-
    append(_Prefix, Suffix, Board),
    append(List, _Rest, Suffix).

% EXCLUSION/NEGATIVE CONSTRAINTS

%% none_in_positions(+X, +PosList, +Board)
%  Succeeds if X is NOT in any position from PosList.
none_in_positions(X, PosList, Board) :-
    nth1(Ix, Board, X),
    \+ member(Ix, PosList).

%% mutually_exclusive_edges(+X, +Y, +Board)
%  Succeeds if X is on edge [1,2] then Y must be on edge [4,5,6], and vice versa.
%  Fails if both are on the same edge or if either is on the bridge (position 3).
mutually_exclusive_edges(X, Y, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    (
        (member(Ix, [1,2]), member(Iy, [4,5,6])) ;
        (member(Ix, [4,5,6]), member(Iy, [1,2]))
    ).

% CONDITIONAL CONSTRAINTS

%% if_position_then_neighbor(+X, +Pos, +Y, +Board)
%  Succeeds if X is at position Pos, then Y must be its neighbor.
%  If X is not at Pos, the constraint is trivially satisfied.
if_position_then_neighbor(X, Pos, Y, Board) :-
    nth1(Ix, Board, X),
    (
        Ix = Pos ->
        (
            nth1(Iy, Board, Y),
            Distance is abs(Ix - Iy),
            Distance =:= 1
        ) ;
        true
    ).

%% only_if_across(+X, +Y, +Z, +Board)
%  Succeeds if X can only be across from Y if Z is next to X.
%  If X is across from Y, then Z must be next to X.
%  If X is not across from Y, the constraint is trivially satisfied.
only_if_across(X, Y, Z, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    is_across_helper(Ix, Iy) ->
    (
        % X is across from Y, so Z must be next to X
        nth1(Iz, Board, Z),
        Distance is abs(Ix - Iz),
        Distance =:= 1
    ) ;
    true.  % X is not across from Y, so constraint is satisfied

%% is_across_helper(+P1, +P2)
%  Helper predicate: Succeeds if position P1 is across from position P2.
is_across_helper(P1, P2) :-
    member(P1, [1,2]),
    member(P2, [4,5,6]).
is_across_helper(P1, P2) :-
    member(P1, [4,5,6]),
    member(P2, [1,2]).

% COUNTING CONSTRAINTS

%% exactly_n_neighbors(+X, +N, +Board)
%  Succeeds if X has exactly N adjacent positions occupied.
%  Neighbors are positions that are adjacent (distance = 1).
exactly_n_neighbors(X, N, Board) :-
    nth1(Ix, Board, X),
    findall(Y, (
        nth1(Iy, Board, Y),
        Y \= X,
        Distance is abs(Ix - Iy),
        Distance =:= 1
    ), Neighbors),
    length(Neighbors, N).

%% max_distance(+X, +Y, +MaxDist, +Board)
%  Succeeds if the distance between X and Y is at most MaxDist.
%  Succeeds trivially if X = Y and MaxDist >= 0.
max_distance(X, X, MaxDist, _) :-
    MaxDist >= 0.
max_distance(X, Y, MaxDist, Board) :-
    nth1(Ix, Board, X),
    nth1(Iy, Board, Y),
    X \= Y,
    Distance is abs(Ix - Iy),
    Distance =< MaxDist.

% SET-BASED CONSTRAINTS

%% is_consecutive_range(+List)
%  Succeeds if List contains consecutive integers.
is_consecutive_range([_]).
is_consecutive_range([X, Y | Rest]) :-
    Y =:= X + 1,
    is_consecutive_range([Y | Rest]).

%% pairwise_not_adjacent(+List, +Board)
%  Succeeds if no two colors from List are adjacent to each other.
pairwise_not_adjacent([], _).
pairwise_not_adjacent([H | T], Board) :-
    \+ (
        member(Other, T),
        nth1(Ih, Board, H),
        nth1(Io, Board, Other),
        Distance is abs(Ih - Io),
        Distance =:= 1
    ),
    pairwise_not_adjacent(T, Board).
*/

% =============================================================================
% PART 2 DIF POINTS - PROJECT
% =============================================================================

/*
In Part 2, your calculate_score/3 treats every failed constraint as a -1 penalty. 
Task: The game designers want to introduce a "Critical Constraint." 
Write a specialized version of the scoring clause specifically for the across(X, Y) constraint. 
If an across constraint is violated, it should subtract 5 points instead of 1. All other violations remain -1.

% best_score(+Constraints, -Score)
% Finds the maximum possible score
best_score_mod(Constraints, Score) :-
    colors(Colors),
    findall(CurScore, (
        permutation(Colors, Board),
        calculate_score_mod(Constraints, Board, CurScore)
    ), AllScores),
    max_list(AllScores, Score).

% calculate_score(+Constraints, +Board, -Score)
% Base case: score starts at 0
calculate_score_mod([], _Board, 0).

% Success case: Constraint satisfied (0 points)
calculate_score_mod([C|T], Board, Score) :-
    call(C, Board), !,
    calculate_score_mod(T, Board, RemainingScore),
    Score is 0 + RemainingScore.

% Failure case: Constraint ACROSS violated (-5 point)
calculate_score_mod([across(X,Y)|T], Board, Score) :-
	\+ across(X,Y,Board), !,
    calculate_score_mod(T, Board, RemainingScore),
    Score is -5 + RemainingScore.

% Failure case: other constraint violated (-1 point)
calculate_score_mod([_|T], Board, Score) :-
    calculate_score_mod(T, Board, RemainingScore),
    Score is -1 + RemainingScore.
*/

/*
The real game has "Hard" and "Easy" cards.
Task: Modify calculate_score/3 to accept a weighted list of constraints, such as [hard(across(blue, green)), easy(next_to(white, orange))].
and subtract -3 for a hard failure and -1 for an easy one.

% Success case (remains 0)
calculate_score_weighted([C|T], Board, Score) :-
    (C = hard(Goal) ; C = easy(Goal)),
    call(Goal, Board), !,
    calculate_score_weighted(T, Board, RemainingScore),
    Score is 0 + RemainingScore.

% Failure: Hard constraint
calculate_score_weighted([hard(Goal)|T], Board, Score) :-
    \+ call(Goal, Board), !,
    calculate_score_weighted(T, Board, RemainingScore),
    Score is -3 + RemainingScore.

% Failure: Easy constraint
calculate_score_weighted([easy(Goal)|T], Board, Score) :-
    \+ call(Goal, Board), !,
    calculate_score_weighted(T, Board, RemainingScore),
    Score is -1 + RemainingScore.

calculate_score_weighted([], _, 0).
*/