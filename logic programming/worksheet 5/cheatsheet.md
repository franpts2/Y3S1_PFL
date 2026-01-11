# Prolog: Collecting Solutions, Graphs, and Trees Cheatsheet

## 1. Collecting Solutions
Prolog provides three main predicates to collect all solutions to a query into a list instead of finding them one by one.

| Predicate | Description | Behavior if No Solutions |
| :--- | :--- | :--- |
| `findall(?Term, :Goal, -List)` | Collects all instances of `Term` for which `Goal` is true. | Returns an empty list `[]`. |
| `bagof(?Term, :Goal, -List)` | Groups results by uninstantiated variables in `Goal`. | Fails. |
| `setof(?Term, :Goal, -List)` | Like `bagof`, but results are **ordered** and **duplicates are removed**. | Fails. |

### Advanced Collection Features
* **Conjunctive Goals:** Use parentheses for multiple goals: `findall(C, (parent(P, C), female(C)), L)`.
* **Compound Terms:** Collect multiple variables as a pair: `findall(P-C, parent(P, C), L)`.
* **Existential Quantifier (`^`):** Used in `bagof` or `setof` to ignore specific variables for grouping: `bagof(Child, Parent^parent(Parent, Child), List)`.

---

## 2. Graphs and Search
Graphs are represented as a set of `connected(Node1, Node2)` facts.

### Depth-First Search (DFS) with Cycle Prevention
To avoid infinite loops in graphs with cycles, use an accumulator to track visited nodes.

```prolog
% connects_dfs(+Start, +Finish, +Visited, -Path)
connects_dfs(S, F) :- connects_dfs(S, F, [S]).

connects_dfs(F, F, _).
connects_dfs(S, F, T) :-
    connected(S, N),
    not(memberchk(N, T)),
    connects_dfs(N, F, [N|T]).
```

### Breadth-First Search (BFS)

Uses findall to expand all neighbors at the current level.

``` Prolog
connects_bfs(S, F) :- connects_bfs([S], F, []).

connects_bfs([F|_], F, _).
connects_bfs([S|R], F, V) :-
    findall(N, (connected(S, N), not(memberchk(N, V)), not(memberchk(N, [S|R]))), L),
    append(R, L, NR),
    connects_bfs(NR, F, [S|V]).
```

## 3. Binary Trees

Trees are defined as node(Value, Left, Right) or null.

### Core Tree Predicates

- **Member**:
    ``` prolog
    tree_member(Val, node(Val, _L, _R) ).

    tree_member(Val, node(V, L, _R) ):-
	    [Val < V,] tree_member(Val, L).

    tree_member(Val, node(V, _L, R) ):-
	    [Val > V,] tree_member(Val, R).
    ```

- **List all members**
    ``` prolog
    tree_list( null, [] ).

    tree_list( node(Val, L, R), List ):-
	    tree_list(L, Left),
	    tree_list(R, Right),
	    append(Left, [Val|Right], List).
    ```

- **Ordered Check**:
    ``` Prolog
    tree_is_ordered(Tree) :-
        tree_list(Tree, List),
        sort(List, List).
    ```

- **Insert Element**:
    ``` prolog
    tree_insert( null, V, node(V, null, null) ).

    tree_insert( node(V, L, R), V, node(V, L, R) ).

    tree_insert( node(V, L, R), Val, node(V, NL, R) ):-
    	Val < V, tree_insert( L, Val, NL).

    tree_insert( node(V, L, R), Val, node(V, L, NR) ):-
	    Val > V, tree_insert( R, Val, NR).
    ```

- **Height**:
    ``` Prolog
    tree_height(null, 0).
    tree_height(node(_, L, R), H) :-
        tree_height(L, HL), tree_height(R, HR),
        H is 1 + max(HL, HR).

- **Balanced Check**: A tree is balanced if the height difference between subtrees is ≤1.
    ``` prolog
    tree_is_balanced(null).

    tree_is_balanced(node(Val, L, R)):-
	    tree_is_balanced(L),
	    tree_is_balanced(R),
	    tree_height(L, HL),
	    tree_height(R, HR),
    	abs(HL-HR) =< 1.
    ```

## 4. Generic Puzzle Solver

A template for solving one-person games.

``` Prolog
initial(InitialState).

final(State):- winning_condition(State).

move(OldState, NewState):- valid_move(OldState,NewState).

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
```

### Shortest Path (using setof)

To find the shortest path, collect all paths and sort them by length.

``` Prolog
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
```
