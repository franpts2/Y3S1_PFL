# Prolog Meta-Programming, Operators & Models Cheatsheet

## 1. Meta-logical Predicates (Type Checking)

Used to check the type or instantiation state of a term.

| Predicate     | Description                                                                         |
| :------------ | :---------------------------------------------------------------------------------- |
| `var(A)`      | True if `A` is an uninstantiated variable.                            |
| `nonvar(A)`   | True if `A` is NOT a variable (atom, number, or compound).            |
| `ground(A)`   | True if `A` contains no uninstantiated variables in any substructure. |
| `atom(A)`     | True if `A` is a Prolog atom.                                         |
| `number(A)`   | True if `A` is an integer or a float.                                 |
| `atomic(A)`   | True if `A` is an atom or a number.                                   |
| `compound(A)` | True if `A` is a compound term (e.g., `f(x)`).                        |

---

## 2. Term Manipulation & Meta-Programming

Predicates to decompose or construct terms dynamically.

### Key Predicates

- **`functor(Term, Name, Arity)`**: Gets or sets the name and arity of a term.
- **`arg(Index, Term, Arg)`**: Accesses the Nth argument of a term (starts at index 1).
- **`Term =.. List` (Univ)**: Converts between a term and a list (e.g., `f(a,b) =.. [f, a, b]`).
- **`call(Goal, ...)`**: Executes a goal. Can be used for higher-order programming.

### Multi-Directional `sum/3` Example

Handles addition or subtraction based on which arguments are instantiated.

```prolog
sum(A, B, S):- number(A), number(B), !, S is A + B.
sum(A, B, S):- number(A), number(S), !, B is S - A.
sum(A, B, S):- number(B), number(S), !, A is S - B.
```

### `map` implementattion

```prolog
map(_,[]).
map(P,[H|T]):-
	G =.. [P,H],
	G,
	map(P,T).
```

---

## 3. Operators

Operators are defined by **Precedence** (1-1200, lower is higher priority) and **Associativity**.

Defining Operators: `op(+Prec, +Type, +Name)`

- **Types**:
    - Infix: `xfx` (non-assoc), `xfy` (right-assoc), `yfx` (left-assoc).

    - Prefix: `fx`, `fy`.

    - Postfix: `xf`, `yf`.

---

## 4. Computational Model Emulations

Prolog can emulate various theoretical machines by treating the state transition as a recursive search.

### Deterministic Finite Automata (DFA)

```prolog
accept(Str):- initial(State), accept(Str, State).

accept([], State):- final(State).
accept([S|Ss], State):-
    delta(State, S, NState),
    accept(Ss, NState).

```

### Pushdown Automata (PDA)

```prolog
accept(Str):- initial(State), accept(Str, State, []).

accept([], State, []):- final(State).
accept([S|Ss], State, Stack):-
    delta(State, S, Stack, NewState, NewStack),
    accept(Ss, NewState, NewStack).

```

### Definite Clause Grammars (DCG)

DCGs provide a simplified syntax for Context-Free Grammars.

```prolog
% Palindrome DCG
pal --> [].
pal --> [_].
pal --> [S], pal, [S].

% Usage: phrase(pal, "madamimadam").

```

---

## 5. Incomplete Data Structures

Efficiency can be increased by leaving a free variable at the end of a structure to allow "in-place" updates.

### Difference Lists

Allows list concatenation by unifying the tail variable.

```prolog
% Appending X\Y and Y\W results in X\W
append_dl(X\Y, Y\W, X\W).

```

---

## 6. Statistics & Performance

Obtain execution data using `statistics/0` (print all) or `statistics/2` (keyword based).

---

## 7. Notable Libraries (SICStus)

- **`aggregate`**: SQL-like queries (count, sum, min, max).

- **`clpfd`**: Constraint Logic Programming over Finite Domains (ideal for puzzles/optimization).