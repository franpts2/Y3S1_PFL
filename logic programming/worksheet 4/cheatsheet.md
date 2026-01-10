# Cut, I/O, and Useful Predicates Cheatsheet

## 1. The Cut Operator (`!`)

- **Purpose**: Prunes the search tree to stop backtracking and improve efficiency.

- **Mechanism**: Freezes all choices made since the parent goal unified with the current clause.

- **Scope**: Prunes alternative clauses for the same predicate and alternative solutions for goals to the left of the cut.

<br>

- **Red Cut**: Changes the logic/results of the program; removing it changes the output.

- **Green Cut**: Only improves efficiency; removing it does not change the set of solutions.

---

## 2. Negation and Conditionals

- **Negation as Failure (NAF)**: `not(X) :- X, !, fail. not(_X).`

  - Returns **yes** if `X` cannot be proven, **no** if `X` succeeds.

  - Requires **ground terms** (no variables) to work reliably.

- **If-Then-Else (via Cut)**:

  ```prolog
  if_then_else(If, Then, Else) :- If, !, Then.
  if_then_else(If, Then, Else) :- Else.
  ```

- **If-Then-Else (via NAF)**:
  ```prolog
  ite(If, Then, Else) :- If, Then.
  ite(If, Then, Else) :- not(If), Else.
  ```

---

## 3. Input / Output (I/O)

- **Nature**: Stream-based and produces side effects that are **not** undone by backtracking.

- **Term I/O**:

  - `read(X)`: Reads a term (must end with a period `.`).

  - `write(X)`: Prints a term.

  - `format(String, Args)`: Formatted output (similar to printf).

    - `~w`: Writes the term as if using write/1.

    - `~d`: Formats an integer as a decimal.

    - `~n`: Outputs a new line (equivalent to nl).

    - `~a`: Prints an atom without quotes.

    - `~f`: Prints a floating-point number.

- **Character I/O**:

  - `get_char(C)` / `put_char(C)`: Handle single characters.

  - `get_code(N)` / `put_code(N)`: Handle ASCII codes.

  - `peek_char(C)` / `peek_code(N)`: Look at the next input without consuming it.

- **Stream Control**:

  - `nl`: Prints a newline.

  - `skip_line`: Clears the current input buffer until the next newline.

- **File I/O**:

  - `see(File)` / `seen`: Redirect input from a file / Close file.

  - `tell(File)` / `told`: Redirect output to a file / Close file.

---

## 4. Useful Built-in Predicates

- **`repeat`**: Always succeeds; used to create infinite loops for user input until a condition is met.

- **`between(L, U, X)`**: Generates or tests if `X` is an integer between `L` and `U`.

- **`library(random)`**:

  - `random(L, U, V)`: Generates a random number in a range.

  - `random_member(X, List)`: Selects a random element.

  - `random_permutation(L1, L2)`: Randomly shuffles a list.

---

## 5. Code Management

- **`consult(File)`** or **`[File]`**: Loads a Prolog source file.

- **`use_module(library(Name))`**: Loads a specific library (e.g., `lists`, `random`, `between`).

- **`ensure_loaded(File)`**: Loads a file only if it hasn't been loaded in the current session.

- **`include(File)`**: Directly inserts text from another file during compilation.
