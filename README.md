# Y3S1 - PFL (Functional and Logical Programming)

## Overview

This repository contains solutions and exercises for the PFL (Functional and Logical Programming) course, Year 3 Semester 1.

## Repository Structure

The repository is organized into two main folders:

- **functional programming/** — Contains all **Haskell** exercises, organized by worksheet and project:

  - `worksheet N/` — Each directory contains Haskell source files (e.g., `N_M.hs`) corresponding to worksheet exercises.
  - `project 1/` — Small Haskell project for coursework, including a calculator and parser.
  - `extras/` — Additional problems, reworked exercises, and project drafts.
  - Some worksheet folders include a README listing the most challenging exercises.

- **logic programming/** — Contains **Prolog** exercises, organized by worksheet:
  - `worksheet N/` — Each directory contains Prolog source files (e.g., `N.pl`) for the respective worksheet.
  - This section is still in progress and may be updated further.

## How to Run the Haskell Exercises

**Prerequisites:**

- Install the Glasgow Haskell Compiler (GHC) and GHCi. On Debian/Ubuntu:

  ```bash
  sudo apt install ghc ghc-doc ghci
  ```

**Quick methods to run or test a file:**

- Load a file interactively in GHCi:

  ```bash
  ghci "functional programming/worksheet 1/1_1.hs"
  # then in the GHCi prompt call functions defined in the file
  ```

- Compile a file with GHC:

  ```bash
  ghc -o bin/1_1 "functional programming/worksheet 1/1_1.hs"
  ./bin/1_1
  ```

## How to Run the Prolog Exercises

**Prerequisites:**

- Install SICStus Prolog. (See your institution's instructions or https://sicstus.sics.se/ for details.)

**To run a Prolog file:**

- Start SICStus in the terminal:

  ```bash
  sicstus
  ```

- At the SICStus prompt, load a worksheet file (example for worksheet 1, exercise 1):

  ```prolog
  | ?- ["logic programming/worksheet 1/1ab.pl"].
  ```

  or
  ```prolog
  | ?- consult('logic programming/worksheet 1/1ab.pl').
  ```

  and 
  ```prolog
  | ?- reconsult('logic programming/worksheet 1/1ab.pl').
  ```
  to update the file

- Then you can call predicates defined in the file directly at the prompt.

## Project 1 (Haskell)

Located in `functional programming/project 1/`:

- `Calculator.hs` — Main calculator implementation (can be loaded or compiled)
- `Parsing.hs` — Parsing helpers used by the calculator

To run the project:

```bash
ghci "functional programming/project 1/Calculator.hs"
```

## Conventions and Tips

- Filename conventions: `sheet_exercise.hs` (Haskell), `N.pl` (Prolog)
- For running many exercises, consider creating a small driver file (e.g., `Main.hs`) that imports selected modules and exposes test runs.

---

_These exercises were done solely by Francisca Portugal from September 2025 to January 2026._
