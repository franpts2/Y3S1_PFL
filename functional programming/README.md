# Y3S1 - PFL (Functional and Logical Programming)

## Overview

This repository contains exercise sheets and Haskell solutions for the PFL (Functional and Logical Programming) course, Year 3 Semester 2.

## Repository structure

This repository is organised by worksheet: each `worksheet N/` directory contains Haskell exercise source files (named like `N_M.hs`), whose names inherently relate to the exercise numbers in the worksheet PDF (on that same folder).

Some worksheet folders include a README listing the exercises I found most challenging; these subjective notes highlight areas I focused on improving.

## How to run the exercises

Prerequisites:

- Install the Glasgow Haskell Compiler (GHC) and GHCi. On Debian/Ubuntu you can install with:

```bash
sudo apt install ghc ghc-doc ghci
```

Quick methods to run or test a file:

- Load a file interactively in GHCi:

```bash
ghci worksheet\ 1/1_1.hs
# then in the GHCi prompt call functions defined in the file
```

- Compile a file with GHC:

```bash
ghc -o bin/1_1 worksheet\ 1/1_1.hs
./bin/1_1
```

## Project 1

There is a small project in the `project 1/` folder used for coursework.

- `project 1/Calculator.hs` — main calculator implementation (can be loaded or compiled).
- `project 1/Parsing.hs` — parsing helpers used by the calculator.

To run the project:

```bash
ghci project\ 1/Calculator.hs
```

## Conventions and tips

- Filename conventions: `sheet_exercise.hs`
- If you want to run many exercises, consider creating a small driver file (e.g., `Main.hs`) that imports selected modules and exposes test runs.

---

_This exercises were done solely by Francisca Portugal on September to December 2025_
