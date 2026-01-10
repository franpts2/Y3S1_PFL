# Prolog Lists Cheatsheet

## 1. Syntax and Internal Representation

- `[]`: The empty list representation.

- `[a, b, c]`: Elements are separated by commas within square brackets.

- `[H|T]`: List constructor where `H` is the head and `T` is the tail (remaining elements).

- `.(Head, Tail)`: Internal representation using the `.` functor; e.g., `[1,2,3] = .(1, .(2, .(3, [])))`.

- `Strings`: Represented as lists of character ASCII codes (e.g., `"Hello"` is `[72, 101, 108, 108, 111]`).

- `:-use_module(library(lists)).`: to use the lists library

---

## 2. Fundamental Built-in Predicates

- `is_list(?Term)`: Succeeds if the term is an empty list or a list construct where the tail is also a list.

<br>

- `length(?List, ?Size)`: Relates a list to its number of elements; can calculate length, verify it, or generate a list of a specific size.

<br>

- `member(?Elem, ?List)`: Non-deterministic membership; finds or verifies all matches in a list through backtracking.

- `memberchk(?Elem, ?List)`: Deterministic membership; stops after the first match and does not backtrack.

<br>

- `append(?L1, ?L2, ?L3)`: Appends two lists into a third; highly flexible for joining or decomposing lists.

<br>

- `reverse(?List, ?Reversed)`: Reverses the order of elements in a list.

---

## 3. List Selection and Sub-lists

- `nth0(?Index, ?List, ?Elem)`: Accesses an element using 0-based indexing.

- `nth1(?Index, ?List, ?Elem)`: Accesses an element using 1-based indexing.

- `nth0(?Index, ?List, ?Elem, ?Rest)`: Retrieves/replaces an element and identifies the remaining `Rest` list.

<br>

- `select(?X, ?XList, ?Y, ?YList)`: Finds an occurrence of `X` in `XList`, replaces it with `Y`, and produces `YList`.

<br>

- `delete(+List, +ToDel, -Result)`: Deletes all occurrences of a specific element from a list.

- `delete(+List, +ToDel, +Count, -Result)`: Deletes a specific `Count` of occurrences of an element.

<br>

- `last(?Init, ?Last, ?List)`: Identifies the `Last` element and the `Init` prefix (the rest of the list).

<br>

- `segment(?List, ?Segment)`: Succeeds when `Segment` is a contiguous subsequence of `List`.

- `sublist(+List, ?Part, ?Before, ?Length, ?After)`: Extracts a contiguous `Part` of a list with a specific `Length` and context.

---

## 4. Sorting and Transformations

- `sort(+List, -SortedList)`: Sorts a proper list and removes duplicate elements.

- `keysort(+PairList, -SortedList)`: Sorts a list of `Key-Value` pairs by the key; original order is retained for identical keys.

<br>

- `transpose(?Matrix, ?Transposed)`: Converts matrix rows into columns and vice-versa.

<br>

- `remove_dups(+List, ?PrunedList)`: Removes duplicate elements from a list.

<br>

- `permutation(?List, ?Permutation)`: Generates all possible permutations of a list through backtracking.

<br>

- `rotate_list(+Amount, ?List, ?Rotated)`: Cyclically shifts a list by a specified number of positions.

<br>

- `append(+ListOfLists, -List)`: Concatenates a list of lists into a single list.

---

## 5. Math and Aggregation

- `sumlist(+ListOfNumbers, ?Sum)`: Calculates the total sum of all numbers in a list.

<br>

- `max_member(?Max, +List)`: Identifies the largest element in the list.

- `min_member(?Min, +List)`: Identifies the smallest element in the list.

- `max_member(:Comp, ?Max, +List)`: Finds the maximum element using a custom comparison predicate.

- `min_member(:Comp, ?Min, +List)`: Finds the minimum element using a custom comparison predicate.

---

## 6. Higher-Order Predicates

- `maplist(:Pred, +List)`: Succeeds if the predicate succeeds for every element in the list.

- `maplist(:Pred, +L1, ?L2)`: Applies a predicate to transform each element of `L1` into `L2`.

- `map_product(:Pred, +Xs, +Ys, ?List)`: Applies a predicate to the Cartesian product of two lists.

<br>

- `scanlist(:Pred, +List, ?Start, ?Final)`: Performs a `foldl` operation to reduce a list to a final value.

<br>

- `cumlist(:Pred, +List, ?Start, ?ResultList)`: Similar to `scanlist` but provides a list of all intermediate accumulation steps.

<br>

- `some(:Pred, +List)`: Succeeds if at least one element in the list satisfies the predicate.

<br>

- `include(:Pred, +List, ?Filtered)`: Filters a list to keep only elements that satisfy the predicate.

- `exclude(:Pred, +List, ?Filtered)`: Filters a list to remove elements that satisfy the predicate.

<br>

- `group(:Pred, +List, ?Front, ?Back)`: Splits a list into a `Front` group where the predicate succeeds and a `Back` remainder.
