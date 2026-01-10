/*
immature(X):- adult(X), !, fail.
immature(_X).
- red
justification:
	This cut implements Negation as Failure. 
	If adult(X) succeeds, the cut "freezes" that choice and the fail predicate causes the whole goal to fail.
	If you removed this cut, Prolog would backtrack to the second clause (immature(_X).) and mistakenly succeed even for adults. 
	Because removing it changes the logical result, it is red.

adult(X):- person(X), !, age(X, N), N >=18.
adult(X):- turtle(X), !, age(X, N), N >=50.
adult(X):- spider(X), !, age(X, N), N>=1.
adult(X):- bat(X), !, age(X, N), N >=5.
- red
justification:
	These cuts are used to make the categories mutually exclusive. 
	Once Prolog identifies that X is a person(X), the cut ensures it will never try to check if X is a  turtle(X), spider(X), or bat(X). 
	If you removed these cuts and a term happened to satisfy multiple categories 
	(e.g., a mythological being that is both a person and a spider), Prolog could return multiple conflicting answers for adult(X). 
	Since the cut forces a single specific path and prevents others from being explored, it influences the result and is red.
*/