% a)
gcd(X,0,X).

gcd(X,Y,G):-
	Y > 0,
	Z is X mod Y,
	gcd(Y,Z,G).

% b)
lcm(X,Y,M):-
	Z is X*Y,
	gcd(X,Y,G),
	M is Z div G. /* integer division */