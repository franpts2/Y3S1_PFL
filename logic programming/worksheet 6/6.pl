% a)
:- op(550, fx, flight).
:- op(580, xfx, from).
:- op(570, xfx, to).
:- op(570, xfx, at).
:- op(550, xfy, :).

/*
		  to
        /      \
     from        at
    /   \     /      \
flight porto  lisbon  :
   \               /     \
tp1949            16      15
*/

% b)
:- op(590, fx, if).
:- op(580, xfx, then).
:- op(570, xfx, else).

/*
		if
         |
       then
     /      \
    X       else
           /    \
          Y      Z
*/

% code to make b) usable
if Condition then TruePart else FalsePart :-
	( call(Condition) -> 
		call(TruePart)
	;
		call(FalsePart)
	).