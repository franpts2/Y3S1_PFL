% pilot/1
pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

% inTeam/2, inTeam(a,b) = a is in team b
inTeam(lamb,breitling).
inTeam(besenyei,redbull).
inTeam(chambliss,redbull).
inTeam(maclean,mrt). % mrt = mediterranean racing team
inTeam(mangold,cobra).
inTeam(jones,matador).
inTeam(bonhomme,matador).

% pilots/2, pilots(a,b) = a pilots b
pilots(lamb,mx2).
pilots(besenyei,edge540).
pilots(chambliss,edge540).
pilots(maclean,edge540).
pilots(mangold,edge540).
pilots(jones,edge540).
pilots(bonhomme,edge540).

% circuit/1
circuit(istanbul).
circuit(budapest).
circuit(porto).

% win/2, win(a,b) = a wins/won in b
win(jones,porto).
win(mangold,budapest).
win(mangold,istanbul).

% nGates/2, nGates(a,n) = a has n gates
nGates(istanbul,9).
nGates(budapest,6).
nGates(porto,5).

teamWins(Team, Race):- win(P,Race), inTeam(P,Team).

% a)
more_gates(X,Y):- /* X has more gates than Y? */
	nGates(X,NX),
	nGates(Y,NY),
	NX > NY.

most_gates(X):-
	circuit(X),
	\+ more_gates(_,X).

% b)
less_gates(X,Y):- /* X has less gates than Y? */
	nGates(X,NX),
	nGates(Y,NY),
	NX < NY.

least_gates(X):-
	circuit(X),
	\+ less_gates(_,X).

% c)
gate_diff(X):-
    most_gates(M),
    nGates(M,NM),
    least_gates(L),
    nGates(L,NL),
    X is NM - NL.

% d)
same_team(X,Y):-
    inTeam(X,T),
    inTeam(Y,T),
    X \= Y.

% e)
is_from_winning_team(P,C):-
    circuit(C), pilot(P),
    teamWins(T,C),
    \+ win(P,C), /* not the winner */
    inTeam(P,T).
