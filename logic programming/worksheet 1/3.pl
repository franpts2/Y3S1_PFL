% a)

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

% b)
pilotWinPorto(Pilot):- win(Pilot,porto).
teamWinPorto(Team):- teamWins(Team,porto).
nineGates(Circuit):- nGates(Circuit,9).
notEdge(Pilot):- pilots(Pilot,Plane), Plane \= edge540.
wonMoreThanOne(Pilot):- win(Pilot,C1), win(Pilot,C2), C1 \= C2.
planeWonPorto(Plane):- pilots(P,Plane), win(P,porto).