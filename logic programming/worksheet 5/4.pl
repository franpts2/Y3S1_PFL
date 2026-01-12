%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).

% a)
get_all_nodes(ListOfAirports):-
	setof(A,Dest^Comp^Code^Hr^Dur^Orig^Comp2^Code2^Hr2^Dur2^(
		flight(A,Dest,Comp,Code,Hr,Dur);
		flight(Orig,A,Comp2,Code2,Hr2,Dur2)
		),ListOfAirports).

% b)
cities_by_company(Comp,Cities):-
	setof(C,D^Code^Hr^Dur^(
		flight(C,D,Comp,Code,Hr,Dur);
		flight(D,C,Comp,Code,Hr,Dur)
	),Cities).

my_reverse(L1,L2):-
	my_reverse_aux(L1,[],L2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,L2):-
	my_reverse_aux(T,[H|Acc],L2).


most_diversified(Company):-
	setof(NumCities-Company,Cities^(
		cities_by_company(Company,Cities),
		length(Cities,NumCities)
		),L),
	my_reverse(L,[MaxVal-AnyComp|Rest]),
	member(MaxVal-Company,[MaxVal-AnyComp|Rest]). % for backtracking

% c)
not(X) :- X, !, fail. 
not(_X).

find_flights_dfs(Dest,Dest,_,[]).

find_flights_dfs(Cur,Dest,Visited,[Code|Rest]):-
	flight(Cur,Next,_,Code,_,_),
	not(memberchk(Next,Visited)),
	find_flights_dfs(Next,Dest,[Next|Visited],Rest).

find_flights(Origin,Destination,Flights):-
	find_flights_dfs(Origin,Destination,[Origin],Flights).

% d)
find_flights_bfs(Origin,Destination,Flights):-
	bfs_queue([[Origin]],Destination,Flights).

% base case: 1st path in queue reaches destination 
bfs_queue([[Dest|T]|_],Dest,Path):-
	extract_codes_and_reverse([Dest|T],Path). % reverse to get S -> ... -> F order 

bfs_queue([[Cur|T]|R],Dest,Path):-
	findall([Next,Code,Cur|T],Comp^Hr^Dur^(
		flight(Cur,Next,Comp,Code,Hr,Dur),
		not(memberchk(Next,T))
	),NewPaths),
	append(R,NewPaths,NextQueue),
	bfs_queue(NextQueue,Dest,Path).

extract_codes_and_reverse(PathWCities,FinalCodes):-
	extract_codes(PathWCities,CodesRev),
	my_reverse(CodesRev,FinalCodes).


% resulting list is like [City,Code,City,Code,...]
extract_codes([],[]).
extract_codes([_City],[]).

extract_codes([_City,Code|T],[Code|Rest]):-
	extract_codes(T,Rest).

% e)
find_all_flights(Origin,Dest,Flights):-
	findall(F,(
		find_flights_bfs(Origin,Dest,F) % can also be find_all_flights/3
	),Flights).

% f)
find_flights_least_stops(Origin,Dest,ListOfFlights):-
	findall(F,(
		find_flights_bfs(Origin,Dest,F)
	),[Shortest|RestFlights]),

	length(Shortest,MinLen),

	findall(F,(
		member(F,[Shortest|RestFlights]),
		length(F,MinLen)
	),ListOfFlights).