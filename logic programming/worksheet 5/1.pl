female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(alex).
female(lily).
femaly(poppy).

male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(bo).
male(dylan).
male(luke).
male(rexford).
male(calhoun).
male(george).

% grace and frank are parents of phil
parent(grace,phil).
parent(frank,phil).

% dede and jay are parents of claire and mitchell
parent(dede,claire).
parent(jay,claire).
parent(dede,mitchell).
parent(jay,mitchell).

% jay and gloria are parents of joe
parent(jay,joe).
parent(gloria,joe).

% gloria and javier are parents of manny
parent(gloria,manny).
parent(javier,manny).

% barb and merle are parents of cameron and pameron
parent(barb,cameron).
parent(merle,cameron).
parent(barb,pameron).
parent(merle,pameron).

% phil and claire are parents of haley, alex and luke
parent(phil,haley).
parent(claire,haley).
parent(phil,alex).
parent(claire,alex).
parent(phil,luke).
parent(claire,luke).

% mitchell and cameron are parents of lily and rexford
parent(mitchell,lily).
parent(cameron,lily).
parent(mitchell,rexford).
parent(cameron,rexford).

% pameron and bo are parents of calhoun
parent(pameron,calhoun).
parent(bo,calhoun).

% haley and dylan are parents of george and poppy
parent(haley,george).
parent(haley,poppy).
parent(dylan,george).
parent(dylan,poppy).


% a) 
children(Person,Children):-
	findall(Child,parent(Person,Child), Children).

% b)
children_of([],[]).

children_of([H|T],[H-ChildrenH|Rest]):-
	children(H,ChildrenH),
	children_of(T,Rest).

% c)
family(F):-
	setof(P,(male(P);female(P)),F).

% d)
couple(X-Y):-
	parent(X,A),
	parent(Y,A),
	X \= Y.
