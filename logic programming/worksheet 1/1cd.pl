female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(alex).
female(lily).
female(poppy).

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

% PARENT RELATIONS
% grace and frank are parents of phil
mother(grace,phil).
father(frank,phil).

% dede and jay are parents of claire and mitchell
mother(dede,claire).
father(jay,claire).
mother(dede,mitchell).
father(jay,mitchell).

% jay and gloria are parents of joe
father(jay,joe).
mother(gloria,joe).

% gloria and javier are parents of manny
mother(gloria,manny).
father(javier,manny).

% barb and merle are parents of cameron and pameron
mother(barb,cameron).
father(merle,cameron).
mother(barb,pameron).
father(merle,pameron).

% phil and claire are parents of haley, alex and luke
father(phil,haley).
mother(claire,haley).
father(phil,alex).
mother(claire,alex).
father(phil,luke).
mother(claire,luke).

% mitchell and cameron are parents of lily and rexford
father(mitchell,lily).
father(cameron,lily).
father(mitchell,rexford).
father(cameron,rexford).

% pameron and bo are parents of calhoun
mother(pameron,calhoun).
father(bo,calhoun).

% haley and dylan are parents of george and poppy
mother(haley,george).
mother(haley,poppy).
father(dylan,george).
father(dylan,poppy).

% GRANDPARENT RELATIONS
% grace and frank are grandparents of haley, alex and luke
grandmother(grace,haley).
grandmother(grace,alex).
grandmother(grace,luke).
grandfather(frank,haley).
grandfather(frank,alex).
grandfather(frank,luke).

% dede and jay are grandparents of haley, alex and luke
grandmother(dede,haley).
grandmother(dede,alex).
grandmother(dede,luke).
grandfather(jay,haley).
grandfather(jay,alex).
grandfather(jay,luke).

% dede and jay are grandparents of lily and rexford
grandmother(dede,lily).
grandmother(dede,rexford).
grandfather(jay,lily).
grandfather(jay,rexford).

% barb and merle are grandparents of lily and rexford
grandmother(barb,lily).
grandmother(barb,rexford).
grandfather(merle,lily).
grandfather(merle,rexford).

% barb and merle are grandparents of calhoun
grandmother(barb,calhoun).
grandfather(merle,calhoun).

% claire and phil are grandparents of george and poppy
grandmother(claire,george).
grandmother(claire,poppy).
grandfather(phil,george).
grandfather(phil,poppy).

% SIBLING/HALFSIBLING RELATIONS
% claire and mitchell are siblings, + joe is their half sibling
siblings(claire,mitchell).
halfSiblings(claire,joe).
halfSiblings(mitchell,joe).

% cameron and pameron are siblings
siblings(cameron,pameron).

% haley, alex and luke are siblings
siblings(haley,alex).
siblings(haley,luke).
siblings(alex,luke).

% lily and rexford are siblings
siblings(lily,rexford).

% george and poppy are siblings
siblings(george,poppy).

% COUSIN RELATIONS
% haley, alex and luke are cousins with lily and rexford
cousins(haley,lily).
cousins(haley,rexford).
cousins(alex,lily).
cousins(alex,rexford).
cousins(luke,lily).
cousins(luke,rexford).

% lily and rexford are cousins with calhoun
cousins(calhoun,lily).
cousins(calhoun,rexford).

% UNCLE/AUNT RELATIONS
% mitchell is uncle of haley, alex and luke
uncle(mitchell,haley).
uncle(mitchell,alex).
uncle(mitchell,luke).

% claire is aunt of lily and rexford
aunt(claire,lily).
aunt(claire,rexford).

% cameron is uncle of calhoun
uncle(cameron,calhoun).

% pameron is aunt of lily and rexford
aunt(pameron,lily).
aunt(pameron,rexford).

/* 1d
are Haley and Lily cousins?
| ?- cousins(haley,lily).
yes

who is Luke’s father?
| ?- father(X,luke).
X = phil ? ;
no

who is Lily’s uncle?
| ?- uncle(X,lily).
no

who is a grandmother?
| ?- grandmother(X,_).
X = grace ? ;
X = grace ? ;
X = grace ? ;
X = dede ? ;
X = dede ? ;
X = dede ? ;
X = dede ? ;
X = dede ? ;
X = barb ? ;
X = barb ? ;
X = barb ? ;
X = claire ? ;
X = claire ? ;
no

list all pairs of siblings
| ?- siblings(X,Y).
X = claire,
Y = mitchell ? ;
X = cameron,
Y = pameron ? ;
X = haley,
Y = alex ? ;
X = haley,
Y = luke ? ;
X = alex,
Y = luke ? ;
X = lily,
Y = rexford ? ;
X = george,
Y = poppy ? ;
no

list all pairs of half-siblings
| ?- halfSiblings(X,Y).
X = claire,
Y = joe ? ;
X = mitchell,
Y = joe ? ;
no
*/

% 1 e)
% MARRIAGES (marriage/3)
marriage(dede,jay,1968).
marriage(jay,gloria,2008).

% DIVORCES (divorce/3)
divorce(jay,dede,2003).