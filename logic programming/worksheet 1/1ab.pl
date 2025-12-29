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

/* 1b)
i. Is Haley a female?
| ?- female(haley).
yes

ii. Is Gil a male?
| ?- male(gil).  
no

iii. Is Frank a parent of Phil?
| ?- parent(frank,phil).
yes

iv. Who are Claire’s parents?
| ?- parent(X,claire).
X = dede ? ;
X = jay ? ;
no

v. Who are Gloria’s children?
| ?- parent(gloria,X).
X = joe ? ;
X = manny ? ;
no

vi. Who are Jay’s grandchildren?
| ?- parent(jay,Child), parent(Child, Grandchild).
Child = claire,
Grandchild = haley ? ;
Child = claire,
Grandchild = alex ? ;
Child = claire,
Grandchild = luke ? ;
Child = mitchell,
Grandchild = lily ? ;
Child = mitchell,
Grandchild = rexford ? ;
no

vii. Who are Lily’s grandparents?
| ?- parent(Grandparent, Parent), parent(Parent,lily).
Grandparent = dede,
Parent = mitchell ? ;
Grandparent = jay,
Parent = mitchell ? ;
Grandparent = barb,
Parent = cameron ? ;
Grandparent = merle,
Parent = cameron ? ;
no

viii. Does Alex have children?
| ?- parent(alex,X).
no

ix. Who are Luke’s siblings (child of both Luke’s parents)?
| ?- parent(P1, luke), parent(P2, luke), P1 @< P2, parent(P1, Sibling), parent(P2, Sibling), Sibling \= luke.
P1 = claire,
P2 = phil,
Sibling = haley ? ;
P1 = claire,
P2 = phil,
Sibling = alex ? ;
no
*/