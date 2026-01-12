/*
- Empty Node: Represented as null.
- Other Nodes: Represented as a compound term in the format node(Value, Left, Right)
*/

% a)
tree_size(null,0).

tree_size(node(_,Left,Right), Size):-
    tree_size(Left,SL),
    tree_size(Right,SR),
    Size is 1 + SL + SR.

% b)
tree_map(_,null,null).

tree_map(Pred,node(V1,L1,R1),node(V2,L2,R2)):-
    call(Pred,V1,V2), % Pred(V1,V2)
    tree_map(Pred,L1,L2),
    tree_map(Pred,R1,R2).

% c)
tree_value_at_level(Tree,Value,Level):-
    nonvar(Value),
    var(Level), !,
    (find_level(Tree,Value,0,Level) -> true; Level = -1).

tree_value_at_level(Tree,Value,Level):-
    find_level(Tree,Value,0,Level).


find_level(node(Value,_,_),Value,L,L). % found at cur node

find_level(node(_,Left,_),Value,L,FoundL):-
    L1 is L+1,
    find_level(Left,Value,L1,FoundL).

find_level(node(_,_,Right),Value,L,FoundL):-
    L1 is L+1,
    find_level(Right,Value,L1,FoundL).

% for testing:
% A tree with root 3, children 1 and 7, and grandchildren 5 and 9
MyTree = node(3, node(1, null, null), node(7, node(5, null, null), node(9, null, null))).