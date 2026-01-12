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