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