data Tree a = Leaf
            | Node a (Tree a) (Tree a)

buildTree :: Int -> Tree ()
buildTree 0 = Leaf
buildTree n = Node () (buildTree (n-1)) (buildTree (n-1))

paths :: Tree () -> [[Bool]]
paths Leaf = [[]]
paths (Node _ left right) = leftPaths ++ rightPaths
    where 
        leftPaths = map (False:) (paths left)  -- map (False:) [[False], [True]] -> [[False,False], [False,True]]
        rightPaths = map (True:) (paths right)

booleans :: Int -> [[Bool]]
booleans n = paths (buildTree n)