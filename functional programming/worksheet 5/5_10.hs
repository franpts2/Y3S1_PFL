type Name = Char -- 'x', 'y', 'z', etc
type Env = [(Name, Bool)]

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

------------------------------------------------
eval :: Env -> Prop -> Bool
eval env (Const c) = c 
eval env (Var x) = case lookup x env of  -- lookup :: Eq a ⇒ a → [(a,b)] → Maybe b
                    Just b -> b
                    Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Imply p q) = not (eval env p) || eval env q

-- from 5.7
vars :: Prop -> [Name]
vars (Const x) = []
vars (Var p) = [p]
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2
vars (Imply prop1 prop2) = vars prop1 ++ vars prop2

-- from 5.8 (needed for 5.9)
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

-- from 5.9
environments :: [Name] -> [Env]
environments lst = map (zip lst) (booleans (length lst))

------------------------------------------------

table :: Prop -> [(Env, Bool)]
table p = [(env, eval env p) | env <- environments (vars p)]