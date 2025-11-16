type Name = Char -- 'x', 'y', 'z', etc

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Or Prop Prop -- new
          | Imply Prop Prop

------------------------------------------------

type Env = [(Name, Bool)] -- associations of names to values

eval :: Env -> Prop -> Bool
eval env (Const c) = c 
eval env (Var x) = case lookup x env of  -- lookup :: Eq a ⇒ a → [(a,b)] → Maybe b
                    Just b -> b
                    Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Or p q) = eval env p || eval env q -- new
eval env (Imply p q) = not (eval env p) || eval env q