type Name = Char -- 'x', 'y', 'z', etc

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

------------------------------------------------

vars :: Prop -> [Name]
vars (Const x) = []
vars (Var p) = [p]
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2
vars (Imply prop1 prop2) = vars prop1 ++ vars prop2
 
