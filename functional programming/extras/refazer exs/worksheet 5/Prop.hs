
module Prop where

-- tipo para expressÃµes proposicionais
data Prop = Const Bool        -- constantes
          | Var Char          -- variÃ¡veis
          | Neg Prop          -- negaÃ§Ã£o
          | Conj Prop Prop    -- conjunÃ§Ã£o
          | Disj Prop Prop    -- disjunÃ§Ã£o
          | Impl Prop Prop    -- implicaÃ§Ã£o
            deriving (Eq,Show)
