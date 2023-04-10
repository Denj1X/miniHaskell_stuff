module Eval where

import Exp
import Data.List ( union, delete )

vars :: Exp -> [IndexedVar]
vars (X v)= [v]
vars (Lam x c1) = if elem x (vars c1) then vars c1 else x: vars c1
vars (App c1 c2) = union (vars c1) (vars c2)

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars (X v) = [v]
freeVars (Lam v c) = filter (not . (==) v) (vars c)
freeVars (App c1 c2) = union (freeVars c1) (freeVars c2)
-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

-- >>> freeVars  (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> freeVars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

-- >>> freeVars (Lam (IndexedVar {ivName = "x", ivCount = 0}) (App (X (IndexedVar {ivName = "x", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))))
-- []

occursFree :: IndexedVar -> Exp -> Bool
occursFree v exp = elem v (freeVars exp)

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar v vs = if elem (IndexedVar (ivName v) (ivCount v + 1)) vs 
    then freshVar (IndexedVar (ivName v) (ivCount v + 1)) vs
    else (IndexedVar (ivName v) (ivCount v + 1))

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x", IndexedVar {ivName = "x", ivCount = 1}, IndexedVar {ivName = "y", ivCount = 2}] 
-- IndexedVar {ivName = "x", ivCount = 2}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X x)= if x == toReplace then X replacement else X x
renameVar toReplace replacement (Lam x e) = Lam(if x == toReplace then replacement else x) (renameVar toReplace replacement e)
renameVar toReplace replacement(App exp1 exp2) = App (renameVar toReplace replacement exp1) (renameVar toReplace replacement exp2)

-- >>> renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0}) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X var) = if var == toReplace then replacement else X var
substitute toReplace replacement (App exp1 exp2) = (App (substitute toReplace replacement exp1) (substitute toReplace replacement exp2))
substitute toReplace replacement (Lam var exp) = if var == toReplace then (Lam var exp) 
                                                 else
                                                    if not (occursFree var replacement) then (Lam var (substitute toReplace replacement exp))
                                                    else
                                                        (Lam newVar (substitute toReplace replacement renamedExp))
                                                        where
                                                            newVar :: IndexedVar
                                                            newVar = freshVar var (vars replacement)

                                                            renamedExp :: Exp
                                                            renamedExp = renameVar var newVar exp
-- >>> substitute (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> substitute (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 1}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

normalize :: Exp -> Exp
normalize e = maybe e normalize(step e)
    where   
        step(X x) = Nothing
        step(Lam x e) = Lam x <$> step e
        step(App (Lam x ex) e) = Just (substitute x e ex)
        step (App e1 e2)
            = case step e1 of
                Nothing -> App e1 <$> step e2
                Just e1' -> Just (App e1' e2)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0})))) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))))))
-- X (IndexedVar {ivName = "x", ivCount = 0})