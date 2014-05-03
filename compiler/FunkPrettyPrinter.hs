module FunkPrettyPrinter
( pprint
) where

import FunkLangDef
import Data.List (intersperse)

-- I'm lazy, okay?
nl, sp :: ShowS
nl  = showString "\n"
sp  = showString " "

compose :: [a -> a] -> a -> a
compose = foldr (.) id

showStrings :: [String] -> ShowS
showStrings = compose . map showString

pprint :: FunkProgram -> String
pprint prog = pprProgram prog ""

pprProgram :: FunkProgram -> ShowS
pprProgram = pprMultiple pprScDefn

pprScDefn :: FunkScDefn -> ShowS
pprScDefn (name, args, expr) = showString name . sp . pprVars args . showString "= " . pprExpr expr

pprExpr :: FunkExpr -> ShowS
pprExpr (EVar v)                     = showString v
pprExpr (ENum n)                     = shows n
pprExpr (EConstr tag arity)          = showStrings ["Pack{", show tag, ",", show arity]
pprExpr (EAp (EAp (EVar "+") e1) e2) = pprAExpr e1 . showString " + " . pprAExpr e2
pprExpr (EAp f x)                    = pprExpr f . showString " " . pprAExpr x
pprExpr (ECase expr alts)            = showString "case " . pprExpr expr . showString " of " . pprAlters alts
pprExpr (ELam vars expr)             = showString "\\" . pprVars vars . showString "-> " . pprExpr expr
pprExpr (ELet isrec defs expr)       = keyword . nl
                                     . pprDefs defs . nl
                                     . showString "in " . pprExpr expr
  where keyword = showString $ if isrec then "letrec" else "let"

pprAExpr :: FunkExpr -> ShowS
pprAExpr e
  | isAtomic e = pprExpr e
  | otherwise  = showString "(" . pprExpr e . showString ")"

pprMultiple :: (a -> ShowS) -> [a] -> ShowS
pprMultiple f = compose . intersperse (showString ";" . nl) . map f

pprDefs :: [(Name, FunkExpr)] -> ShowS
pprDefs = pprMultiple pprDef

pprDef :: (Name, FunkExpr) -> ShowS
pprDef (name, expr) = showStrings [name, " = "] . pprExpr expr

pprAlters :: [FunkAlt] -> ShowS
pprAlters = pprMultiple pprAlter

pprAlter :: FunkAlt -> ShowS
pprAlter (tag, vars, expr) = shownTag . pprVars vars . showString "-> " . pprExpr expr
  where shownTag = showStrings ["<", show tag, "> "]

pprVars :: [Name] -> ShowS
pprVars = compose . map (\x -> showString x . sp)
