module FunkLangDef where

type Name  = String
type IsRec = Bool

data Expr a
  = EVar    Name
  | ENum    Int
  | EConstr Int Int
  | EAp     (Expr a) (Expr a)
  | ELet    IsRec [(a, Expr a)] (Expr a)
  | ECase   (Expr a) [Alter a]
  | ELam    [a] (Expr a)
   deriving (Show, Eq)

type Alter   a = (Int,  [a], Expr a)
type ScDefn  a = (Name, [a], Expr a)
type Program a = [ScDefn a]

type FunkExpr    = Expr    Name
type FunkAlt     = Alter   Name
type FunkScDefn  = ScDefn  Name
type FunkProgram = Program Name

bindersOf :: [(a,b)] -> [a]
bindersOf = map fst

rhssOf :: [(a,b)] -> [b]
rhssOf = map snd

isAtomic :: Expr a -> Bool
isAtomic (EVar _) = True
isAtomic (ENum _) = True
isAtomic _        = False
