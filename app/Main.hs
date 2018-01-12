module Main where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

main :: IO ()
main = putStrLn "Hi"

data Prog = Prog [Def] Expr

data Def = Def String [String] Expr

data Expr = Num Int
          | Plus Expr Expr
          | Let String Expr Expr
          | Ref String
          | Lambda [String] Expr
          | Call Expr [Expr]
          | NewClos String
          | SetEnv String Expr Expr Expr
          | GetEnv String Expr
          | ClosFunc Expr
          deriving (Eq, Show)


free :: Expr -> Set String
free (Num _) = Set.empty
free (Plus a b) = free a `Set.union` free b
free (Let name binding body) = free binding `Set.union` Set.delete name (free body)
free (Ref name) = Set.singleton name
free (Lambda args body) = foldr Set.delete (free body) args
free (Call fn args) = Set.unions . map free $ fn:args
free (NewClos _) = Set.empty
free (SetEnv name clos binding body) = free clos `Set.union` free binding `Set.union` free body
free (GetEnv name clos) = free clos
free (ClosFunc clos) = free clos


convertProg :: Prog -> Prog
convertProg (Prog origDefs body) = Prog (origDefs ++ newDefs) newBody
  where
    globals = Set.fromList . map (\(Def name _ _) -> name) $ origDefs
    (newBody, newDefs) = runWriter . flip runReaderT globals . flip evalStateT (0, 0) . convert $ body




convert :: Expr -> StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) Expr
convert n@Num{} = return n
convert (Plus a b) = do
  a' <- convert a
  b' <- convert b
  return $ Plus a' b'
convert (Let name binding body) = do
  bind <- convert binding
  body' <- convert body
  return $ Let name bind body'
convert r@Ref{} = return r

convert (Lambda args body) = do
  let freeVars = free body `Set.difference` Set.fromList args
  name <- freshFunc
  body' <- convert body
  tell [Def name ("_env" : args) (subst (free body') (Ref "_env") body')]
  closName <- freshClos
  let setEnv name = SetEnv name (Ref closName) (Ref name)
  let envBindings = foldr setEnv (Ref closName) (Set.toList freeVars)
  return $ Let closName (NewClos name) envBindings

convert (Call (Ref name) args) = do
  args' <- mapM convert args
  isGlobal <- Set.member name <$> ask
  if isGlobal
    then return $ Call (Ref name) args'
    else return $ callClosure (Ref name) args'
convert (Call fn args) = do
  fn' <- convert fn
  args' <- mapM convert args
  return $ callClosure fn' args'

convert ne@NewClos{} = return ne
convert (SetEnv name clos binding body) = do
  clos' <- convert clos
  binding' <- convert binding
  body' <- convert body
  return $ SetEnv name clos' binding' body'
convert (GetEnv name clos) = GetEnv name <$> convert clos
convert (ClosFunc clos) = ClosFunc <$> convert clos


callClosure :: Expr -> [Expr] -> Expr
callClosure closure args = Call (ClosFunc closure) $ closure : args


freshFunc :: StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) String
freshFunc = do
  (f, c) <- get
  put (f+1, c)
  return $ "_f" ++ show f

freshClos :: StateT (Int, Int) (ReaderT (Set String) (Writer [Def])) String
freshClos = do
  (f, c) <- get
  put (f, c+1)
  return $ "_c" ++ show c


subst :: Set String -> Expr -> Expr -> Expr
subst vars env n@Num{} = n
subst vars env (Plus a b) = Plus (subst vars env a) (subst vars env b)
subst vars env (Let name binding body) = Let name binding' body'
  where
    binding' = subst vars env binding
    body' = subst (Set.delete name vars) env body
subst vars env (Ref name)
  | Set.member name vars = GetEnv name env
  | otherwise            = Ref name
subst vars env (Lambda args body) = Lambda args $ subst (vars `Set.difference` Set.fromList args) env body
subst vars env (Call func args) = Call (subst vars env func) (map (subst vars env) args)
subst vars env nc@NewClos{} = nc
subst vars env (SetEnv name clos binding body) = SetEnv name clos' binding' body'
  where
    clos' = subst vars env clos
    binding' = subst vars env binding
    body' = subst vars env body
subst vars env (GetEnv name clos) = GetEnv name $ subst vars env clos
subst vars env (ClosFunc clos) = ClosFunc $ subst vars env clos



{-

f = lambda x: lambda y: x + y
plus1 = f(1)
plus2 = f(2)
plus1(3) + plus2(3)

-- becomes --

def _f0(clos, x):
  _c0 = (_f1, dict())
  _c0[1]['x'] = x
  return _c0

def _f1(clos, y):
  x = clos[1]['x']
  return x + y

_c0 = (_f0, dict())
f = _c0
plus1 = f[0](f, 1)
plus2 = f[0](f, 2)
plus1[0](plus1, 3) + plus2[0](plus2, 3)

-}
