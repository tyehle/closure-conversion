module Main where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader

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
          | SetEnv Expr String Expr Expr
          | GetEnv Expr String
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
free (SetEnv env name binding body) = free env `Set.union` free binding `Set.union` free body
free (GetEnv env name) = free env
free (ClosFunc env) = free env


convertProg :: Prog -> Prog
convertProg (Prog origDefs body) = Prog (origDefs ++ newDefs) newBody
  where
    globals = Set.fromList . map (\(Def name _ _) -> name) $ origDefs
    (newBody, newDefs) = runWriter . flip runReaderT globals . convert $ body




convert :: Expr -> ReaderT (Set String) (Writer [Def]) Expr
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
convert (Lambda args body) = undefined
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
convert (SetEnv env name binding body) = do
  env' <- convert env
  binding' <- convert env
  body' <- convert body
  return $ SetEnv env' name binding' body'
convert (GetEnv env name) = flip GetEnv name <$> convert env
convert (ClosFunc env) = ClosFunc <$> convert env

callClosure :: Expr -> [Expr] -> Expr
callClosure closure args = Call (ClosFunc closure) $ closure : args



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
plus1 = f[0](1, f[1])
plus2 = f[0](2, f[1])
plus1[0](plus1, 3) + plus2[0](plus2, 3)

-}
