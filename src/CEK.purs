module CEK where

import Control.Bind ((>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, put, runStateT, modify_)
import Data.Array (length, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foldable as DF
import Data.HashMap (HashMap, empty, insert, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (unit)
import Debug (trace, traceM)
import Expr (Expr(..), showExprPrec)
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit, bind, discard, map, pure, unit, (+), (<>), (==))

type Addr
  = Int

type Env
  = HashMap String Addr

data Value
  = VInt Int
  | VClos String Expr Env
  | VPrim String Int (Array Addr)
  | VCont Frame

instance showValue :: Show Value where
  show (VInt n) = show n
  show (VClos var expr env) = "#λ" <> var <> ". " <> show expr
  show (VPrim name _ env) = "#" <> name
  show (VCont cont) = show cont

-- showEnv :: Boolean -> Env -> String
-- showEnv nls env =
--   joinWith (if nls then "\n" else "; ") (map showMapping env)
--   where
--     showMapping (Tuple var expr) = var <> " → " <> show expr
data Frame
  = Hole -- □
  | HoleFunc Expr Env Addr -- □ expr
  | HoleArg Addr Addr -- value □
  | HoleFuncOnly Addr Addr

instance showFrame :: Show Frame where
  show Hole = "□"
  show (HoleFunc arg _ _) = "□ " <> showExprPrec 2 arg
  show (HoleFuncOnly func _) = "□ arg"
  show (HoleArg func _) = "func □"

type Heap = HashMap Addr Value

data CEK
  = CEK
    { code :: Either Value Expr
    , env :: Env
    , cont :: Addr
    , state :: Heap
    , newsym :: Addr
    }

startState :: Expr -> CEK
startState e =
  CEK
    { code: Right e
    , env: empty
    , cont: 0
    , state: singleton 0 (VCont Hole)
    , newsym: 1
    }

type Interp a
  = StateT CEK (Except String) a

gensym :: Interp Addr
gensym = do
  CEK s <- get
  put (CEK (s { newsym = s.newsym + 1 }))
  pure (s.newsym)

  
alloc :: Value -> Interp Addr
alloc v = do
  addr <- gensym
  modify_ \(CEK s) -> CEK (s { state = insert addr v s.state })
  pure addr

lookupAddr :: Addr -> Interp Value
lookupAddr a = do
  CEK cek <- get
  case lookup a cek.state of
    Nothing -> unsafeCrashWith "Invalid address"
    Just x -> pure x

runStep :: CEK -> Either String CEK
runStep s = runExcept (map snd (runStateT doStep s))
  where
  doStep = do
    stepCEK
    get

doneCheck :: CEK -> Boolean
doneCheck (CEK cek) =
  case cek.code of
    Right _ -> false
    Left _ -> case lookup cek.cont cek.state of
      Just (VCont Hole) -> true
      _ -> false

lookupEnv :: Env -> String -> Interp Value
lookupEnv env x = case lookup x env of
  Nothing -> case DF.lookup x prims of
    Nothing -> throwError ("Variable not bound: " <> x)
    Just prim -> pure prim
  Just a -> lookupAddr a

stepCEK :: Interp Unit
stepCEK = do
  CEK cek <- get
  case cek.code of
    Left val -> alloc val >>= plugFrame
    Right (Var x) -> do
       v <- lookupEnv cek.env x
       modify_ \(CEK c) -> CEK (c { code = Left v })
    Right (Lit x) -> do
      -- Skip the expr -> value step for integers?
      -- alloc (VInt x) >>= plugFrame
      modify_ \(CEK c) -> CEK (c { code = Left (VInt x) })
    Right (App f x) -> do
      newcont <- alloc (VCont (HoleFunc x cek.env cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right f, cont = newcont })
    Right (Abs var body) -> do
      let clos = VClos var body cek.env
      modify_ \(CEK c) -> (CEK c { code = Left clos })
    Right (CallCC expr) -> do
      newcont <- alloc (VCont (HoleFuncOnly cek.cont cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right expr, cont = newcont })

plugFrame :: Addr -> Interp Unit
plugFrame addr = do
  CEK cek <- get
  -- traceM cek
  case lookup cek.cont cek.state of
    Nothing -> unsafeCrashWith "Invalid continuation"
    Just (VCont Hole) -> pure unit
    Just (VCont (HoleFunc arg env cont)) -> do
      newcont <- alloc (VCont (HoleArg addr cont))
      modify_ \(CEK c) -> CEK (c { code = Right arg, env = env, cont = newcont })
    Just (VCont (HoleFuncOnly arg cont)) -> applyFunc arg addr cont
    Just (VCont (HoleArg funcAddr cont)) -> applyFunc addr funcAddr cont
    Just jjj -> unsafeCrashWith ("Not a continuation" <> show jjj)

applyFunc :: Addr -> Addr -> Addr -> Interp Unit
applyFunc addr funcAddr cont = do
  func <- lookupAddr funcAddr
  case func of
    VClos var body env ->
      modify_ \(CEK c) -> CEK (c { code = Right body, env = insert var addr env, cont = cont })
    VPrim name arity args -> 
      if length args + 1 == arity
      then do
        args' <- traverse lookupAddr (snoc args addr)
        r <- runPrim name args'
        modify_ \(CEK c) -> CEK (c { code = Left r, cont = cont })
      else do
        modify_ \(CEK c) -> CEK (c { code = Left (VPrim name arity (snoc args addr)), cont = cont })
    VCont _ -> do
      modify_ \(CEK c) -> CEK (c { cont = funcAddr })
    _ -> throwError "Tried to apply an argument to something that isn't a function, primitive, or continuation"

runPrim :: String -> Array Value -> Interp Value
runPrim "add" [VInt n, VInt m] = pure (VInt (n + m))
runPrim "add" _ = throwError "'add' expects two integer arguments"
runPrim _ _ = throwError "unknown primitive"

prims :: Array (Tuple String Value)
prims = [ Tuple "add" (VPrim "add" 2 []) ]

type GCState = { roots :: Array Addr, newheap :: Heap, oldheap :: Heap }

-- copyLive :: GCState -> GCState
-- copyLive {roots, newheap, oldheap} = foldl copyRoot newheap
--   where
--     copyRoot = ?a
