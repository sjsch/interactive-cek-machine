module CEK where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, put, runStateT, modify_)
import Data.Array (length, snoc)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.CommutativeRing ((*))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foldable as DF
import Data.HashMap (HashMap, empty, insert, lookup, member, singleton)
import Data.HashMap as HM
import Data.Maybe (Maybe(..), maybe)
import Data.Show (class Show, show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Expr (Expr(..), showExprPrec)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit, bind, discard, map, pure, unit, (+), (<>), (==))

type Addr
  = Int

type Env
  = HashMap String Addr

data Value
  = VInt Int
  | VBool Boolean
  | VClos String Expr Env
  | VPrim String Int (Array Addr)
  | VCont Frame
  | VPair Addr Addr
  | VUndef

instance showValue :: Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VClos var expr env) = "#λ" <> var <> ". " <> show expr
  show (VPrim name _ env) = "#" <> name
  show (VCont cont) = show cont
  show (VPair l r) = "pair l r"
  show VUndef = "undefined"

data Frame
  = Hole -- □
  | HoleFunc Expr Env Addr -- □ expr
  | HoleArg Addr Addr -- value □
  | HoleFuncOnly Addr Addr -- □ value
  | HoleIf Expr Expr Env Addr -- if □ then expr else expr
  | HoleLet Env Addr Expr Addr -- let v = □ in expr

instance showFrame :: Show Frame where
  show Hole = "□"
  show (HoleFunc arg _ _) = "□ " <> showExprPrec 2 arg
  show (HoleFuncOnly func _) = "□ arg"
  show (HoleArg func _) = "func □"
  show (HoleIf t f _ _) = "if □ then " <> showExprPrec 0 t <> " else " <> showExprPrec 0 f
  show (HoleLet _ _ body _) = "let var = □ in " <> showExprPrec 0 body

type Heap
  = HashMap Addr Value

data CEK
  = CEK
    { code :: Either Addr Expr
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
    garbageCollect
    get

doneCheck :: CEK -> Boolean
doneCheck (CEK cek) = case cek.code of
  Right _ -> false
  Left _ -> case lookup cek.cont cek.state of
    Just (VCont Hole) -> true
    _ -> false

lookupEnv :: Env -> String -> Interp Addr
lookupEnv env x = case lookup x env of
  Nothing -> case DF.lookup x prims of
    Nothing -> throwError ("Variable not bound: " <> x)
    Just prim -> alloc prim
  Just a -> pure a

stepCEK :: Interp Unit
stepCEK = do
  CEK cek <- get
  case cek.code of
    Left val -> plugFrame val
    Right (Var x) -> do
      v <- lookupEnv cek.env x
      modify_ \(CEK c) -> CEK (c { code = Left v })
    Right (Lit x) -> do
      r <- alloc (VInt x)
      modify_ \(CEK c) -> CEK (c { code = Left r })
      plugFrame r
    Right (LitB x) -> do
      r <- alloc (VBool x)
      modify_ \(CEK c) -> CEK (c { code = Left r })
      plugFrame r
    Right (App f x) -> do
      newcont <- alloc (VCont (HoleFunc x cek.env cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right f, cont = newcont })
    Right (Abs var body) -> do
      clos <- alloc (VClos var body cek.env)
      modify_ \(CEK c) -> (CEK c { code = Left clos })
    Right (CallCC expr) -> do
      newcont <- alloc (VCont (HoleFuncOnly cek.cont cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right expr, cont = newcont })
    Right (If cond t f) -> do
      newcont <- alloc (VCont (HoleIf t f cek.env cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right cond, cont = newcont })
    Right (Let var body expr) -> do
      v <- alloc VUndef
      let env' = insert var v cek.env
      newcont <- alloc (VCont (HoleLet env' v expr cek.cont))
      modify_ \(CEK c) -> CEK (c { code = Right body, env = env', cont = newcont })

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
    Just (VCont (HoleIf t f env cont)) -> case lookup addr cek.state of
      Nothing -> unsafeCrashWith "Invalid address"
      Just (VBool b) -> do
        let
          x = if b then t else f
        modify_ \(CEK c) -> CEK (c { code = Right x, env = env, cont = cont })
      Just _ -> throwError "Tried to use if on a non-boolean"
    Just (VCont (HoleLet env v expr cont)) -> do
      val <- lookupAddr addr
      modify_ \(CEK c) -> CEK (c { code = Right expr, state = insert v val cek.state, cont = cont, env = env })
    Just x -> unsafeCrashWith ("Not a continuation " <> show x)

applyFunc :: Addr -> Addr -> Addr -> Interp Unit
applyFunc addr funcAddr cont = do
  func <- lookupAddr funcAddr
  case func of
    VClos var body env -> modify_ \(CEK c) -> CEK (c { code = Right body, env = insert var addr env, cont = cont })
    VPrim name arity args ->
      if length args + 1 == arity then do
        args' <- traverse lookupAddr (snoc args addr)
        r <- runPrim name (snoc args addr) args'
        modify_ \(CEK c) -> CEK (c { code = Left r, cont = cont })
      else do
        r <- alloc (VPrim name arity (snoc args addr))
        modify_ \(CEK c) -> CEK (c { code = Left r, cont = cont })
    VCont _ -> do
      modify_ \(CEK c) -> CEK (c { cont = funcAddr })
    _ -> throwError "Tried to apply an argument to something that isn't a function, primitive, or continuation"

runPrim :: String -> Array Addr -> Array Value -> Interp Addr
runPrim "add" _ [ VInt n, VInt m ] = alloc (VInt (n + m))

runPrim "add" _ _ = throwError "'add' expects two integer arguments"

runPrim "mul" _ [ VInt n, VInt m ] = alloc (VInt (n * m))

runPrim "mul" _ _ = throwError "'mul' expects two integer arguments"

runPrim "eq" _ [ VInt n, VInt m ] = alloc (VBool (n == m))

runPrim "eq" _ _ = throwError "'eq' expects two integer arguments"

runPrim "pair" [ l, r ] _ = alloc (VPair l r)

runPrim "pair" _ _ = throwError "'pair' expects two arguments"

runPrim "fst" _ [ VPair l _ ] = pure l

runPrim "fst" _ _ = throwError "'fst' expects a pair argument"

runPrim "snd" _ [ VPair _ r ] = pure r

runPrim "snd" _ _ = throwError "'snd' expects a pair argument"

runPrim _ _ _ = throwError "unknown primitive"

prims :: Array (Tuple String Value)
prims =
  [ Tuple "add" (VPrim "add" 2 [])
  , Tuple "mul" (VPrim "mul" 2 [])
  , Tuple "eq" (VPrim "eq" 2 [])
  , Tuple "pair" (VPrim "pair" 2 [])
  , Tuple "fst" (VPrim "fst" 1 [])
  , Tuple "snd" (VPrim "snd" 1 [])
  ]

type GCState
  = { roots :: Array Addr, newheap :: Heap, oldheap :: Heap }

exprRoots :: Env -> Expr -> Array (Tuple String Addr)
exprRoots env (Var v) = maybe [] (\x -> [Tuple v x]) (lookup v env)
exprRoots _ (Lit _) = []
exprRoots _ (LitB _) = []
exprRoots env (If t f e) = exprRoots env t <> exprRoots env f <> exprRoots env e
exprRoots env (App f a) = exprRoots env f <> exprRoots env a
exprRoots env (Abs v a) = exprRoots (HM.delete v env) a
exprRoots env (CallCC e) = exprRoots env e
exprRoots env (Let v b e) =
  let env' = HM.delete v env
  in exprRoots env' b <> exprRoots env' e

exprRoots' :: Env -> Expr -> Array Addr
exprRoots' env expr = map snd (exprRoots env expr)

valRoots :: Value -> Array Addr
valRoots (VInt _) = []

valRoots (VBool _) = []

valRoots (VPair l r) = [ l, r ]

valRoots (VClos v expr env) = exprRoots' env (Abs v expr)

valRoots (VPrim _ _ env) = env

valRoots VUndef = []

valRoots (VCont Hole) = []

valRoots (VCont (HoleArg func cont)) = [ func, cont ]

valRoots (VCont (HoleFunc expr env cont)) = snoc (exprRoots' env expr) cont

valRoots (VCont (HoleFuncOnly arg cont)) = [ arg, cont ]

valRoots (VCont (HoleIf t f env cont)) = snoc (exprRoots' env t <> exprRoots' env f) cont

valRoots (VCont (HoleLet env v expr cont)) = snoc (exprRoots' env expr) cont 

copyLive :: GCState -> GCState
copyLive state = foldl copyRoot state state.roots
  where
  copyRoot s root
    | member root s.newheap = s { roots = A.delete root s.roots }
    | otherwise = case lookup root s.oldheap of
      Nothing -> unsafeCrashWith "Address went nowhere"
      Just val -> s { newheap = insert root val s.newheap, roots = s.roots <> valRoots val }

garbageCollect :: Interp Unit
garbageCollect = do
  CEK cek <- get
  let
    newheap = go { roots: initialRoots cek, newheap: empty, oldheap: cek.state }
  put (CEK (cek { state = newheap }))
  where
  go s@{ roots: [] } = s.newheap

  go s = go (copyLive s)

  initialRoots { code: Left addr, cont } = [ addr, cont ]

  initialRoots { code: Right expr, env, cont } = exprRoots' env expr <> [ cont ]
