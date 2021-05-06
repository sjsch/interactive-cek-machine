module CEK where

import Prelude

import Control.Monad.State (State)
import Data.Array (length, (:))
import Data.Either (Either(..))
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Expr (Expr(..))

type Env = Array (Tuple String Value)

data Value
  = VInt Int
  | VClos String Expr Env
  | VPrim String Int (Array Value)

instance showValue :: Show Value where
  show (VInt n) = show n
  show (VClos var expr env) = "λ" <> var <> ". " <> show expr <> " [" <> showEnv false env <> "]"
  show (VPrim name _ env) = name <> " [" <> joinWith "; " (map show env) <> "]"

showEnv :: Boolean -> Env -> String
showEnv nls env =
  joinWith (if nls then "\n" else "; ") (map showMapping env)
  where
    showMapping (Tuple var expr) = var <> " → " <> show expr

data Frame
  = Hole                        -- □
  | HoleFunc Expr Env Frame     -- □ expr
  | HoleArg Value Frame         -- value □

data CEK = CEK (Either Value Expr) Env Frame

type Interp a = State Int a

initialCEK :: Expr -> CEK
initialCEK e = CEK (Right e) [] Hole

plugFrame :: Value -> Env -> Frame -> Either String CEK
plugFrame val _ Hole = Right (CEK (Left val) [] Hole)
plugFrame val _ (HoleFunc arg env cont) =
  Right (CEK (Right arg) env (HoleArg val cont))
plugFrame val _ (HoleArg (VClos var body env) cont) =
  Right (CEK (Right body) (Tuple var val : env) cont)
plugFrame val env (HoleArg (VPrim name arity args) cont)
  | length args + 1 == arity = do
      r <- runPrim name (args <> [val])
      pure (CEK (Left r) env cont)
  | otherwise = Right (CEK (Left (VPrim name arity (args <> [val]))) env cont)
plugFrame val _ (HoleArg _ cont) =
  Left "Tried to apply an argument to a non-function"

stepCEK :: CEK -> Either String CEK
stepCEK (CEK (Right (Var x)) env cont) = case lookup x (env <> prims) of
  Nothing -> Left ("Variable not bound: " <> x)
  Just val -> Right (CEK (Left val) env cont)
stepCEK (CEK (Right (Lit n)) env cont) =
  plugFrame (VInt n) env cont
stepCEK (CEK (Right (Abs var body)) env cont) =
  Right (CEK (Left (VClos var body env)) env cont)
stepCEK (CEK (Right (App func arg)) env cont) =
  Right (CEK (Right func) env (HoleFunc arg env cont))
stepCEK (CEK (Left val) env cont) = plugFrame val env cont
stepCEK k = Right k

prims :: Array (Tuple String Value)
prims = [ Tuple "add" (VPrim "add" 2 []) ]

runPrim :: String -> Array Value -> Either String Value
runPrim "add" [VInt n, VInt m] = Right (VInt (n + m))
runPrim "add" _ = Left "'add' expects two integer arguments"
runPrim _ _ = Left "unknown primitive"
