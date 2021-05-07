module Main where

import CEK (Addr, CEK(..), Env, Frame(..), Value(..), doneCheck, runStep, startState)
import Control.Category (identity)
import Control.Monad.Error.Class (throwError)
import Dagre (Def(..), Graph, CommonAttr, dagre, defAttr)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashMap (HashMap)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Expr (parseExpr)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, disabled)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, Void, bind, const, discard, join, not, pure, show, unit, ($), (<>), negate)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  HA.runHalogenAff do
    HA.awaitLoad
    appBody' <- HA.selectElement (QuerySelector "#app")
    appBody <- maybe (throwError (error "Could not find app body")) pure appBody'
    runUI component unit appBody

data Action
  = ActionProgram String
  | ActionStep

type Slots = ( graph :: forall q. H.Slot q Void Unit )

type State =
  { errors :: Maybe String
  , cek :: Maybe CEK
  }

_graph = Proxy :: Proxy "graph"

component :: forall a. H.Component a Unit Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { errors: Nothing, cek: Nothing }

  render state =
    HH.div_ $ join
      [ pure $ HH.div [ class_ (ClassName "inputbar") ]
        [ HH.input [ HE.onValueInput ActionProgram ]
        , HH.button
          [ onClick (const ActionStep)
          , disabled (not (isJust state.cek))
          ]
          [HH.text "Step"]
        ]
      , do
           errs <- maybe [] (\x -> [x]) state.errors
           pure $ HH.div [ class_ (ClassName "errors") ] [ HH.text errs ]
      , do
           cek <- maybe [] (\x -> [x]) state.cek
           pure $ HH.slot_ _graph unit dagre (cekToGraph cek)
      ]

  handleAction = case _ of
    ActionProgram "" ->
      H.put { errors: Nothing, cek: Nothing }

    ActionProgram p ->
      case parseExpr p of
        Left err -> H.modify_ (_ { errors = Just err })
        Right code -> H.put
                      { errors: Nothing
                      , cek: Just (startState code)
                      }
    
    ActionStep -> H.modify_ \state ->
      case state.cek of
        Nothing -> state
        Just cek -> do
          if doneCheck cek
            then state
            else case runStep cek of
              Left err -> state { errors = Just err }
              Right cek' -> state { cek = Just cek' }
 
cekToGraph :: CEK -> Graph Int
cekToGraph (CEK cek) =
  showCode cek.code
  <> envToLinks (-1) cek.env
  <> [ Edge (-1) cek.cont defAttr { label = Just "cont" } ]
  <> heapToGraph cek.state

  where
    showCode (Left x) = valToGraph (_ { cssClass = Just "ccont" }) (-1) x
    showCode (Right x) = [ Node (-1) defAttr { label = Just (show x), cssClass = Just "ccont" } ]

heapToGraph :: HashMap Addr Value -> Graph Int
heapToGraph graph = foldMapWithIndex (valToGraph identity) graph

valToGraph :: (CommonAttr -> CommonAttr) -> Addr -> Value -> Graph Int
valToGraph attr addr val = [ Node addr (attr { label: Just (show val), cssClass: cssClass val }) ] <> valLinks addr val
  where
    cssClass (VCont _) = Just "cont"
    cssClass _ = Nothing

valLinks :: Addr -> Value -> Graph Int
valLinks _ (VInt _) = []
valLinks addr (VClos _ _ env) =
  envToLinks addr env
valLinks addr (VCont cont) =
  let f a = [ Edge addr a defAttr { label = Just "cont", cssClass = Just "cont" } ]
  in case cont of
    Hole -> []
    HoleArg x a -> f a <> [ Edge addr x defAttr { label = Just "func" } ]
    HoleFunc _ _ a -> f a
    HoleFuncOnly x a -> f a <> [ Edge addr x defAttr { label = Just "arg" } ]
valLinks addr (VPrim _ _ env) =
  foldMapWithIndex (\k a -> [ Edge addr a defAttr { label = Just (show k) } ]) env

envToLinks :: Addr -> Env -> Graph Int
envToLinks addr env = foldMapWithIndex (\k a -> [ Edge addr a defAttr { label = Just k } ]) env

-- cekToGraph :: CEK -> Graph Int
-- cekToGraph (CEK code env cont) =
--   let depth = frameDepth cont
--   in
--    [ Node depth defAttr {label = Just (showCode code <> "\n" <> showEnv true env)}
--    ]
--   <> frameToGraph depth (depth - 1) cont

--    where
--      showCode (Left val) = show val
--      showCode (Right expr) = show expr

-- frameDepth :: Frame -> Int
-- frameDepth Hole = 1
-- frameDepth (HoleFunc _ _ c) = 1 + frameDepth c
-- frameDepth (HoleArg _ c) = 1 + frameDepth c

-- frameToGraph :: Int -> Int -> Frame -> Graph Int
-- frameToGraph _ _ Hole = []
-- frameToGraph prev n (HoleFunc arg env cont) =
--   [ Node n defAttr {label = Just ("○ " <> showExprPrec 2 arg <> "\n" <> showEnv true env)}
--   , Edge prev n defAttr
--   ]
--   <> frameToGraph n (n - 1) cont
-- frameToGraph prev n (HoleArg func cont) =
--   [ Node n defAttr {label = Just ("(" <> show func <> ") ○")}
--   , Edge prev n defAttr
--   ]
--   <> frameToGraph n (n - 1) cont
