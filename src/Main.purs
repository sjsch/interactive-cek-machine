module Main where


import CEK (Addr, CEK(..), Env, Frame(..), Value(..), doneCheck, runStep, startState)
import Control.Bind ((>>=))
import Control.Category (identity)
import Dagre (CommonAttr, Def(..), Graph, dagre, defAttr)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashMap (HashMap)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    HA.awaitLoad
    nodes <- liftEffect do
      doc <- window >>= document
      nodes <- querySelectorAll (QuerySelector ".cek-machine") (toParentNode doc)
      nodesArray <- NL.toArray nodes
      pure $ mapMaybe HTMLElement.fromNode nodesArray
    traverse_ (runUI component unit) nodes

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
          , disabled (not (validAndNotDone state.cek))
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

  validAndNotDone Nothing = false
  validAndNotDone (Just s) = not (doneCheck s)
 
cekToGraph :: CEK -> Graph Int
cekToGraph (CEK cek) =
  showCode cek.code
  <> envToLinks (-1) cek.env
  <> [ Edge (-1) cek.cont defAttr { label = Just "cont" } ]
  <> heapToGraph cek.state

  where
    showCode (Left x) =
      [ Node (-1) defAttr { label = Just "value", cssClass = Just "ccont" }
      , Edge (-1) x defAttr { label = Just "value" }
      ]
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
