module Main where

import CEK (Addr, CEK(..), Env, Frame(..), Value(..), doneCheck, exprRoots, runStep, startState)
import Control.Bind ((>>=))
import Control.Category (identity, (>>>))
import Dagre (CommonAttr, Def(..), Graph, dagre, defAttr)
import Data.Array (foldMap, mapMaybe)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashMap (HashMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Expr (Expr(..), parseExpr)
import Halogen (ClassName(..), RefLabel(..), getRef)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, disabled, ref)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, Void, bind, const, discard, join, negate, not, pure, show, unit, ($), (-), (<>))
import Type.Proxy (Proxy(..))
import Web.DOM.Element (getAttribute)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (key)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    HA.awaitLoad
    nodes <-
      liftEffect do
        doc <- window >>= document
        nodes <- querySelectorAll (QuerySelector ".cek-machine") (toParentNode doc)
        nodesArray <- NL.toArray nodes
        pure $ mapMaybe HTMLElement.fromNode nodesArray
    traverse_ runAndLoadCEK nodes

runAndLoadCEK :: HTMLElement -> Aff Unit
runAndLoadCEK el = do
  init <-
    liftEffect do
      prog <- getAttribute "data-prog" (HTMLElement.toElement el)
      step <- getAttribute "data-step" (HTMLElement.toElement el)
      pure do
        p <- prog
        s <- step
        s' <- Int.fromString s
        pure { prog: p, step: s' }
  _ <- runUI component init el
  pure unit

type InitialCEK
  = { prog :: String, step :: Int }

data Action
  = ActionProgram String
  | ActionStep
  | ActionNone
  | ActionInitialize

type Slots
  = ( graph :: forall q. H.Slot q Void Unit )

type State
  = { errors :: Maybe String
    , cek :: Maybe CEK
    , initial :: Maybe InitialCEK
    }

_graph = Proxy :: Proxy "graph"

component :: forall a. H.Component a (Maybe InitialCEK) Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just ActionInitialize
              }
    }
  where
  initialState init = { errors: Nothing, cek: Nothing, initial: init }

  render state =
    HH.div_
      $ join
          [ pure
              $ HH.div [ class_ (ClassName "inputbar") ]
                  [ HH.input
                      [ HE.onValueInput ActionProgram
                      , HE.onKeyDown (\ke -> if key ke == "Enter" then ActionStep else ActionNone)
                      , ref progInputRef
                      ]
                  , HH.button
                      [ onClick (const ActionStep)
                      , disabled (not (validAndNotDone state.cek))
                      ]
                      [ HH.text "Step" ]
                  ]
          , do
              errs <- maybe [] (\x -> [ x ]) state.errors
              pure $ HH.div [ class_ (ClassName "errors") ] [ HH.text errs ]
          , do
              cek <- maybe [] (\x -> [ x ]) state.cek
              pure $ HH.slot_ _graph unit dagre (cekToGraph cek)
          ]

  handleAction = case _ of
    ActionProgram "" -> H.put { errors: Nothing, cek: Nothing, initial: Nothing }
    ActionProgram p -> case parseExpr p of
      Left err -> H.modify_ (_ { errors = Just err })
      Right code ->
        H.put
          { errors: Nothing
          , cek: Just (startState code)
          , initial: Nothing
          }
    ActionStep ->
      H.modify_ \state -> case state.cek of
        Nothing -> state
        Just cek -> do
          if doneCheck cek then
            state
          else case runStep cek of
            Left err -> state { errors = Just err }
            Right cek' -> state { cek = Just cek' }
    ActionInitialize -> do
      progInput <- getRef progInputRef
      { initial } <- H.get
      case initial of
        Nothing -> pure unit
        Just { prog, step } -> do
          liftEffect do
            progInput' <-
              maybe (throw "Could not find element for input") pure progInput
                >>= HTMLInputElement.fromElement
                >>> maybe (throw "Could not find element for input") pure
            HTMLInputElement.setValue prog progInput'
          H.put case runCEKSteps prog step of
            Left err -> { errors: Just err, cek: Nothing, initial: Nothing }
            Right cek -> { errors: Nothing, cek: Just cek, initial: Nothing }
    ActionNone -> pure unit

  validAndNotDone Nothing = false

  validAndNotDone (Just s) = not (doneCheck s)

  progInputRef = RefLabel "proginput"

runCEKSteps :: String -> Int -> Either String CEK
runCEKSteps prog steps = do
  expr <- parseExpr prog
  let
    cek = startState expr
  go cek steps
  where
  go cek 0 = pure cek

  go cek n = do
    cek' <- runStep cek
    go cek' (n - 1)

cekToGraph :: CEK -> Graph Int
cekToGraph (CEK cek) =
  showCode cek.code
    <> [ Edge (-1) cek.cont defAttr { label = Just "cont" } ]
    <> heapToGraph cek.state
  where
  showCode (Left x) =
    [ Node (-1) defAttr { label = Just "value", cssClass = Just "ccont" }
    , Edge (-1) x defAttr { label = Just "value" }
    ]

  showCode (Right x) =
    [ Node (-1) defAttr { label = Just (show x), cssClass = Just "ccont" } ]
      <> exprToLinks (-1) x cek.env

heapToGraph :: HashMap Addr Value -> Graph Int
heapToGraph graph = foldMapWithIndex (valToGraph identity) graph

valToGraph :: (CommonAttr -> CommonAttr) -> Addr -> Value -> Graph Int
valToGraph attr addr val = [ Node addr (attr { label: Just (show val), cssClass: cssClass val }) ] <> valLinks addr val
  where
  cssClass (VCont _) = Just "cont"

  cssClass _ = Nothing

valLinks :: Addr -> Value -> Graph Int
valLinks _ (VInt _) = []

valLinks _ (VBool _) = []

valLinks _ VUndef = []

valLinks a (VPair l r) =
  [ Edge a l defAttr { label = Just "l" }
  , Edge a r defAttr { label = Just "r" }
  ]

valLinks addr (VClos v expr env) = exprToLinks addr (Abs v expr) env

valLinks addr (VCont cont) =
  let
    f a = [ Edge addr a defAttr { label = Just "cont", cssClass = Just "cont" } ]
  in
    case cont of
      Hole -> []
      HoleArg x a -> f a <> [ Edge addr x defAttr { label = Just "func" } ]
      HoleFunc expr env a -> f a <> exprToLinks addr expr env
      HoleFuncOnly x a -> f a <> [ Edge addr x defAttr { label = Just "arg" } ]
      HoleIf et ef env a -> f a <> exprToLinks addr et env <> exprToLinks addr ef env
      HoleLet env v expr a -> f a <> exprToLinks addr expr env

valLinks addr (VPrim _ _ env) = foldMapWithIndex (\k a -> [ Edge addr a defAttr { label = Just (show k) } ]) env

exprToLinks :: Addr -> Expr -> Env -> Graph Int
exprToLinks addr expr env = foldMap (\(Tuple k a) -> [ Edge addr a defAttr { label = Just k } ])
                            (exprRoots env expr)
