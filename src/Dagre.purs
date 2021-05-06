module Dagre (module Dagre.Types, dagre) where

import Control.Monad.RWS (get, put)
import Dagre.Types (CommonAttr, Def(..), EdgeAttr, Graph, NodeAttr, defAttr)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1, EffectFn2, runEffectFn2)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Halogen (ClassName(..), Component, ElemName(..), Namespace(..), RefLabel(..), defaultEval, getHTMLElementRef, mkComponent, mkEval)
import Halogen.HTML (div, elementNS)
import Halogen.HTML.Properties (class_, ref)
import Prelude (class Eq, Unit, Void, bind, discard, pure, when, (/=), (<<<))
import Web.HTML (HTMLElement)

data Action a = Initialize | Receive (Graph a)

refGraphSVG :: RefLabel
refGraphSVG = RefLabel "graphsvg"

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

foreign import renderGraph :: forall a. EffectFn2 HTMLElement (Graph a) Unit

foreign import setupZoom :: EffectFn1 HTMLElement Unit

dagre :: forall a q. Eq a => Component q (Graph a) Void Aff
dagre = mkComponent
  { initialState: \s -> s
  , render: \_ -> div
                  [ ref refGraphSVG, class_ (ClassName "dagre") ]
                  [ elementNS
                    svgNS
                    (ElemName "svg")
                    []
                    [ elementNS
                      svgNS
                      (ElemName "g")
                      [] []
                    ]
                  ]
  , eval: mkEval defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    , initialize = Just Initialize
    }
  }
  where
    handleAction = case _ of
      Initialize -> do
        writeGraph
        -- svg <- findSVG
        -- liftEffect (runEffectFn1 setupZoom svg)
      Receive g -> updateGraph g

    updateGraph newG = do
      oldG <- get
      when (newG /= oldG) do
        put newG
        writeGraph

    writeGraph = do
      newG <- get
      svg <- findSVG
      liftEffect (runEffectFn2 renderGraph svg newG)

    findSVG = do
      svg <- getHTMLElementRef refGraphSVG
      liftEffect (maybe (throw "Could not find SVG element for graph") pure svg)
