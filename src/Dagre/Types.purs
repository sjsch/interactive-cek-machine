module Dagre.Types where

import Prelude

import Data.Maybe (Maybe(..))

type Graph a = Array (Def a)

data Def a
  = Node a NodeAttr
  | Edge a a EdgeAttr

derive instance eqDef :: Eq a => Eq (Def a)

type CommonAttr
  = { label :: Maybe String, cssClass :: Maybe String }

type NodeAttr
  = CommonAttr

type EdgeAttr
  = CommonAttr

defAttr :: NodeAttr
defAttr = { label: Nothing, cssClass: Nothing }
