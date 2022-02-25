module Language.Dot
  ( Attr
  , CompassPt
  , E(..)
  , EdgeEnd
  , EdgeStmt(..)
  , G(..)
  , Graph(..)
  , Id(..)
  , N(..)
  , NodeId(..)
  , NodeStmt(..)
  , Port(..)
  , S(..)
  , Shape(..)
  , Stmt
  , SubGraph(..)
  , box_
  , class Label
  , directed_
  , label
  , labelImpl
  , nodeStmt
  , shape
  , undirected_
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Graph
  = Graph
  { strict :: Boolean
  , type :: Variant ( directed :: Unit, undirected :: Unit )
  , id :: Maybe Id
  , stmts :: Array Stmt
  }

directed_ :: forall v. Variant ( directed :: Unit | v )
directed_ = inj (Proxy :: _ "directed") unit

undirected_ :: forall v. Variant ( undirected :: Unit | v )
undirected_ = inj (Proxy :: _ "undirected") unit

nodeStmt :: forall a v. a -> Variant ( nodeStmt :: a | v )
nodeStmt = inj (Proxy :: _ "nodeStmt")

type Stmt
  = Variant
      ( nodeStmt :: NodeStmt
      , edgeStmt :: EdgeStmt
      --, attrStmt :: AttrStmt
      , subGraph :: SubGraph
      )

data SubGraph
  = SubGraph { id :: Id, stmts :: Array Stmt }

-- data AttrStmt
--   = AttrStmt
--     ( Variant
--         ( graph :: Unit
--         , node :: Unit
--         , edge :: Unit
--         )
--     )
--     (Array (Attr a))
newtype Id
  = Id String

newtype NodeId
  = NodeId { id :: Id, port :: Maybe Port }

data Port
  = Port Id (Maybe CompassPt)

type CompassPt
  = Variant
      ( n :: Unit
      , ne :: Unit
      , e :: Unit
      , se :: Unit
      , s :: Unit
      , sw :: Unit
      , w :: Unit
      , nw :: Unit
      , c :: Unit
      , "_" :: Unit
      )

newtype NodeStmt
  = NodeStmt { id :: NodeId, attrs :: Array (Attr N) }

type EdgeEnd
  = Variant
      ( nodeId :: NodeId
      , subgraph :: SubGraph
      )

data EdgeStmt
  = EdgeStmt EdgeEnd EdgeEnd (Array EdgeEnd) (Array (Attr E))

--------------------------------------------------------------------------------
-- class Stringify
--------------------------------------------------------------------------------
class Stringify a where
  toString :: a -> String

--------------------------------------------------------------------------------
-- Attr
--------------------------------------------------------------------------------
newtype Attr a
  = Attr { key :: Id, value :: Id }

data N

data E

data G

data S

class Label a where
  label :: String -> Attr a

instance labelN :: Label N where
  label = labelImpl

labelImpl :: forall a. String -> Attr a
labelImpl value = Attr { key: Id "label", value: Id value }

shape :: Shape -> Attr N
shape s = Attr { key: Id "label", value: Id $ shapeToString s }

--------------------------------------------------------------------------------
-- AttrTypes
--------------------------------------------------------------------------------
type Shape
  = Variant
      ( box :: Unit
      , ellipse :: Unit
      )

box_ :: forall v. Variant ( box :: Unit | v )
box_ = inj (Proxy :: _ "box") unit

shapeToString :: Shape -> String
shapeToString s = unsafeCoerce (unsafeCoerce s).type
