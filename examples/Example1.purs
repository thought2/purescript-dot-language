module Example1 where

import Prelude
import Data.Maybe (Maybe(..))
import Language.Dot as D

-- graph :: D.Graph
-- graph =
--   D.Graph
--     { strict: true
--     , type: D.directed_
--     , id: Just $ D.Id "my-graph"
--     , stmts:
--         [ D.nodeStmt
--             $ D.NodeStmt
--                 { id: D.NodeId { id: D.Id "node1", port: Nothing }
--                 , attrs:
--                     [ D.label "Foo"
--                     , D.shape D.box_
--                     ]
--                 }
--         ]
--     }
