module Hydra.DidacticWaffle where

import Hydra.Prelude hiding (State)
import Data.HashSet (HashSet)

data UTxOSet = UTxOSet
  deriving (Eq, Show)

data State
  = Initial { committed :: HashSet Party, participants :: HashSet Party }
  | Open
  | Closed
  | Final
  deriving (Eq, Show)

data Party = Alice
           | Bob
  deriving (Eq, Show)

initialize :: HashSet Party -> State
initialize participants = Initial { committed = mempty, participants }

abort :: State -> State
abort = \case
  Initial {} -> Final
  s -> s

commit :: Party -> State -> State
commit p = const

-- Maybe something to collect here?
collect :: State -> State
collect = \case
  Initial { committed, participants } | committed == participants -> Open
  s -> s

close :: State -> State
close = \case
  Open -> Closed
  s -> s

fanout :: State -> State
fanout = \case
  Closed -> Final
  s -> s
