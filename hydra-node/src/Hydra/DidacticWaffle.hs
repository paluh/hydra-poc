module Hydra.DidacticWaffle where

import Hydra.Prelude hiding (State)

data UTxOSet = UTxOSet
  deriving (Eq, Show)

-- {utxos :: UTxOSet}

data State
  = Initial
  | Open
  | Closed
  | Final
  deriving (Eq, Show)

-- () -> Initial
-- Final -> ()

-- data Machine = Machine { state :: State }

initialize :: State
initialize = Initial

abort :: State -> State
abort = \case
  Initial -> Final
  s -> s

commit {- UTxO -> -} :: State -> State
commit = undefined

-- Maybe something to collect here?
collect :: State -> State
collect = \case
  Initial -> Open
  s -> s

close :: State -> State
close = \case
  Open -> Closed
  s -> s

fanout :: State -> State
fanout = \case
  Closed -> Final
  s -> s
