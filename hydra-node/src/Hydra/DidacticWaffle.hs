module Hydra.DidacticWaffle where

import qualified Data.HashSet as HahSet
import Hydra.Prelude hiding (State)

data UTxOSet = UTxOSet
  deriving (Eq, Show)

data State
  = Initial {committed :: HashSet Party, participants :: HashSet Party}
  | Open
  | Closed
  | Final
  deriving (Eq, Show)

newtype Party = Party Text
  deriving newtype (Eq, Show, Hashable)

instance IsString Party where
  fromString = Party . toText

initialize :: HashSet Party -> State
initialize participants = Initial{committed = mempty, participants}

abort :: State -> State
abort = \case
  Initial{} -> Final
  s -> s

commit :: Party -> State -> State
commit p = \case
  Initial{committed, participants} ->
    Initial
      { participants
      , committed = committed <> HahSet.singleton p
      }
  s -> s

-- Maybe something to collect here?
collect :: State -> State
collect = \case
  Initial{committed, participants} | committed == participants -> Open
  s -> s

close :: State -> State
close = \case
  Open -> Closed
  s -> s

fanout :: State -> State
fanout = \case
  Closed -> Final
  s -> s
