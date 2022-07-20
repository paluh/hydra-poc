module Hydra.DidacticWaffleSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.DidacticWaffle (State (..), abort, close, collect, fanout, initialize)
import Test.QuickCheck (conjoin, (===))

spec :: Spec
spec = do
  it "can transition through abort lifecycle" $
    ( initialize
        & abort
    )
      === Final
  it "can transition through full lifecycle" $
    let st1 = initialize & collect
        st2 = close st1 & fanout
     in conjoin [st1 === Open, st2 === Final]
