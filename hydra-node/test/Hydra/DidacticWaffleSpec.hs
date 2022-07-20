module Hydra.DidacticWaffleSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.HashSet as HashSet
import Hydra.DidacticWaffle (State (..), abort, close, collect, commit, fanout, initialize)
import Test.QuickCheck (conjoin, (===))

spec :: Spec
spec = do
  it "can transition through abort lifecycle" $
    ( initialize mempty
        & abort
    )
      === Final

  it "can transition through full lifecycle" $
    let st1 = initialize mempty & collect
        st2 = close st1 & fanout
     in conjoin [st1 === Open, st2 === Final]

  it "can't collect before every party committed" $
    ( initialize (HashSet.fromList ["alice", "bob"])
        & commit "alice"
        & collect
    )
      `shouldSatisfy` \case
        Initial{} -> True
        _ -> False
