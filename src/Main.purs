module Main where

import Prelude
import Data.Tuple
import Data.Maybe
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM) as DOM
import Partial.Unsafe ( unsafePartial )

-- ADT representing your pick in the game
data RockPaperScissors = Rock | Paper | Scissors

-- ADT representing the winner of the game
data Winner = Human | Computer | Draw

-- The game state is either a combination of each players picks or is nothing because no one has picked yet
type GameState = Maybe (Tuple RockPaperScissors RockPaperScissors)

-- Show instance determining how a RockPaperScissors is printed
instance showRockPaperScissors :: Show RockPaperScissors where
  show Rock     = "Rock"
  show Paper    = "Paper"
  show Scissors = "Scissors"

-- Show instance determining how the winner of the game is printed
instance showWinner :: Show Winner where
  show Human    = "Human"
  show Computer = "Computer"
  show Draw     = "Draw"

-- A function that takes two rock/paper/scissors picks and determines the winner of the game.  The human pick is assumed to be first argument, the computer is the second
determineWinner :: RockPaperScissors -> RockPaperScissors -> Winner
determineWinner Paper Rock     = Human
determineWinner Paper Scissors = Computer
determineWinner Rock Paper     = Computer
determineWinner Rock Scissors  = Human
determineWinner Scissors Paper = Human
determineWinner Scissors Rock  = Computer
determineWinner _ _            = Draw

-- A function that, given the game state, extracts the computer's pick text
computerPickText :: GameState -> String
computerPickText Nothing                       = ""
computerPickText (Just (Tuple _ computerPick)) = show computerPick

-- A function that, given the game state, extracts the name of the winner
winnerName :: GameState -> String
winnerName Nothing                                = ""
winnerName (Just (Tuple playerPick computerPick)) = show $ determineWinner playerPick computerPick

-- the initial state of the game is nothing
initialState :: GameState
initialState = Nothing

-- An effect value for selecting a random Rock / Paper or Scissors
randomPick :: Eff (random :: RANDOM) RockPaperScissors
randomPick = do
  x <- randomInt 1 3
  pure $ unsafePartial $ intToPick x
  where
    intToPick :: Partial => Int -> RockPaperScissors
    intToPick 1 = Rock
    intToPick 2 = Paper
    intToPick 3 = Scissors

-- A function to update the game state by making a random computer pick and combining with the player's choice
performAction :: T.PerformAction _ GameState _ RockPaperScissors
performAction playerPick _ _ = void do
  computerPick <- liftEff randomPick
  T.modifyState $ const $ Just $ Tuple playerPick computerPick

-- A function to render the page based on the game state
render :: T.Render GameState _ _
render dispatch _ state _ =
  [
    R.p [ RP.className "human-picks" ] 
        [ 
        R.text     $ "Your pick: "
        , R.button [ RP.onClick \_ -> dispatch Rock ]
                   [ R.text "Rock" ]
        , R.button [ RP.onClick \_ -> dispatch Paper ]
                   [ R.text "Paper" ]
        , R.button [ RP.onClick \_ -> dispatch Scissors ]
                   [ R.text "Scissors" ]
        ]

    , R.p [ RP.className "computer-picks"]
        [
        R.text     $ "Computer Picked: "
        , R.text   $ computerPickText state
        ]
    , R.p [ RP.className "results"]
        [
        R.text     $ "The winner is: "
        , R.text   $ winnerName state
        ]
  ]
       
-- The component specification, created with a rendering function and a way of updating the state
spec :: T.Spec _ GameState _ _
spec = T.simpleSpec performAction render

-- Renders the component by suppying the initial state
main :: Eff (dom :: DOM.DOM, random :: RANDOM) Unit
main = T.defaultMain spec initialState unit