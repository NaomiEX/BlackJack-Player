-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!
module Player where

-- This is the source for the parser from the course notes
import Cards -- Finally, the generic card type(s)
-- Here you will find types used in the game of TwentyOne
-- Rules of the game

-- You can add more imports if you need them

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.Char (digitToInt, isDigit)
import Data.Either ()
import Data.Foldable (Foldable (fold), find, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Ix (Ix (inRange))
import Data.List
  ( elemIndex,
    find,
    partition,
    sort,
    sortOn,
    transpose,
  )
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Deck (deal, shuffledDecks, sortedDeck)
import EitherIO (EitherIO (runEitherIO), liftEither, liftIO)
import GHC.Float (divideFloat, float2Int, int2Float)
import Parser.Instances
import Parser.Parser
import TwentyOne.Play
import TwentyOne.Rules
import TwentyOne.Types
import Utils (combineWith, firstJust, map', minus, update)

-- type alias for playCard return type - Action taken and the updated mem
type CompleteAction = (Action, String)

-- default actions
defaultCompleteAction :: CompleteAction
defaultCompleteAction = (defaultAction, "")

defaultPureAction :: PureAction
defaultPureAction = PStand

defaultAction :: Action
defaultAction = Stand

defaultBid :: Int
defaultBid = minBid

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
-- when the dealer's card is Nothing, it is a bidding turn
playCard Nothing _ _ _ mem _ = evalBid mem
-- when the dealer's card is not Nothing, it is an action turn
playCard (Just dealer) _ _ _ (Just mem) h = updateMemAndPlay mem dealer h
-- catches any errors, and defaults to Standing and resets memory
playCard _ _ _ _ _ _ = (Stand, "")


--------------------------------------------------
------Determining Action and Updating Memory------
--------------------------------------------------

-- function which is called during bidding turns to decide what to bid and what to store/add to memory
evalBid :: Maybe String -> CompleteAction
-- if this is the first round, memory is Nothing thus memory is simply the returned encoding of the current round
-- otherwise, append the encoding of the current round to the memory so far
evalBid strMem = case strMem of
  Nothing -> (Bid defaultBid, retMem)
  Just s -> (Bid $ bidAmt s, s ++ retMem)
  where
    retMem = storeStartOfRound strMem -- memory to add to the current memory
    -- helper function to determine the bid amount for that round
    -- if the current game is a simulation, use the simulation bid instead, otherwise use the default bid
    bidAmt :: String -> Int
    bidAmt s = if isSimulation $ getHistory s then simBid $ simSetup $ getHistory s else defaultBid

-- takes in the bid amount and serializes it (encodes it) to store in memory
-- / indicates the start of a new round, <number>B indicates the bid amount
startRoundStr :: Int -> String
startRoundStr bid = "/" ++ show bid ++ "B"

-- given the History of the entire game so far, determine if it is a simulation or not
-- made point free using function composition and eta reduction,
-- resulting function is more concise and is more understandable
isSimulation :: History -> Bool
isSimulation = (/= NoSetup) . simSetup

-- take in the current memory so far and add in the encoding for the current round,
-- involves storing the round separator '/' and the bid amount
storeStartOfRound :: Maybe String -> String
storeStartOfRound Nothing = startRoundStr defaultBid
storeStartOfRound (Just mem) = retStr
  where
    -- use pattern-matching to destructure the History datatype to easily access the data within this record syntax,
    --  used the @ notation to pass in the whole History to isSimulation while stil destructuring it
    h@(History sim _) = getHistory mem
    -- if it is a simulation, store the simulation bid otherwise store the defaultBid
    retStr =
      if isSimulation h
        then startRoundStr $ simBid sim
        else startRoundStr defaultBid

-- given the actions performed this round and the player hand, return a list of legal actions
legalActions :: RoundMem -> Hand -> [PureAction]
legalActions RoundM {roundActions, roundDealer} pHand
  | null roundActions = compiledActions
  | hitAfterDD roundActions = [PHit]
  | standAfterDD roundActions = [PStand]
  | otherwise = compiledActions
  where
    defaultActions = [PHit, PStand] -- always available for every turn
    -- uses list comprehension to impose conditions for each type of action
    includeInsurance = [PInsurance | roundDealer == 11 && null roundActions]
    includeDoubleDown = [PDoubleDown | length pHand == 2 && null roundActions]

    compiledActions = defaultActions ++ includeInsurance ++ includeDoubleDown

-- enforces hitting after doubling down
hitAfterDD :: [PureAction] -> Bool
hitAfterDD roundActions =
  (== PDoubleDown) (head roundActions) -- hit if the first action in that round is doubling down,
    && ( (length roundActions < 2) -- and there is only one action in this round,
    -- /this is for simulations where we add the additional condition that the second action is not a hit,
    -- to prevent it from continuously hitting/
           || (/= PHit) (head $ tail roundActions)
       )

-- enforces standing after doubling down and hitting
standAfterDD :: [PureAction] -> Bool
standAfterDD roundActions =
  (length roundActions == 2) -- stand if there has only been two actions,
    && (== PDoubleDown) (head roundActions) --the first action is a double down
    && (== PHit) (head $ tail roundActions) -- and the second action is a hit

-- add the dealer card and the player hand to the memory before performing action
encodeCurState :: History -> Card -> Hand -> String
encodeCurState hist@(History _ playerMem) dCard pHand
  -- if it is the first round of the game or if currently there are less than 2 cards in the hand,
  -- store dealer card and all cards in the player hand.
  -- player hand is serialized/encoded using show which is composed with getRank and fmapped to every card
  -- in the player hand, fold reduces this list of strings to a single strinc by concatenating all the strings
  | null playerMem || storeAll || lessThanTwoCards = dCardEncoding ++ fold (show . getRank <$> pHand)
  | otherwise = newCard -- otherwise, simply encode the newest card obtained (if any)
  where
    -- encode dealer and player card
    dCardEncoding = encodeCard dCard
    newCard = encodeNewCard hist pHand
    mostRecent = mostRecentRound playerMem
    lessThanTwoCards = length (roundHand mostRecent) < 2
    storeAll = (== 0) (roundBid mostRecent) && null (roundHand mostRecent)


-- take in memory add in any new cards
encodeNewCard :: History -> Hand -> String
encodeNewCard hist h
  | null roundMems || toUpdate = encodeCard $ head h
  | otherwise = ""
  where
    roundMems = playerMem hist
    -- updates if the length of the hand mismatches the length of the hand in the memory
    mostRecentHand = if null roundMems then [] else roundHand $ mostRecentRound roundMems
    toUpdate = length h /= length mostRecentHand


-- encode/serialize a card
encodeCard :: Card -> String
-- converts the card from the points represented by the card to the rank of the card
encodeCard card = case valToSymbol $ toPoints card of
  Just symbol -> [symbol] -- non-numeric cards
  Nothing -> show $ toPoints card -- numeric cards, rank is simply the string representation of the points


-- update the memory (with the dealer and player hand) before playing an action
updateMemAndPlay :: String -> Card -> Hand -> CompleteAction
updateMemAndPlay oldMem dCard pHand = completeAction
  where
    oldHist = getHistory oldMem
    updatedMem = oldMem ++ encodeCurState oldHist dCard pHand -- updates memory
    completeAction = playerAction updatedMem dCard pHand -- determines the action to take

-- predetermine moves for certain card combinations, if it falls
-- beyond these conditions, use the monte-carlo tree to determine action
predetermineMoves :: RoundMem -> String -> Card -> Hand -> Int -> Maybe CompleteAction
predetermineMoves rMem strMem dCard pHand bidAmt
  | invalidForPredetermination pHand rMem = Nothing -- cannot perform any predetermined action, use mcts
  | hVal >= 17 = Just (Stand, strMem ++ predetermineEncoding PStand)
  | hVal >= 13 = retVal standOrHit (2, 6)
  | hVal == 12 = retVal standOrHit (4, 6)
  | hVal <= 8 = Just (Hit, strMem ++ predetermineEncoding PHit)
  -- check if we can predtermine doubling down else use mcts
  | invalidForPredeterminedDoubleDown rMem = Nothing
  | hVal == 11 = Just (DoubleDown bidAmt, strMem ++ predetermineEncoding PDoubleDown)
  | hVal == 10 = retVal doubleDownOrHit (2, 9)
  | hVal == 9 = retVal doubleDownOrHit (3, 6)
  | otherwise = Nothing -- catches all other cases, use mcts
  where
    dealerVal = toPoints dCard
    hVal = sum $ calcHandVal pHand
    -- chooses the move based on the dealer range of values
    retVal :: ActionPicker -> Range -> Maybe CompleteAction
    retVal picker r = getCompleteActionFromRange picker strMem r dealerVal bidAmt

-- specify conditions to disallow predetermined doubling down
invalidForPredeterminedDoubleDown :: RoundMem -> Bool
invalidForPredeterminedDoubleDown rMem = notFirstTurn || moreThanTwoCards
  where
    -- cannot double down if not first turn
    notFirstTurn = not $ null (roundActions rMem)
    -- cannot double down if more than two cards
    moreThanTwoCards = length (roundHand rMem) > 2

-- specify the conditions that are invalid for predetermination, use mcts instead
invalidForPredetermination :: Hand -> RoundMem -> Bool
invalidForPredetermination pHand rMem =
  pair || softTotal || recentlyDD
  where
    pair = handIsPair pHand -- if the hand contains a pair
    softTotal = handIsSoft pHand -- check if the hand is a soft total (contains an Ace that)
    recentActions = roundActions rMem
    -- check if doubled down recently
    recentlyDD = elem PDoubleDown $ take 2 recentActions

-- based on a range of dealer values determine to choose between different actions,
-- as specified by the action picker
getCompleteActionFromRange :: ActionPicker -> String -> Range -> Int -> Int -> Maybe CompleteAction
getCompleteActionFromRange picker strMem r dealerVal bidAmt =
  flip (,) updatedMem <$> actionChosen
  where
    pureActionChosen = picker r dealerVal
    updatedMem = strMem ++ predetermineEncoding pureActionChosen
    actionChosen = pureActionToAction pureActionChosen bidAmt

type Range = (LowerBound, UpperBound)

type ActionPicker = Range -> Int -> PureAction

-- if dealer card value is in the given range, stand else hit
standOrHit :: ActionPicker
standOrHit valRange dVal = if inRange valRange dVal then PStand else PHit

-- if dealer card value is in the given range, double down else hit
doubleDownOrHit :: ActionPicker
doubleDownOrHit valRange dVal = if inRange valRange dVal then PDoubleDown else PHit

-- add the predetermined action to the memory following the format
predetermineEncoding :: PureAction -> String
predetermineEncoding pAct = join $ replicate 2 $ show pAct ++ "$"

-- determines action to take for Non-bid actions
playerAction :: String -> Card -> Hand -> CompleteAction
playerAction mem dCard pHand
  -- in a simulation and has a simulated action for that turn, perform fixed simulation action
  | isSimulation hist && hasSimulatedAction hist = (simAction, mem ++ simEncoding)
  -- in a simulation but does not have a simulated action for that turn,
  -- no need to update tree (will be updated outside of simulation), choose random actions
  | isSimulation hist && not (hasSimulatedAction hist) = (randomAction, mem ++ randActionEncoding)
  -- in a simulation but not the first turn, choose random actions
  | otherwise =
    if isNothing predetermineMove
      then normRes -- if no predetermined move, store memory data into tree, then get action from tree
      else fromMaybe defaultCompleteAction predetermineMove -- else use the action returned by predetermineMove
  where
    hist = getHistory mem
    -- get simulation action
    (simAction, simEncoding) = fromMaybe (defaultAction, "") $ safeSimAction hist pHand
    -- get random action if ran out of simulation actions or invalid sim action
    (randomAction, randActionEncoding) = getRandomAction hist pHand
    -- use Monte Carlo Tree Search to determine the action to perform
    normRes = mcts mem (playerMem hist) dCard pHand
    curRound = mostRecentRound $ playerMem hist
    predetermineMove = predetermineMoves curRound mem dCard pHand (roundBid curRound)

-- determine if there are any more simulated actions to perform
-- where the turn number corresponds to the simulated action to perform
hasSimulatedAction :: History -> Bool
hasSimulatedAction hist = turnNum < numSimActs
  where
    numSimActs = length $ simAction $ simSetup hist
    turnNum = getTurnNum hist


-- get random action for simulation
getRandomAction :: History -> Hand -> Encoding Action
getRandomAction history hand = (fromMaybe defaultAction randomAction, show pureRandAction)
  where
    curRound = mostRecentRound $ playerMem history
    -- generate a random seed from the current hand,
    -- use this pseudo-random seed to obtain a random action
    randSeed = getRandSeed curRound hand
    validActions = legalActions curRound hand -- to ensure the random action is legal
    pureRandAction = chooseRandomAction randSeed validActions
    randomAction = pureActionToAction pureRandAction $ roundBid curRound


-- get as different a seed as possible from the current hand and the memory of the current round
getRandSeed :: RoundMem -> Hand -> RandState
getRandSeed rMem h = length (roundActions rMem) * product (toPoints <$> h) + sum (length . show . getRank <$> h)


-- Obtain the action to perform using the Monte-Carlo Tree Search method
mcts :: String -> Memory -> Card -> Hand -> Encoding Action
mcts strMem mem dealerCard playerHand = fromMaybe (defaultAction, "") action
  where
    -- use do syntax to be able to extract data from the Maybe contexts from partial functions
    action = do
      let (cur : rest) = mem -- separate the current round's memory from the previous rounds' memory using pattern matchin
          validActs = getNodes (legalActions cur playerHand) True [PHit .. PInsurance]
          seed = getRandSeed cur playerHand

      -- store the previous rounds' memory in the empty MCT
      (updatedMCT, _) <- convertMemAndStoreToMCT rest

      -- get the three best actions from the tree, returns list of (Action, PureAction)
      -- we need Action to return an action to playCard, whereas we require the PureAction to store into memory
      threeBest <- getNActionFromMCT 3 dealerCard playerHand cur updatedMCT validActs
      let pureActions = snd <$> threeBest

      -- simulate the three best actions found
      allSimInfos <- simulateGivenLegalActions cur pureActions (seed, Just [])
      let gStates = convertDirectlyMemToGstate allSimInfos

      -- update the monte carlo tree with the simulation information
      (updatedSimMCT, _) <- updateMemoryInMCT gStates updatedMCT
      -- obtain the action to perform by finding the action with the largest associated win rate in the tree
      (finalAction, finalPureAction) <- getLargestWinRateAction cur dealerCard playerHand updatedSimMCT validActs

      -- update the memory with the simulation information, so that future turns have more data
      -- use bind on lists to call the function with each element and concatenate the lists at the end
      -- the encoding/serialization of the simulation information is defined in the show instance of RoundMem
      let updatedMemStr = strMem ++ "$" ++ tail (show =<< allSimInfos)
      -- update the memory with the chosen action
      addedChosenAction <- insertChosenAction updatedMemStr finalPureAction
      pure (finalAction, addedChosenAction ++ "$")

-- encode the chosen action to perform for this round to the memory in the appropriate position (see BNF explanation in report)
-- wrapped in Maybe in case it fails (for example no '$')
insertChosenAction :: String -> PureAction -> Maybe String
insertChosenAction strMem pAction = do
  -- split at the first instance of $ from the back
  -- insert the chosen action right before this $
  (reversedSim, dol : reversedRest) <- splitAtFirst '$' (reverse strMem)
  pure $reverse reversedRest ++ show pAction ++ [dol] ++ reverse reversedSim

-- choose the action which results in the largest win rate from the given tree for the current round
getLargestWinRateAction :: RoundMem -> Card -> Hand -> Tree -> [PureAction] -> Maybe (Action, PureAction)
getLargestWinRateAction rMem dCard pHand tree validActs = do
  -- use bind to unwrap the Zipper from the Maybe context, here we do not need the breadcrumbs,
  -- only the tree that is rooted in the node that has the highest win rate
  (wrTree, _) <- getLargestWRAction gameStateNode validNodeIds Nothing
  actionTreeToAction rMem wrTree -- converts the tree node to an action
  where
    validNodeIds = show <$> getNodes validActs True [PHit .. PInsurance]
    -- find the node representing the current game state and move down a level (see MCTS section in report)
    gameStateNode = moveLeft =<< findSubTree gameState tree
    gameState = getCurrentGameState dCard pHand

--gets valid nodes based on flag
getNodes :: [PureAction] -> Bool -> [PureAction] -> [PureAction]
getNodes pureActs flag valid = if flag then filter (`elem` valid) pureActs else pureActs

type RandomPair a = (RandState, a)

-- number of simulations to run for every action
simNum :: Int
simNum = 200

-- given the current round information and a list of actions to simulate, and performs n simulations for each given action
simulateGivenLegalActions :: RoundMem -> [PureAction] -> RandomPair (Maybe [SimInfo]) -> Maybe [SimInfo]
simulateGivenLegalActions rMem actionLst (prevSeed, simActionsSoFar)
  | null actionLst = simActionsSoFar
  | otherwise = recursiveCall $ accumulate simNum
  where
    recursiveCall = simulateGivenLegalActions rMem (tail actionLst)
    simulate num = simulateNTimes num rMem
    -- generates new pseudo-random numbers using the previous random number as the seed for this generation
    -- use liftA2 to lift the cons operator over two Maybe contexts
    -- to prepend the latest simulation action to all the previous simulation actions so far
    accumulate n = (nextRand prevSeed, liftA2 (:) (simulate n (head actionLst) prevSeed) simActionsSoFar)


-- get the best n actions from the monte-carlo tree based on the UCB formula
-- the best actions must be a legal action for that turn
getNActionFromMCT :: Int -> Card -> Hand -> RoundMem -> Tree -> [PureAction] -> Maybe [(Action, PureAction)]
getNActionFromMCT n dCard pHand roundMem tree validActs = action
  where
    gameState = getCurrentGameState dCard pHand --  convert from the current round information to a GameState
    possibleActionIDs = show <$> (getNodes validActs True [PHit .. PInsurance])
    action = do
      gameStateTree <- findSubTree gameState tree -- tree corresponding to the current game state
      let actionSpace = moveLeft gameStateTree -- move down a level to access the action space for that game state
      -- find the best n actions from this level, ensuring that the action chosen is legal
      chosenActionZippers <- getNActionOnLargestUCB n actionSpace possibleActionIDs []
      let actionTrees = getTree <$> chosenActionZippers
      -- flip the contexts inside out from a list of Maybes to a Maybe of lists using sequence
      sequence $ actionTreeToAction roundMem <$> actionTrees

type Encoding a = (a, String)

-- perform a simulation action safely (ensure that it is legal, otherwise perform a random legal action instead)
safeSimAction :: History -> Hand -> Maybe (Encoding Action)
safeSimAction hist pHand =
  case performSimAction hist of
    Nothing -> randomAction -- if the simulated fixed action is invalid choose randomly from the valid actions
    -- if the simulated action is one of the valid actions then perform that action otherwise perform a random action
    enc@(Just (act, _)) -> if any (elem act) validActions then enc else randomAction
  where
    rMem = if null $playerMem hist then mempty else mostRecentRound $ playerMem hist
    randomAction = Just $ getRandomAction hist pHand
    validActMapping = pureActionToAction <$> filter (/= PSplit) (legalActions rMem pHand)
    -- here I compose sequence twice, where the first sequence is applied to the list and function contexts,
    -- converts it to a list of functions which accept an integer to produce a Maybe Action
    -- to a function which accepts an integer and produces a list of Maybe Actions
    -- the second sequence then is applied to the list and Maybe contexts,
    -- converts it to a Maybe of a list of actions
    -- this is done to avoid repeatedly passing the same argument (roundBid rMem) to all the functions in the list,
    -- it makes it much more concise
    validActions = sequence . sequence validActMapping $ roundBid rMem

-- perform the fixed simulation action
performSimAction :: History -> Maybe (Encoding Action)
performSimAction hist = encoding setup
  where
    turnNum = getTurnNum hist
    setup = simSetup hist
    -- helper function to encode a SimSetup, to use pattern matching to produce different results
    -- fails if there is no simulation information
    encoding :: SimSetup -> Maybe (Encoding Action)
    encoding NoSetup = Nothing
    encoding (Setup bid act) = Just (fromMaybe defaultAction action, show pureAct)
      where
        -- get the simulation action corresponding to the current turn number
        pureAct = fromMaybe defaultPureAction $ getIthElem turnNum act
        action = pureActionToAction pureAct bid


-- point free implementation using function composition, makes it more concise and readable
getTurnNum :: History -> Int
getTurnNum = length . roundActions . mostRecentRound . playerMem

-- given a random number, pair up a random number with each of the given pure actions
-- sort based on this random number and get the first action
chooseRandomAction :: RandState -> [PureAction] -> PureAction
chooseRandomAction seed pureActions = snd $ head $ sortOn fst randPairs
  where
    randPairs = zip (generateRandSeq $ nextRand seed) pureActions

-- given a starting random seed, keep applying the nextRand function to the previous generated random seed and so on
-- this produces a lazy infinite sequence of random numbers generated in pure fashion by using the previous random
-- number as a seed to generate the next one.
-- also point free using eta-reduction to increase readability
generateRandSeq :: RandState -> [RandState]
generateRandSeq = iterate nextRand

--------------------------------------------------------
---------- PARSING MEMORY TO CUSTOM DATA TYPE ----------
--------------------------------------------------------

-- define PureAction as an algebraic data type,
-- the reason why I created a different datatype to Action is because for the monte-carlo tree
-- I am generally not interested in the bid amount, but simply the type of action performed.
-- using this custom datatype allows me to avoid having to pass around the bid, which will be unused,
-- to create a valid action for all my functions. With this I can succinctly work on the type of action instead.
data PureAction = PHit | PStand | PDoubleDown | PInsurance | PSplit
  deriving (Eq, Enum) -- derives Eq and Enum to be equality testable and enumerable

-- defines the serialization of PureAction, every pure action is succinctly represented by a single character
instance Show PureAction where
  show act = case act of
    PHit -> "H"
    PStand -> "S"
    PDoubleDown -> "D"
    PSplit -> "P"
    PInsurance -> "I"


-- to store outcome of round (win/draw/loss)
data Outcome = W | U | L deriving (Show, Enum, Eq, Ord)

-- determine the outcome of a game from the starting and end points
outcome :: Points -> Points -> Outcome
outcome prevP curP
  | curP > prevP = W
  | curP == prevP = U
  | otherwise = L


-- when parsing, to convert the string representation of some data to the actual data itself
-- instead of repeating is x >> pure X, for each of these, I define a mapping from the character representation
-- to the actual information that it represents, then we can traverse through this list and succinctly form
-- parsers for each of these different instances (see functions below)

actionMapping :: [(Char, PureAction)]
actionMapping = [('S', PStand), ('H', PHit), ('D', PDoubleDown), ('P', PSplit), ('I', PInsurance)]

handMapping :: [(Char, Int)]
handMapping = [('A', 11), ('T', 10), ('J', 10), ('K', 10), ('Q', 10)]

outcomeMapping :: [(String, Outcome)]
outcomeMapping = [("W", W), ("L", L), ("U", U)]

-- takes in the bid amount to produce a valid action from a pure action
pureActionMapping :: Points -> [(PureAction, Action)]
pureActionMapping p =
  [ (PStand, Stand),
    (PHit, Hit),
    (PDoubleDown, DoubleDown p),
    (PSplit, Split p),
    (PInsurance, Insurance $ div (fromIntegral p) 2)
  ]

-- converts an action to a pure action given a bid
-- fails if the given pure action does not exist
pureActionToAction :: PureAction -> Int -> Maybe Action
-- obtain the corresponding action for the matched pure action which is in the second element
pureActionToAction pAction bid = snd <$> pair
  where
    -- matches the pure action which is in the first element of each tuple
    pair = find ((== pAction) . fst) (pureActionMapping bid)

-- converts the string representation of a pure action to a pure action
-- fails if the given pure action does not exist
charToPureAction :: Char -> Maybe PureAction
charToPureAction c = snd <$> pair
  where
    pair = find ((== c) . fst) actionMapping

type CardInfo = (Char, Int)

defaultCardInfo :: CardInfo
defaultCardInfo = ('_', 0)

data OutcomeInfo = OutcomeInfo {numWins :: Int, numUndecided :: Int, numLosses :: Int}

-- defines the serialization for outcome info, importantly
-- condense x number of outcomes to <outcome>x
-- and if x is 0 don't add anything to the string at all
-- saves a lot of memory space especially for simulations (I perform 100s per turn)
instance Show OutcomeInfo where
  show OutcomeInfo {numWins, numUndecided, numLosses} =
    (if numWins == 0 then "" else "W" ++ show numWins)
      ++ (if numUndecided == 0 then "" else "U" ++ show numUndecided)
      ++ (if numLosses == 0 then "" else "L" ++ show numLosses)

-- define the semigroup instance for OutcomeInfo to define how two OutcomeInfo types are appended,
-- i.e. combining two Monoids (OutcomeInfo)
instance Semigroup OutcomeInfo where
  (<>) (OutcomeInfo w1 u1 l1) (OutcomeInfo w2 u2 l2) = OutcomeInfo (w1 + w2) (u1 + u2) (l1 + l2)

-- define the base/identity OutcomeInfo which, when mappended with another OutcomeInfo simply returns that OutcomeInfo
instance Monoid OutcomeInfo where
  mempty = OutcomeInfo 0 0 0

-- define a datatype to store the round information,
-- uses record syntax to be able to quickly access the various pieces of information,
-- with the automatically-created accessor functions
data RoundMem = RoundM
  { roundActions :: [PureAction], -- the actions performed that round
    roundHand :: [Int], -- the card values obtianed this round
    roundRanks :: [Rank], -- the ranks of the card obtained this round
    roundBid :: Int, -- the amount that the player bid
    roundDealer :: Int, -- the card value of the dealer's card that turn
    roundOutcome :: OutcomeInfo -- the compressed result of that round
  }

-- serializing/encoding the relevant round information into the memory
-- simulations are separated by | and we are only interested in recording the newest action (the simulated action)
-- and the outcome of that action
instance Show RoundMem where
  show RoundM {roundActions, roundOutcome} =
    "|" ++ show (fromMaybe defaultPureAction $ safeHead roundActions) ++ show roundOutcome


-- for a single round there should only be one dealer and bid, so just take the maximum dealer and bid amount
-- otherwise for actions, hand, and ranks concatenate them to the old list of actions
-- for the outcome use the mappend instance of OutcomeInfo to combine roundOutcomes
instance Semigroup RoundMem where
  (<>) (RoundM a1 h1 r1 b1 d1 out1) (RoundM a2 h2 r2 b2 d2 out2) =
    RoundM (a1 ++ a2) (h1 ++ h2) (r1 ++ r2) (max b1 b2) (max d1 d2) (out1 <> out2)

-- define the base/identity of the RoundMem datatype which when mappended to another RoundMem just returns that RoundMem
instance Monoid RoundMem where
  mempty = RoundM [] [] [] 0 0 mempty

-- define a type alias for a list of round memories, i.e. the list of memories of the entire game so far
type Memory = [RoundMem]

-- custom data type to store simulation information so that it performs predetermined actions with the predetermined bid amount
data SimSetup
  = NoSetup
  | Setup
      { simBid :: Int,
        simAction :: [PureAction]
      }
  deriving (Show, Eq)

-- custom data type to store the entire history of the game
data History = History
  { simSetup :: SimSetup,
    playerMem :: Memory
  }
  deriving (Show)

-- defines the semigroup instance of History to tell how to combine multiple Histories
-- there should only be one simulation setup so just take the first one,
-- impossible for one setup to be NoSetup and the other to be Setup,
-- and use the mappend instance defined for RoundMem to combine memories
instance Semigroup History where
  (<>) (History setup1 mem1) (History _ mem2) = History setup1 $ mem1 <> mem2

-- define the base/identity of the History datatype which when mappended to another History type just returns that History
instance Monoid History where
  mempty = History NoSetup []

-- given the memory string so far, parse it into the History datatype
getHistory :: String -> History
getHistory = extractFromParseResult mempty . parse parseHistory

-- parse the core of the memory - parses an action if that doesn't work parses a hand - i.e. cards
-- the reason why I do not use ||| here is because both parsers have different type results, thus
-- they need to be separated into cases like so
parseMem :: Parser RoundMem
parseMem =
  P
    ( \i -> case parse parseMemAction i of --parse action
        Result rest res -> Result rest $ memInitializeAction res
        Error _ -> case parse parseHand i of --if that doesn't work parse for a hand
          Result rest res -> Result rest $ memInitializeHand res
          Error e -> Error e
    )

-- parses the simulation identifier
parseSim :: Parser Char
parseSim = is '?'

-- if the sim identifier was successfully parsed this means that the current game is a simulation
-- parse the simSetup and then parse the rest of the string (which should be RoundMems as in a normal game)
parseHistory :: Parser History
parseHistory =
  P
    ( \s -> case parse parseSim s of
        Result rest _ ->
          case parse parseSimSetup rest of
            -- use looseParseAll because in a simulation memory can never be Nothing,
            -- because we have passed in some memory when initializing it
            -- therefore we need to parse the memory even before the first bid round thus there are no / (start of round) separators yet
            Result rest2 parsed -> History parsed <$> parse looseParseAll rest2
            Error e -> Error e
        -- not in a simulation, strictParseAll makes sure that there is always at least one / (start of round) separator
        -- because otherwise memory should be empty (Nothing) and should not yet be parsed
        Error _ -> History NoSetup <$> parse strictParseAll s
    )

-- parse the information regarding the simulation - the bid and actions to perform
-- and generates a SimSetup from this data
parseSimSetup :: Parser SimSetup
parseSimSetup = do
  bid <- parseNumAndSeparator 'B' -- parses the bid amount and the separator
  simActions <- list1 parseMemAction -- uses list1 to ensure that there is atleast one action to parse
  pure $ Setup bid simActions

-- parses rounds in a simulation, allows 0 or more rounds, denoted by / separators (constrast with strictParseAll)
looseParseAll :: Parser Memory
looseParseAll = parseAll list -- for simulations

-- parses rounds in a non-simulation game, allows 1 or more rounds
strictParseAll :: Parser Memory
strictParseAll = parseAll list1

-- takes a list parser (either strict or loose) and parses the whole memory
parseAll :: (Parser [RoundMem] -> Parser [[RoundMem]]) -> Parser Memory
parseAll f = do
  l <- f (is '/' >> parseRoundInfo) -- parses the round separator, ignores the parsed result and parses the rest of the memory
  -- join to concatenate the list of lists
  -- reverse so that latest round is at the head, convenient as most functions are interested in the most recent/current memory
  pure $ reverse $ join l

-- uses the derived Enum instance for rank to generate a mapping for the string representation to the actual Rank type
-- produces : [('A',Ace), ('2', Two), ...]
charRankMapping :: [(Char, Rank)]
charRankMapping = flip zip [Ace ..] $ head . show <$> [Ace ..]

-- converts the given character to a Rank datatype, since it can fail if the provided character is not found in the list
-- wrap the return type in the Maybe context
charToRank :: Char -> Maybe Rank
charToRank str = snd <$> pair
  where
    pair = find ((== str) . fst) charRankMapping

-- given a pure action initialize the RoundMem with the given pure action,
-- by making use of the mempty we defined in the Monoid instance of RoundMem,
--  and the record syntax of RoundMem to do this succinctly
memInitializeAction :: PureAction -> RoundMem
memInitializeAction act = mempty {roundActions = [act]}

-- initialize the hand of the player that round given information about the card (its rank, value)
memInitializeHand :: CardInfo -> RoundMem
memInitializeHand (str, val) = resultingMem $ charToRank str
  where
    -- auxiliary function to use pattern matching to default to mempty if the rank is not found
    resultingMem :: Maybe Rank -> RoundMem
    resultingMem Nothing = mempty
    resultingMem (Just r) = mempty {roundHand = [val], roundRanks = [r]}

-- point-free, initializes the hand, actions, and ranks as empty,
--  and takes in the rest of the information as arguments to build a RoundMem
memInitializeRound :: Points -> Int -> OutcomeInfo -> RoundMem
memInitializeRound = RoundM [] [] []

-- parse using the given parser 0 or more times
-- uses pure mempty to default to a parser which always suceeds for any input
-- and produces mempty (the default RoundMem value)
-- difference with list is that the return type is not in a list
-- instead they are combined using the mappend definition for that type
parseMemList :: Parser RoundMem -> Parser RoundMem
parseMemList p1 = parseMemList1 p1 ||| pure mempty

-- parse using the given parser multiple times, must suceed at least once
-- difference with list1 is that it combines the resulting output using the
-- mappend defintion for that type
parseMemList1 :: Parser RoundMem -> Parser RoundMem
parseMemList1 p = do
  p' <- p -- parse once using the parser
  p'' <- parseMemList p -- parse 0 or more times using the parser
  pure (p' <> p'') -- combine the results

-- parse a single round's memory with parseMemList and
-- wrap the resulting parsed RoundMem into a list within a list
parseLMemList :: Parser RoundMem -> Parser [[RoundMem]]
parseLMemList p1 =
  P
    ( \str -> case parse (parseMemList p1) str of
        Result rest res -> Result rest [[res]]
        Error e -> Error e
    )

-- parse a list of digits (0 or more) and the given separator (in that order)
-- ignore the parsed result of parsing the separator using <* and apply the appendNum
-- function to the list of integers wrapped in the Parser context using fmap instance of Parser
-- to concatenate them to form a number
parseNumAndSeparator :: Char -> Parser Int
parseNumAndSeparator c = appendNum <$> list parseDigit <* is c

-- parses PureActions + Cards from the memory string, does not deal with separators
parseRoundInfo :: Parser [RoundMem]
parseRoundInfo = do
  bid <- parseNumAndSeparator 'B' -- parses the bid amount
  -- parses the dealer card, for the dealer card we are only interested in the card value, not the card rank
  (_, dealerVal) <- parseHand ||| pure defaultCardInfo
  simMem <- parseMemAndSim -- parse the sim information and the (non-sim) round information
  -- mappend each simulation memory with the (non-simulation) round memory
  -- containing the bid amount and dealer card by fmapping it to the result of parsing the simulation memory
  pure $ (<> memInitializeRound bid dealerVal mempty) <$> simMem

-- parse both the information from the non-simulation round in front and then the simulation information afterwards,
-- this must succeed once, if this fails just parse the non-simulation round's information
parseMemAndSim :: Parser [RoundMem]
parseMemAndSim = do
  -- parse both the information from the non-simulation round in front and then the simulation information afterwards,
  -- this must succeed once, if this fails just parse the non-simulation round's information
  allParsed <- list1 parseConfig ||| parseLMemList parseMem
  -- use the non-simulation round info to add to the simulation info to recreate
  -- the condition of the simulations (see report)
  let folded = scanl1 (map . accumulatePrevious . last) allParsed
  pure $ join folded

-- parses the non-simulation information then the simulation information afterwards
-- for a single round
parseConfig :: Parser [RoundMem]
parseConfig = do
  nonSimMem <- parseMemList parseMem -- parse non-simulation information
  simMem <- parseSimOutcomes nonSimMem -- parse simulation information
  -- append the non-simulation memory to the simulation memory, this is important to extract the chosen
  -- action for a particular round (in parseAll we reverse it so that the round information is the first
  -- element in the Memory list, thus we can easily use pattern matching to extract it)
  pure $ simMem ++ [nonSimMem]

-- parses the simulation outcomes encoded between the separator $, and separated within by |
parseSimOutcomes :: RoundMem -> Parser Memory
parseSimOutcomes roundMem = do
  -- parse the simulation information, the region containing the simulation information
  -- is denoted by opening and closing $, and different simulations are encoded separated by |
  -- within this region, note this can fail for ex: parsing the memory at the beginning of the round
  -- there is not yet any simulation information for that round, thus default to a parser that
  -- always succeeds and simply returns an empty list
  outcomes <- betweenSepbyChar '$' '|' parseSimOutcomeInfo ||| pure []
  -- since each simulation enclosed in the $ is based off the same round, they share the same
  -- bid, dealer, and past actions
  -- replicate the non-simulation RoundMem for every simulation
  -- and pair up the simulation outcomes information with the non-simulation round memory
  let compiledResult = zip outcomes $ replicate (length outcomes) roundMem
      -- recreate the round memory for each simulation, use uncurry to take the first element of the tuple as the first argument
      -- and the second element of the tuple as the second argument
      updatedRes = map (uncurry recreateRoundMem) compiledResult
  if null updatedRes then failed UnexpectedEof else pure updatedRes

type SimResult = (PureAction, OutcomeInfo)

-- recreate the round memory for when the simulation occurred
-- for ex. if the simulation was run when the player had not done any actions yet
-- even if the player has now done multiple actions,
-- we need to recreate the situation when the simulation was ran
recreateRoundMem :: SimResult -> RoundMem -> RoundMem
-- copy the given RoundMem except with updated roundActions and roundOutcome
recreateRoundMem (simAction, outcomeInf) rMem =
  rMem
    { -- to recreate the conditions when the simulation ran, replace the last action performed with the simulation action
      roundActions = replaceLast simAction $ roundActions rMem,
      -- combine the outcome of the current round with the simulation outcome
      roundOutcome = outcomeInf <> roundOutcome rMem
    }

-- simulation information is encoded as <action><outcome>, therefore we first parse the action that we simulated
-- then we parse the outcome information for the simulations ran with that action
parseSimOutcomeInfo :: Parser SimResult
parseSimOutcomeInfo = do
  action <- parseMemAction
  outcomeInf <- list parseOutcomeInfo
  pure (action, accumulateOutcomeInfo outcomeInf)

-- since to condense the memory we only store the simulated action, to recreate the conditions when the simulation ran,
-- i.e. the round memory at that point we also need to include the actions taken, cards obtained up to that point
accumulatePrevious :: RoundMem -> RoundMem -> RoundMem
accumulatePrevious rMem1 rMem2 =
  rMem2
    { roundActions = roundActions rMem1 ++ roundActions rMem2,
      roundHand = roundHand rMem1 ++ roundHand rMem2,
      roundOutcome = roundOutcome rMem2, --take only the simulation outcome
      roundRanks = roundRanks rMem1 ++ roundRanks rMem2
    }


-- deserializes the outcome information which is in the form <outcomeType><num>
-- and converts it to the custom datatype OutcomeInfo
parseOutcomeInfo :: Parser OutcomeInfo
parseOutcomeInfo = do
  rOutcome <- parseOutcome -- parses the type of outcome, 'W', 'U', or 'L'
  digits <- list parseDigit -- parses a list of digits representing the number of times that outcome occurred
  let combinedDigit = appendNum digits -- concatenate the digits to form a single number
  pure $ createOutcomeInfo rOutcome combinedDigit

-- combine a list of OutcomeInfos to a single OutcomeInfo
accumulateOutcomeInfo :: [OutcomeInfo] -> OutcomeInfo
accumulateOutcomeInfo = mconcat

-- copies the deault mempty and changing the appropriate number for that outcome with the given number
createOutcomeInfo :: Outcome -> Int -> OutcomeInfo
createOutcomeInfo out n = case out of
  W -> mempty {numWins = n}
  L -> mempty {numLosses = n}
  U -> mempty {numUndecided = n}

-- map the stringConverter over the list of outcome mappings that we defined earlier,
-- and fold over the list of parsers connecting the Parsers with |||
parseOutcome :: Parser Outcome
parseOutcome = foldr1 (|||) $ converterMapping stringConverter outcomeMapping

-- parses the given string ignores the output and instead return the given element
stringConverter :: Converter String a a
stringConverter str r = traverse_ is str >> pure r

-- replace hand (only occurs if there is an ace and it flips from 11 to 1)
replaceLatestHand :: RoundMem -> [Int] -> RoundMem
replaceLatestHand mem newHand = mem {roundHand = newHand}

-- converts from the card value to the serialization/encoding of the card
valToSymbol :: Int -> Maybe Char
valToSymbol val = fst <$> pair
  where
    -- finds the tuple where the value in the second element of the tuple matches the given value
    pair = find ((== val) . snd) handMapping

-- folds over the list of PureAction parsers using ||| so that the resulting parser
-- can parse any valid card, for ex. Parser ('S', PStand) ||| Parser ('H', PHit) ||| ...
parseMemAction :: Parser PureAction
parseMemAction = foldr (|||) (head pureAction) pureAction

-- a valid hand is either a special card, which is non-numeric, or has a numeric hand
parseHand :: Parser CardInfo
parseHand = parseSpecialHand ||| parseNumericHand

-- folds over the list of CardInfo parsers using ||| for non numeric ranked cards, so that the resulting parser
-- can parse any valid card, for ex. Parser ('A',11) ||| Parser ('T', 10) ||| ...
parseSpecialHand :: Parser CardInfo
parseSpecialHand = foldr (|||) (head handVal) handVal

-- map isAction to actionMapping where character is first element of the tuple and
-- PureAction is the second element
pureAction :: [Parser PureAction]
pureAction = convertTypeChar actionMapping

-- parses the given character, and return a tuple
-- with the given character and the given item
pairConverter :: Char -> b -> Parser (Char, b)
pairConverter c r = is c >> pure (c, r)

-- produces a list of CardInfo parsers for each of the possible card values
-- for ex, [Parser ('A', 11), Parser ('T', 10), ..] where the first Parser
-- parses 'A' and returns the tuple ('A', 11) - the character encoding of its rank, and the value of the card
handVal :: [Parser CardInfo]
handVal = convertToPair handMapping

-- set converterMapping types to specifically work with characters as the first element of the tuple
-- and to produce a parser that parses that produces a tuple with that character
-- and the second element of the tuple
convertToPair :: [(Char, a)] -> [Parser (Char, a)]
convertToPair = converterMapping pairConverter

-- set converterMapping types to specifically work with characters as the first element of the tuple
-- and to produce a parser that parses that produces the second element of the tuple
convertTypeChar :: [(Char, a)] -> [Parser a]
convertTypeChar = converterMapping charConverter

type Converter a b c = a -> b -> Parser c

-- generic function which takes in a Converter function, which generally
-- checks if the input string matches the first element in the tuple, if so then use the
-- second element of the tuple to create a Parser for some type c
converterMapping :: Converter a b c -> [(a, b)] -> [Parser c]
-- maps the converter function to the list, i.e. matches with every tuple pair in the list
-- uses uncurry to take the first element of the tuple as the first argument, and the
-- second element of the tuple as the second argument
converterMapping f l = uncurry f <$> l


-- parse cards which have a numeric rank
parseNumericHand :: Parser (Char, Int)
parseNumericHand = do
  digit <- parseDigit -- parses the digit
  pure (head $ show digit, digit) -- returns the string encoding of that digit with the parsed digit itself

-- parse a digit and convert it from a character to an integer
parseDigit :: Parser Int
-- <&> is the flipped version of <$>
-- takes context then function
parseDigit = satisfy isDigit <&> digitToInt

-- given a character and some item r, if the next character in the input is c
-- then return a parser which always succeeds and outputs the given item r
charConverter :: Converter Char a a
charConverter c r = is c >> pure r

-- parser which parses the next character but only if it satisfis the given function
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- character
  if f c then pure c else unexpectedCharParser c


-- produces a non-empty list of values after applying the given parser, it must succeed at least once
-- and is separated by the second parser
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 sep = p1 >>= \i -> list (sep >> p1) >>= \i' -> pure (i : i')

-- a function that produces a list of values after applying the given parser,
-- separated by the second parser
sepby :: Parser a -> Parser s -> Parser [a]
sepby p sep = sepby1 p sep ||| pure []

-- Parser that continues producing a list of values from the given parser
list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

-- Parser that continues producing a list of values from the given parser, must succeed at least once
list1 :: Parser a -> Parser [a]
list1 p = do
  p' <- p
  p'' <- list p
  pure (p' : p'')


-- parses an opening and closing separator, ignore them
-- and parse the contents between the separators.
between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x


-- takes two characters which represent the opening and closing separator
-- to parse and ignore, then parse the contents between the two separators
-- where the contents are separated by some character
betweenSepbyChar :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyChar c1 separator parserA = between (is c1) (is c1) (sepby parserA (is separator))


-- evaluates the value of aces, if the given hand sums to over 21, that means it must contain a soft ace
-- because otherwise would have busted convert the last of such aces to a 1.
evaluateAce :: [Int] -> Maybe [Int]
evaluateAce h = if sum h > 21 then condReplaceLast h 11 1 else Just h


-- takes a ParseResult and a default value to fall back on,
-- gets the result or returns the default value
extractFromParseResult :: a -> ParseResult a -> a
extractFromParseResult def (Error _) = def
extractFromParseResult _ (Result _ result) = result


------------------------------------------
----------------MCTS SETUP----------------
------------------------------------------


-- all monte-carlo related information as a record syntax for easy access with the accessor functions
data MCInfo = MCInfo
  { wins :: Float,
    numSimsThisNode :: Int,
    numSimsSubtreeTotal :: Int
  }
  deriving (Eq)

type NodeID = String

-- node information, including the node id which represents some part of the game state which the node represents
data NodeInfo = NodeInfo
  { nid :: NodeID,
    mcInfo :: MCInfo -- the monte-carlo related information for that node
  }
  deriving (Eq)

-- the node/tree's left and right child
data Children = Children {left :: Tree, right :: Tree} deriving (Eq)

-- Tree is either None, or a node that has children (could be None if it is a leaf),
-- and information relating to that node
data Tree
  = None
  | Node
      { children :: Children,
        nodeInfo :: NodeInfo
      }
  deriving (Eq)

defaultMCInfo :: MCInfo
defaultMCInfo = MCInfo 0 0 0

noChildren :: Children
noChildren = Children None None

-- stores relevant information about the hand, the total value,
-- if it has an ace (soft total) and whether it has a pair or not
data HandInfo = HandInfo
  { hasAce :: Bool,
    hasPair :: Bool,
    val :: Int
  }

-- game state contains the relevant information about the round in terms of the monte-carlo tree
data GameState = GState
  { curDealerCard :: Int, -- the value of the dealer's card
    cardNum :: Int, -- the number of cards in the player's hand
    curHand :: HandInfo, -- information regarding the player's hand
    actionTaken :: Maybe PureAction, -- an action, if this is not the current round, else Nothing
    gameOutcome :: OutcomeInfo -- outcome for that round, if it exists
  }



-- checks if the user has performed split during this turn
performedSplit :: RoundMem -> Bool
performedSplit = elem PSplit . roundActions


-- convert rank to val considering the sum of the hand (Aces might flip)
rankToValDeck :: Int -> Rank -> Int
rankToValDeck hVal r
  | hVal > 10 && indVal == 11 = 1
  | otherwise = indVal
  where
    indVal = rankToVal r

-- purely convert rank to val without considering flipping Aces
rankToVal :: Rank -> Int
rankToVal r
  | isDigit $ head strRank = read strRank
  | strRank == "A" = 11
  | otherwise = 10
  where
    strRank = show r

-- convert all previous rounds in memory to gamestate
-- (importantly it should not be passed in the current round info)
memoryToGameState :: Memory -> Maybe [[GameState]]
memoryToGameState = mapM roundMemConversion
  where
    roundMemConversion cur = roundMemToGameState cur (Just []) []

-- to convert a single round to multiple game states, doesnt do it for current rount (see getCurrentGameState for this)
roundMemToGameState :: RoundMem -> Maybe [GameState] -> [Int] -> Maybe [GameState]
roundMemToGameState _ Nothing _ = Nothing
roundMemToGameState rMem _ [] = roundMemToGameState rMem (Just []) (take 2 $ roundHand rMem) -- start off with an empty accumulated game states list
roundMemToGameState RoundM {roundActions = []} gStateSoFar _ = gStateSoFar -- processed all the actions and all cards
roundMemToGameState rMem@RoundM {roundActions, roundHand, roundDealer, roundOutcome} gStateSoFar handForAction
  | performedSplit rMem = Just [splitGState]
  | otherwise = roundMemToGameState rMem {roundActions = rest} updatedGStateSoFar newHandForAction
  where
    -- encode hand for split
    splitCards = take 2 roundHand
    splitHandInfo = HandInfo (isSoft splitCards) True (sum splitCards) -- encodes the hand information
    splitGState = GState roundDealer (length splitCards) splitHandInfo (Just PSplit) roundOutcome
    -- for non-split hand
    (action : rest) = roundActions -- isolate the first action out of the list of round actions to perform
    adjustForAce = fromMaybe handForAction (changeAce $ Just handForAction) -- adjust the hand for aces
    newHandForAction = generateNewHandForAction handForAction action roundHand -- generate new hand that can be used for converting the next action
    newGameState = generateGameState handForAction roundDealer action rMem roundOutcome
    updatedGStateSoFar :: Maybe [GameState] -- update the game states generated so far
    updatedGStateSoFar = do
      state <- gStateSoFar
      pure $ if sum adjustForAce > 21 then state else newGameState : state


-- store the current game state to a resulting tree, updates the tree
updateMemoryInMCT :: [GameState] -> Tree -> Maybe Zipper
updateMemoryInMCT gStates startT = storeGameStateInMCT gStates (Just (startT, []))

-- stores the previous round information to the base/empty Monte-Carlo tree
convertMemAndStoreToMCT :: Memory -> Maybe Zipper
convertMemAndStoreToMCT mem = do
  prevRoundGameStates <- memoryToGameState mem -- convert from memory to game states to store it in the tree
  storeMemoryInMCT prevRoundGameStates


-- helper function of roundMemToGameState, which generates the next useable hand, for the next action
generateNewHandForAction :: [Int] -> PureAction -> [Int] -> [Int]
generateNewHandForAction prevHand act roundHand = prevHand ++ take cardsObtained (drop numCards roundHand)
  where
    numCards = length prevHand
    cardsObtained = numCardObtainedFromAction act

-- helper function of roundMemToGameState, which generates the game state for that particular action
generateGameState :: [Int] -> Int -> PureAction -> RoundMem -> OutcomeInfo -> GameState
generateGameState handForAction dVal act rMem = GState dVal (length handForAction) handInfo (Just act)
  where
    adjustForAce = fromMaybe handForAction (changeAce $ Just handForAction)
    handInfo = HandInfo (isSoft handForAction) (isPair rMem) (sum adjustForAce)

-- converts aces, if hand sum up to more than 21, replace the last Ace value with a 1
changeAce :: Maybe [Int] -> Maybe [Int]
changeAce Nothing = Nothing
changeAce (Just l)
  | sum l > 21 = changeAce $ condReplaceLast l 11 1
  -- if there is a 1 in the hand, and even if it becomes a soft 11, the total is less than 21, convert it to 21
  -- this can only occur when we're trying to recreate the state for each action we are considering a portion of the hand at a time
  -- and not the whole hand
  | elem 1 l && (sum l -1 + 11) <= 21 = changeAce $ condReplaceLast l 1 11
  | otherwise = pure l

-- the number of cards obtained from performing the given pure action
numCardObtainedFromAction :: PureAction -> Int
numCardObtainedFromAction PHit = 1
numCardObtainedFromAction _ = 0

-- convert state for the current round to GameState
getCurrentGameState :: Card -> Hand -> GameState
getCurrentGameState dCard pHand = GState dCardVal handLen handInfo Nothing mempty
  where
    dCardVal = rankToValDeck 0 (getRank dCard) -- obtain the value of the dealer card
    handLen = length pHand
    -- encode the hand information,
    -- if it is a soft total, if the hand contains a pair, and the sum of the values of the hand
    handInfo = HandInfo (handIsSoft pHand) (handIsPair pHand) (handCalc pHand)

-- calculate the value of a hand of cards
calcHandVal :: Hand -> [Int]
-- take aces into account
calcHandVal h = fromMaybe [] $ evaluateAce defaultEval
  where
    -- get the ranks for each card, convert from rank to the corresponding value
    defaultEval = rankToValDeck 0 . getRank <$> h

-- determine whether the hand total is a soft total
-- (contains an Ace which is currently an 11 but can flip to a 1 if the hand total eventually exceeds 21)
handIsSoft :: Hand -> Bool
handIsSoft = elem 11 . calcHandVal

-- determines whether the list of values contains an 11 (ace that can flip its value to a 1)
isSoft :: [Int] -> Bool
isSoft = elem 11

-- determines whether the hand is a pair
handIsPair :: Hand -> Bool
handIsPair h = length h == 2 && getRank c1 == getRank c2
  where
    [c1, c2] = h

-- takes the round memory and determines if the hand for that round contains a pair,
-- have to have exactly 2 cards and same rank and same value
isPair :: RoundMem -> Bool
isPair RoundM {roundActions, roundHand, roundRanks} = length roundHand == pairLen && c1 == c2
  where
    numSplits = count PSplit roundActions
    pairLen = 2 + numSplits
    [c1, c2] = drop numSplits $ zip roundHand roundRanks


-- give a name to head Memory so its not confusing (this gets the recent memory, which is the memory of the current round)
mostRecentRound :: Memory -> RoundMem
mostRecentRound = head

---------------------------------------------------
-------------------- MCTS TREE --------------------
---------------------------------------------------

-- Reference: http://learnyouahaskell.com/zippers

-- encode either a left or a right breadcrumb, representing whether we followed the left or right subtree to reach the current node
-- also stores the node information of the parent to be able to recreate it on the way up as well as the other tree (which was not traversed)

data Crumb = LeftCrumb NodeInfo Tree | RightCrumb NodeInfo Tree
  deriving (Show)

type Breadcrumbs = [Crumb]

-- define the type alias Zipper which contains a tree and a list of crumbs to retrace the path taken to reach that tree
type Zipper = (Tree, Breadcrumbs)

defaultZipper :: Zipper
defaultZipper = (None, [])

-- obtain the tree from the zipper
getTree :: Zipper -> Tree
getTree (tree, _) = tree

-- move left from the current tree and store the movement by prepending a LeftCrumb to the breadcrumbs so far
-- we save the node info of the parent before moving left and we also save the right child of this parent tree
-- so that we can recreate the parent node later on when we go back up/retrace our path
-- note: if left child is empty, does not fail, instead returns Just (None, ...)
moveLeft :: Zipper -> Maybe Zipper
moveLeft (None, _) = Nothing -- fail for empty tree
moveLeft (Node (Children l r) info, bs) = Just (l, LeftCrumb info r : bs)

-- move right from the current tree and store the movement by prepending a RightCrumb to the breadcrumbs so far
-- we save the node info of the parent before moving right and we also save the left child of this parent tree
-- so that we can recreate the parent node later on when we go back up/retrace our path
-- note: if right child is empty, does not fail, instead returns Just (None, ...)
moveRight :: Zipper -> Maybe Zipper
moveRight (None, _) = Nothing -- fail for empty tree
moveRight (Node (Children l r) info, bs) = Just (r, RightCrumb info l : bs)

-- retrace the path taken, go backwards once based on the breadcrumbs
moveUp :: Zipper -> Maybe Zipper
moveUp (_, []) = Nothing -- no breadcrumbs to follow -> fail
-- if the latest action was moving left from the parent, then the current node must be the right child of that parent
-- thus recreate the parent from the info stored in the Zipper and attach the current node to its right
-- we have essentially moved up the tree
moveUp (t, LeftCrumb info r : bs) = Just (Node (Children t r) info, bs)
-- if the latest action was moving right from the parent, then the current node must be the left child of that parent
-- thus recreate the parent from the info stored in the Zipper and attach the current node to its left
-- we have essentially moved up the tree
moveUp (t, RightCrumb info l : bs) = Just (Node (Children l t) info, bs)

-- move all the way back up to the root/start of the tree
returnToRoot :: Maybe Zipper -> Maybe Zipper
returnToRoot (Just (t, [])) = Just (t, []) -- no more breadcrumbs to follow, we have reached the top, simply return the zipper
returnToRoot (Just z) = returnToRoot (moveUp z) -- recursively call returnToRoot, each time move up a level
returnToRoot Nothing = Nothing

-- takes in a function which modifies the monte-carlo related information of that node
-- and applies it to the tree/node in the passed in zipper
modifyNode :: (MCInfo -> MCInfo) -> Zipper -> Maybe Zipper
modifyNode f (Node c (NodeInfo nid mcInfo), bs) = Just (Node c (NodeInfo nid (f mcInfo)), bs)
modifyNode _ _ = Nothing

-- finds a node at the same level as the node given in the provided zipper
findAtLevel :: NodeID -> Zipper -> Maybe Zipper
findAtLevel nid z@(t, _)
  | found = Just z --  if found, return the zipper
  | not found = moveRight z >>= findAtLevel nid -- otherwise move right
  where
    found = foundNode t nid
findAtLevel _ _ = Nothing

-- increment number of wins for that node (action) by the given amount
incrementMCWins :: Float -> MCInfo -> MCInfo
incrementMCWins n info = info {wins = wins info + n}

-- increment number of simulations performed for that node (action) by the given amount
incrementNumSims :: Int -> MCInfo -> MCInfo
incrementNumSims n info = info {numSimsThisNode = numSimsThisNode info + n}

-- increment number of simulations performed in total for nodes in the same level by the given amount
incrementNumSimsLevel :: Int -> MCInfo -> MCInfo
incrementNumSimsLevel n info = info {numSimsSubtreeTotal = numSimsSubtreeTotal info + n}

-- attach the given tree as the left child of the tree in the given zipper
attachLeft :: Tree -> Zipper -> Maybe Zipper
attachLeft t (Node (Children None r) info, bs) = Just (Node (Children t r) info, bs)
attachLeft _ _ = Nothing

-- attach the given tree as the right child of the tree in the given zipper
attachRight :: Tree -> Zipper -> Maybe Zipper
attachRight t (Node (Children l None) info, bs) = Just (Node (Children l t) info, bs)
attachRight _ _ = Nothing

-- stores all memory (in the form of GameStates) in the MCT
storeMemoryInMCT :: [[GameState]] -> Maybe Zipper
-- fold from the right because in the memory, the left-most element is the most recent round
-- store the oldest to the most recent memory in order
storeMemoryInMCT = foldr storeGameStateInMCT (Just (mct, []))

-- store a single game state in the given tree
storeGameStateInMCT :: [GameState] -> Maybe Zipper -> Maybe Zipper
storeGameStateInMCT [] zipper = zipper -- if memory is empty, base case return accumulator
storeGameStateInMCT (cur : rest) (Just (t, _)) = do
  -- find the node/tree that represents this game state in the tree in the zipper
  zipperForThisMem <- findSubTree cur t

  let (OutcomeInfo w u l) = gameOutcome cur
      totalSims = w + u + l
  -- modify the total number of simulations performed for this found node
  updatedNodeSims <- modifyNode (incrementNumSims totalSims) zipperForThisMem

  --modify the number of wins for this particular action
  let rewardFunc = rewardIncrement $ gameOutcome cur
  updatedWins <- modifyNode rewardFunc updatedNodeSims

  -- then modify the total number of simulations in that level for all the nodes in the same level as the found node
  modifiedLevel <- modifyAllInLevel updatedWins (incrementNumSimsLevel totalSims)

  -- move all the way back up (return to the root of the tree), along the way while backtracking we attach our updated nodes
  let updatedTree = returnToRoot $ pure modifiedLevel
  storeGameStateInMCT rest updatedTree -- recurse to repeat this process for all gamestates
storeGameStateInMCT _ _ = Nothing

-- increment the number of wins for a node, where wins=1 win, draws=0.5 wins, and loss=0 wins
rewardIncrement :: OutcomeInfo -> (MCInfo -> MCInfo)
rewardIncrement (OutcomeInfo w u _) = incrementMCWins $ fromIntegral w + mult drawWeight u
  where
    drawWeight :: Float
    drawWeight = 0.5


-- given the game state find the tree/node in the monte-carlo tree that represents that game state
findSubTree :: GameState -> Tree -> Maybe Zipper
-- if no action, this is the current turn
-- get the node representing the dealer card value + card number + hand total
findSubTree (GState d num hInfo Nothing _) t = do
  cardNumNode <- getCardNumNode d num t
  findAtLevel (show hInfo) =<< moveLeft cardNumNode
-- if action exists, this is a past turn
-- get the node representing the dealer card value + card number + hand total + action taken
findSubTree (GState d num hInfo (Just act) _) t = do
  cardNumNode <- getCardNumNode d num t
  cardValNode <- findAtLevel (show hInfo) =<< moveLeft cardNumNode
  findAtLevel (show act) =<< moveLeft cardValNode

-- get the node representing the dealer value + number of cards in the player's hand
getCardNumNode :: Int -> Int -> Tree -> Maybe Zipper
getCardNumNode dVal cardNum t = do
  let startZ = (t, []) -- start from the root of the given tree, initialize breadcrumbs as an empty list
  dealerZ <- findAtLevel (show dVal) startZ -- find the node representing that dealer value
  findAtLevel (show cardNum) =<< moveLeft dealerZ -- find the node representing the number of cards in the player's hand

-- convert from a node representing an action to an Action and its corresponding PureAction
actionTreeToAction :: RoundMem -> Tree -> Maybe (Action, PureAction)
actionTreeToAction _ None = Nothing
actionTreeToAction rMem tree = do
  -- the node id contains the string representation of the action, convert this to the pure action
  convertedPureAction <- charToPureAction $ head $ nid $ nodeInfo tree
  -- convert the pure aciton to an action
  convertedAction <- pureActionToAction convertedPureAction $ roundBid rMem
  pure (convertedAction, convertedPureAction)


-- update the monte carlo information of all the nodes in the same level as the given node using the function passed in
-- assume given a node in that level itself
modifyAllInLevel :: Zipper -> (MCInfo -> MCInfo) -> Maybe Zipper
modifyAllInLevel z@(t, _) f
  -- recurse to the right most child in that level
  | rightChild /= None = flip modifyAllInLevelAux f =<< getRightMostInLevel z
  -- once we reach the right most node, modify all the nodes going back up the level
  | rightChild == None = modifyAllInLevelAux z f
  where
    rightChild = getRightChild t
modifyAllInLevel _ _ = Nothing

-- given the right most node in the level, update the monte carlo information of all the nodes in the same level
-- as the given node using the function passed in
modifyAllInLevelAux :: Zipper -> (MCInfo -> MCInfo) -> Maybe Zipper
modifyAllInLevelAux z@(_, bs) f
  | isLeftCrumb $ head bs = modifyNode f z --reached top of level
  | otherwise = do
    modified <- modifyNode f z -- modify the node
    parent <- moveUp modified -- move up/backtrack, attaches the updated node
    modifyAllInLevelAux parent f

-- gets the right most child in the same level as the tree in the given zipper
getRightMostInLevel :: Zipper -> Maybe Zipper
getRightMostInLevel z@(None, _) = moveUp z
getRightMostInLevel z = getRightMostInLevel =<< moveRight z

-- checks if the current breadcrumb is a left crumb
isLeftCrumb :: Crumb -> Bool
isLeftCrumb (LeftCrumb _ _) = True
isLeftCrumb (RightCrumb _ _) = False

-- extract the node information of the parent of the current node from the crumb
extractCrumbId :: Crumb -> NodeID
extractCrumbId (LeftCrumb info _) = nid info
extractCrumbId (RightCrumb info _) = nid info

-- all possible dealer values
dealerVals :: [Int]
dealerVals = [2 .. 11]

-- all possible card numbers (5 is charlie)
cardNums :: [Int]
cardNums = [2 .. 4]

-- all possible configuration of hands, all values + all possible soft totals + all possible pairs
cardVals :: [HandInfo]
cardVals = HandInfo True True 12 : (considerHands =<< [4 .. 21])

-- consider pairs, aces, and no aces as different hands
considerHands :: Int -> [HandInfo]
considerHands n = pair : ace : noAce
  where
    noAce = [basicHand n]
    pair = pairHand n
    ace = aceHand n

-- all possible actions
possibleActions :: [PureAction]
possibleActions = [PHit, PStand, PDoubleDown, PSplit, PInsurance]

-- creates a level of nodes from a given list of strings, these strings serve as node ids
createLevel :: [NodeID] -> Tree
createLevel [] = None
-- if only one node id left, this is the right most node in that level
createLevel [x] = Node noChildren (NodeInfo x defaultMCInfo)
-- if more than one node id, it must have a right child
createLevel (x : xs) = Node (Children None (createLevel xs)) (NodeInfo x defaultMCInfo)

-- creates a new level for the given node and attaches it to the given tree's left child
createLevelBelow :: [NodeID] -> Tree -> Tree
createLevelBelow _ None = None
createLevelBelow nids t@(Node c _) = t {children = c {left = createLevel nids}}

-- defines mapping function for that tree
fmapB :: (Tree -> Tree) -> Tree -> Tree
fmapB _ None = None
fmapB f t@(Node (Children None None) _) = f t -- if a leaf node only apply the function to that tree
-- if the node has a left node, map the function to the left tree
fmapB f t@(Node c@(Children l None) _) = t {children = c {left = fmapB f l}}
-- apply function to bottom level (no more left child)
-- attach the left child of the tree after the function has been applied to the resulting tree
-- map the functin to the rightchild
fmapB f t@(Node (Children None r) _) = resTree {children = resChildren {left = resL, right = fmapB f r}}
  where
    resTree = f t -- apply the function to that tree
    resChildren = children resTree
    resL = left resChildren
-- if the node has a left and right node, map the function to the left and right tree
fmapB f t@(Node c@(Children l r) _) = t {children = c {left = fmapB f l, right = fmapB f r}}

-- attach a level of nodes from a given list of Node ids as the left child of the given tree
-- in point-free form using eta-conversion and function composition to improve readability
-- and compactness
attachLevel :: [NodeID] -> Tree -> Tree
attachLevel = fmapB . createLevelBelow

-- generate an empty monte-carlo tree
-- contains all possible game states,
-- but the number of wins + number of simulations are all initialized to 0
mct :: Tree
mct =
  let dealerLevel = createLevel $ show <$> dealerVals -- create the level for all possible dealer values
      cardNumLevel = attachLevel (show <$> cardNums) dealerLevel -- create the level for all possible card numbers
      handLevel = attachLevel (show <$> cardVals) cardNumLevel -- create the level for all possible hand totals
      resTree = attachLevel (show <$> possibleActions) handLevel -- create the level for all possible actions
   in resTree

-- gets the right child of the given tree
getRightChild :: Tree -> Tree
getRightChild None = None
getRightChild (Node (Children _ r) _) = r

-- gets n best actions based on the Upper Confidence Bound formula for a given level in the tree
-- this is always the action level (bottom-most level)
getNActionOnLargestUCB :: Int -> Maybe Zipper -> [NodeID] -> [Zipper] -> Maybe [Zipper]
getNActionOnLargestUCB _ Nothing _ _ = Nothing
getNActionOnLargestUCB 0 _ _ soFar = Just soFar -- already obtained the n actions no need to find any more
getNActionOnLargestUCB _ _ [] soFar = Just soFar -- no more valid actions, return everything we have
getNActionOnLargestUCB n mZip@(Just zipper) validActs soFar = do
  -- general case
  bestZ@(bestT, _) <- getActionOnLargestUCB zipper validActs -- gets the best action for that level
  -- remove that action from the list of valid actions so we don't obtain the same action the next time around
  let removedBestAction = removeElem (nid $ nodeInfo bestT) validActs
  -- recurse until we have obtained all the actions or there are no more valid actions, add in the found best node to the
  -- list of best actions found so far
  getNActionOnLargestUCB (n -1) mZip removedBestAction (soFar ++ [bestZ])


-- gets the action with the largest UCB in that level, sets up for getActionOnLargestUCBAux
getActionOnLargestUCB :: Zipper -> [NodeID] -> Maybe Zipper
getActionOnLargestUCB z@(tree, _) validActs
  -- if the action represented by that node is a legal action, initialize current tree as the largest and move right,
  -- pass this tree to getActionOnLargestUCBAux
  | nid (nodeInfo tree) `elem` validActs = getActionOnLargestUCBAux (moveRight z) validActs (Just z)
  -- if the action represented by the given node is illegal, initialize largest tree as Nothing and move right,
  -- pass this tree to getActionOnLargestUCBAux
  | otherwise = getActionOnLargestUCBAux (moveRight z) validActs Nothing


-- auxiliary function for getActionOnLargestUCB
-- params: current tree, valid nodes, best zipper
getActionOnLargestUCBAux :: Maybe Zipper -> [NodeID] -> Maybe Zipper -> Maybe Zipper
-- if traversed the whole branch and no more right child and still no best tree, return nothing
getActionOnLargestUCBAux (Just (None, _)) _ Nothing = Nothing
-- no more right child, we have traversed the whole branch, return the best tree found so far
getActionOnLargestUCBAux (Just (None, _)) _ bestZ = bestZ
getActionOnLargestUCBAux (Just curZ@(Node _ nInfo, _)) validActs maybeZ
  -- if the action represented by that node is a legal action, compare it with the best tree so far
  -- if it is better replace the best tree with this one
  | nid nInfo `elem` validActs = getActionOnLargestUCBAux (moveRight curZ) validActs (Just updatedBest)
  -- otherwise pass this action and move along to the next action (moving down its right child)
  | otherwise = getActionOnLargestUCBAux (moveRight curZ) validActs maybeZ
  where
    updatedBest = if isNothing maybeZ then curZ else chooseMaxUCBTree (fromJust maybeZ) curZ
getActionOnLargestUCBAux _ _ _ = Nothing

-- get the node with the largest win rate for that level
getLargestWRAction :: Maybe Zipper -> [NodeID] -> Maybe Zipper -> Maybe Zipper
getLargestWRAction Nothing _ _ = Nothing
-- if we reached the bottom level, just return the zipper containing the best node found so far
getLargestWRAction (Just (None, _)) _ best = best
-- if there is no best tree found yet, if the current tree represents a legal action it is automatically the best tree
-- otherwise move on to its right child
getLargestWRAction maybeZ@(Just z@(tree, _)) validActs Nothing
  | nid (nodeInfo tree) `elem` validActs = getLargestWRAction (moveRight z) validActs maybeZ
  | otherwise = getLargestWRAction (moveRight z) validActs Nothing
-- if we have found a best tree, compare the current tree with this best tree (if it's legal) and update the best if the current
--tree has a higher win rate
-- otherwise move on keeping the current best
getLargestWRAction curZ@(Just cur@(curTree, _)) valid bestZ@(Just (bTree, _))
  | nid (nodeInfo curTree) `elem` valid = getLargestWRAction (moveRight cur) valid newBest
  | otherwise = getLargestWRAction (moveRight cur) valid bestZ
  where
    winRate t = calcWinRate $ mcInfo $ nodeInfo t
    newBest = if winRate curTree > winRate bTree then curZ else bestZ

-- calculate the win rate of the tree
calcWinRate :: MCInfo -> Float
calcWinRate MCInfo {wins, numSimsThisNode}
  | numSimsThisNode == 0 = 0
  | otherwise = wins / fromIntegral numSimsThisNode

type Statistic = Float

-- get the larger tree within two zippers based on their UCB score
chooseMaxUCBTree :: Zipper -> Zipper -> Zipper
chooseMaxUCBTree z1@(t1, _) z2@(t2, _)
  --if any one of them is nothing, the other one is automatically the larger UCB tree
  | isNothing ucbT1 = z1
  | isNothing ucbT2 = z2
  | ucbT2 > ucbT1 = z2
  | otherwise = z1
  where
    [ucbT1, ucbT2] = calcUCB <$> [t1, t2]

-- calculate the UCB statistic given a tree
-- formula obtained from on: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
-- balances exploitation (favours the best already simulated action) and exploration (favours actions that have not yet been simulated)
calcUCB :: Tree -> Maybe Statistic
calcUCB tree = do
  let (MCInfo wins sims simsThisLevel) = mcInfo $ nodeInfo tree
      exploitation = wins / fromIntegral sims
  logRes <- safeLogInts simsThisLevel
  divRes <- safeDiv logRes $ fromIntegral sims
  sqrtRes <- safeSqrt divRes
  let exploration = explorationParam * sqrtRes
  -- incentivize exploring new actions that have not been simulated by returning a 99 for them
  if sims == 0 then pure 99 else pure $ exploitation + exploration

-- perform a safe square root, which can fail if the given number is negative
safeSqrt :: Float -> Maybe Statistic
safeSqrt f
  | f < 0 = Nothing
  | otherwise = Just $ sqrt f

-- perform a safe division, which can fail if the denominator is 0
safeDiv :: Float -> Float -> Maybe Statistic
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a / b

--divide ints to produce a float, which can fail if the denominator is 0
safeDivInts :: Int -> Int -> Maybe Statistic
safeDivInts _ 0 = Nothing
safeDivInts a b = Just $ fromIntegral a / fromIntegral b

-- obtain the log of an integer, will fail if the integer is less than 1
safeLogInts :: Int -> Maybe Statistic
safeLogInts n
  | n < 1 = Nothing
  | otherwise = Just $ log $ fromIntegral n

-- hand with no aces no pairs
basicHand :: Int -> HandInfo
basicHand = HandInfo False False

-- hand with a pair
pairHand :: Int -> HandInfo
pairHand = HandInfo False True

-- hand with an ace
aceHand :: Int -> HandInfo
aceHand = HandInfo True False

-- exploration parameter c for MCTS
explorationParam :: Float
explorationParam = sqrt 2

-- directly converts list of round memories (RoundMems) to gamestate (to prepare for storing it into the MCT)
convertDirectlyMemToGstate :: Memory -> [GameState]
convertDirectlyMemToGstate = map convertDirectlyToGstate

-- convert a single round's memory to GameState
convertDirectlyToGstate :: RoundMem -> GameState
convertDirectlyToGstate rMem@RoundM {roundDealer, roundActions, roundHand, roundOutcome} =
  GState roundDealer (length roundHand) handInf (Just $ last roundActions) roundOutcome
  where
    -- computes information regarding the player hand which is relevant to the MCT
    handInf = HandInfo (isSoft roundHand) (isPair rMem) (sum $ fromMaybe roundHand (changeAce $ Just roundHand))


-- check whether we have found the node with the given id
foundNode :: Tree -> NodeID -> Bool
foundNode None _ = False
foundNode t s = nid (nodeInfo t) == s

---------------------------------------------------------------------
-- this section contains show instances for the tree and information related to the tree
-- for the purposes of debugging and ensuring correctness of the tree

instance Show MCInfo where
  show (MCInfo w numThisNode numSubtree) =
    "Monte Carlo related info: ["
      ++ " Wins: "
      ++ show w
      ++ " Number of simulations for this node: "
      ++ show numThisNode
      ++ " Total number of simulations for this subtree: "
      ++ show numSubtree
      ++ "]"

instance Show NodeInfo where
  show (NodeInfo v mc) =
    "Node Info: {\n"
      ++ "\tValue: "
      ++ show v
      ++ "\n"
      ++ "\t"
      ++ show mc
      ++ "\n"
      ++ "\t}\n"

instance Show Children where
  show (Children l r) = "LEFT: " ++ show l ++ "RIGHT: " ++ show r

instance Show Tree where
  show None = "None\n"
  show (Node c n) = "(\n" ++ show n ++ show c ++ ")\n"

instance Show HandInfo where
  show (HandInfo a p v) = show v ++ aceStr ++ pairStr
    where
      pairStr = if p then "P" else ""
      aceStr = if a then "A" else ""

instance Show GameState where
  show (GState d cNum hInfo act gameOutcome) =
    "Dealer: " ++ show d
      ++ " Card num: "
      ++ show cNum
      ++ " Hand Info: "
      ++ show hInfo
      ++ safeShow act " Action: "
      ++ "Outcome: "
      ++ show gameOutcome
    where
      safeShow :: Show a => Maybe a -> String -> String
      safeShow (Just a) str = str ++ show a
      safeShow Nothing _ = ""

----------------------------------------------------
-------------------- SIMULATION --------------------
----------------------------------------------------

-- plays a single round till completion, since the hand is not carried over to the next round it's not
-- necessary to play out a whole game

-- the constant bid amount for the simulation game
simBidAmt :: Points
simBidAmt = 100

threeDecks :: Stock
threeDecks = join $ replicate 3 sortedDeck

-- take in a seed and generate a random deck (random length + random cards)
randomizeDeck :: RandState -> Int -> Stock
randomizeDeck seed maxLen = map snd $ take len sortedRandDeck
  where
    next = nextRand seed
    len = maxMod 10 next maxLen -- generates a deck with the length between 10 and maxLen
    -- generate random indices to pair with each card (referenced src/Deck.hs)
    indices = foldr (const accumulateRandAndLst) (nextRand next, []) [1 .. length threeDecks]
    -- accumulates the random states
    accumulateRandAndLst :: (RandState, [RandState]) -> (RandState, [RandState])
    accumulateRandAndLst (prevSeed, previs) = (nextRand prevSeed, prevSeed : previs)
    -- randomize the deck by sorting based on the paired random number
    sortedRandDeck = sortOn fst $ zip (snd indices) threeDecks

-- constant, simulation always starts with 1000 pts
simStartPts :: Int
simStartPts = 1000

-- perform a simulation with the current round memory and the given action we want to simulate
-- takes in a random seed to randomize the deck
simulateNTimes :: Int -> RoundMem -> PureAction -> RandState -> Maybe SimInfo
simulateNTimes n rMem simAct seed = do
  -- perform the simulation n times and reduces them (concatenates the simulation information to a single list)
  -- converts it from a list of Maybe SimInfos to a Maybe list of SimInfos
  simInfos <- sequence $ snd $ foldr (const accumulateSimInfos) (seed, []) [1 .. n]
  accumulateInfo rMem simAct simInfos -- accumulate the information for all n simulations
  where
    -- accumulate simulation informations and use the current random state to generate the next random state
    accumulateSimInfos :: (RandState, [Maybe SimInfo]) -> (RandState, [Maybe SimInfo])
    accumulateSimInfos (prevSeed, simInfos) = (nextRand prevSeed, simulation prevSeed : simInfos)
    simulation = simulateOnce rMem simAct -- partially applied, only requires seed

-- accumulates all N simulation information based on the current rounds information
accumulateInfo :: RoundMem -> PureAction -> [SimInfo] -> Maybe SimInfo
accumulateInfo rMem pAction simInfos = do
  let prevActions = roundActions rMem
      -- combines OutcomeInfos using the defined mappend for the Semigroup instance of OutcomeInfo
      accOutcome = accumulateOutcomeInfo $ roundOutcome <$> simInfos
  pure
    rMem
      { roundOutcome = accOutcome,
        roundActions = prevActions ++ [pAction] --add the simulated action to the list of actions performed this round
      }

-- perform a single simulation
simulateOnce :: RoundMem -> PureAction -> RandState -> Maybe SimInfo
simulateOnce rMem actionToSimulate seed = do
  simTrick <- generateSimTrick rMem actionToSimulate -- generate the trick to pass into the simulation
  simDeck <- recreateDeck rMem seed -- generate the random deck for that simulation
  getSimInfo $ runSimulation simDeck simTrick -- obtain the resulting information after running the simulation

-- generate the trick to pass into the simulation
generateSimTrick :: RoundMem -> PureAction -> Maybe Trick
generateSimTrick RoundM {roundActions} act =
  Just simTrick
  where
    simActions = fold $ show <$> roundActions ++ [act] -- to recreate the current game state, perform all the actions performed in the round so far
    simMem = "?" ++ show simBidAmt ++ "B" ++ simActions -- initialize the simulation memory with the sim bid amount and the actions to perform
    simTrick = [PlayNode (Play "99" 0 500 (Bid 100) simMem []) Nil] -- generate the Trick

-- recreate the deck for the current round, i.e. ensures that the dealer gets the same card as they did for the current non-simulation round
-- ensures the player in the simulation gets the same starting cards as the player in the current non-simulated round
recreateDeck :: RoundMem -> RandState -> Maybe Stock
recreateDeck RoundM {roundDealer, roundRanks} seed = do
  dealerUpCard <- generateCardFromVal roundDealer -- we know the dealer's up card, so put it on top of the deck
  dealerDownCard <- generateCardFromVal $ maxMod 1 seed 12 -- randomize the dealer's down card
  let playerCards = Card Spade <$> roundRanks -- suit doesn't matter
      randomizedRestDeck = randomizeDeck seed (length playerCards + 2)
  pure $ dealerUpCard : dealerDownCard : playerCards ++ randomizedRestDeck -- generates the deck to recreate the current non-simulation round game state

-- convert from the point value of the card to its rank
valToRank :: [(Int, Rank)]
valToRank = [(1, Ace), (11, Ace)] ++ zip [2 .. 10] [Two .. Ten]

-- generate a card with the given rank of the card
generateCardFromVal :: Int -> Maybe Card
generateCardFromVal rank = generateCardFromRank <$> generateRankFromVal rank

-- for the purposes of simulation, the suit is unimportant so convert all to spades
generateCardFromRank :: Rank -> Card
generateCardFromRank = Card Spade

-- based on the mapping list, generate the rank of the card given its value
generateRankFromVal :: Int -> Maybe Rank
generateRankFromVal num = snd <$> find ((== num) . fst) valToRank

type SimInfo = RoundMem

-- extract the memory returned from the simulation from the trick
getSimMemory :: Trick -> Maybe Memory
getSimMemory (_ : players) = extractSimMemory $ last players
getSimMemory _ = Nothing

-- extracts the simulation memory from the playnode
extractSimMemory :: PlayNode -> Maybe Memory
extractSimMemory node = do
  memStr <- memory <$> nodeValue node -- gets the string memory returned
  -- removes the simulation part of the memory that can only be parsed in simulation rounds
  ind <- elemIndex '/' memStr
  let useableMem = drop ind memStr
  -- parse the simulation memory and extract the parsed result
  safeExtractParseResult $ parse strictParseAll useableMem

-- takes in a ParseResult and safely extract a value from it,
-- if it fails return a Nothing
safeExtractParseResult :: ParseResult a -> Maybe a
safeExtractParseResult (Result _ a) = Just a
safeExtractParseResult (Error _) = Nothing

-- extract the points obtained by the simulation player
extractSimPts :: HandResult -> Points
extractSimPts HandResult {handPoints} = getPoints $ head handPoints

-- obtain the relevant simulation information (points gained/lost)
getSimInfo :: Either GameError (HandResult, Trick, Card, Stock) -> Maybe SimInfo
getSimInfo (Left _) = Nothing -- simulation failed
getSimInfo (Right (res, trick, _, _)) = do
  simMemory <- head <$> getSimMemory trick -- gets the simulation memory
  let simOutcomeInf = weightedOutcome simStartPts (extractSimPts res) simBidAmt -- takes the weighted outcome
      updatedSimMemory = simMemory {roundOutcome = simOutcomeInf} -- update the roundOutcome with the result of the simulation
  pure updatedSimMemory

-- computes the weighted outcome based on how many points were gained/lost
weightedOutcome :: Points -> Points -> Int -> OutcomeInfo
weightedOutcome startPts endPts bid = evaluateOutcome bid $ endPts - startPts

-- reward function, based on start and end points determines how much to reward the action
evaluateOutcome :: Int -> Int -> OutcomeInfo
evaluateOutcome bidAmt res
  | res < - bidAmt = defaultOutcomeInf {numLosses = 2}
  | res == - bidAmt = defaultOutcomeInf {numLosses = 1}
  | res == 0 = defaultOutcomeInf {numUndecided = 1}
  | res == bidAmt = defaultOutcomeInf {numWins = 1}
  | res > bidAmt = defaultOutcomeInf {numWins = 2} --  reward more risk
  | otherwise = defaultOutcomeInf -- to catch any errors
  where
    defaultOutcomeInf = OutcomeInfo 0 0 0

-- default simulation player id to differentiate from non-simulation players
defaultSimPlayerId :: PlayerId
defaultSimPlayerId = "99"

-- run the simulation given the deck to use and the memory to pass in
runSimulation :: Stock -> Trick -> Either GameError (HandResult, Trick, Card, Stock)
runSimulation deck trick = playHandSim deck trick simGamePoints
  where
    simPlayer = Player defaultSimPlayerId playCard -- create the simulation player
    simGamePoints = [GamePoints simPlayer $ Rich simStartPts] -- start the player off with 1000 points


-------------- FOR SIMULATION ----------------
-- NOTE: IN THIS SECTION OF THE CODE I MODIFIED THE SOURCE CODE PROVIDED IN src/TwentyOne/Play.hs
--       TO REMOVE THE IO CONTEXT FOR THE PURPOSES OF SIMULATION I WILL NOT BE DOCUMENTING THESE
--       AS IT IS NOT MY CODE (made only minor changes)

dealContinuous :: Int -> Int -> Int -> [Card] -> ([Hand], [Card])
dealContinuous _ n m deck =
  -- Take from deck
  let total = m * n
      numFromDeck = min total (length deck)
      dealtCards = take numFromDeck deck
      deck' = drop numFromDeck deck
      remaining = total - numFromDeck

      -- Take more from deck after shuffling
      newDeck = genDeck deck'
      allDealtCards = dealtCards ++ take remaining newDeck
      newDeck' = drop remaining newDeck
      finalDeal = deal n m allDealtCards

      genDeck :: [Card] -> [Card]
      genDeck [] = randomizeDeck 0 $ length threeDecks -- not random
      genDeck d = d
   in (finalDeal, newDeck')

takeContinuousSim :: Int -> Int -> [Card] -> ([Card], [Card])
takeContinuousSim decksUsed n cards =
  let (c, d) = dealContinuous decksUsed n 1 cards
   in (concat c, d)

playCardsSim ::
  Maybe Card -> -- Dealer's current up-card
  [GamePoints] -> -- Scores from previous round
  Stock -> -- Stock pile
  Trick -> -- Previous trick
  Trick -> -- Current trick
  PlayerHand -> -- Player hand
  Int -> -- Second ID used to identify separate Play sequences
  Either GameError (Trick, Stock, [GamePoints]) -- new trick, new stock pile, new points
playCardsSim upCard scores stock prev current hand sid = do
  let (PlayerHand Player {_playerId = pid} handCards) = hand

      -- Player's previous action in the current round
      playerNode =
        fromMaybe Nil $ find (hasIds pid sid . nodeValue') current

  -- Player's action
  (choice, updated) <-
    playActionSim
      upCard
      hand
      scores
      prev
      current
      playerNode
      sid

  -- Parse changes from action
  let (newCards, stocked, updatedBid, newBid) =
        parseActionSim choice handCards stock (getPoints playerNode)

  -- Combine with results
  let newScores = map' ((== pid) . getId) (gpmap (`minus` newBid)) scores

  -- Update trick with action
  let newPlay s = Play pid s updatedBid choice updated newCards
      newCurrent = update (PlayNode (newPlay sid) playerNode) current

  -- What to do next
  let nextPlayCard s' st' ph' i' t' =
        playCardsSim upCard s' st' prev t' (PlayerHand (owner hand) ph') i'

      continuePlaying = nextPlayCard newScores stocked newCards sid

  -- Auxiliary function for pattern matching to determine recursion
  let playCardsSim' ::
        Action ->
        HandValue ->
        Either GameError (Trick, Stock, [GamePoints])
      playCardsSim' Hit (Value _) = continuePlaying newCurrent
      playCardsSim' (DoubleDown _) (Value _) = continuePlaying newCurrent
      playCardsSim' (Insurance _) _ = do
        let insNewplay = newPlay (sid + 1)
            insTree = PlayNode insNewplay playerNode : newCurrent
        continuePlaying insTree
      playCardsSim' a@(Split _) _ = do
        let (cards', stocked') = takeContinuousSim numDecks 2 stocked

        -- Mix cards in hand with new cards
        let [hand1, hand2] = transpose [handCards, cards']

        -- Original line
        (t', s', p') <-
          nextPlayCard
            newScores
            stocked'
            hand1
            sid
            newCurrent

        -- Find memory from original line
        let mem' = fromJust $ getMemory pid sid t'

        -- Split line
        let splitNewPlay = Play pid (sid + 1) updatedBid a mem' handCards
            splitTree = PlayNode splitNewPlay playerNode : t'

        nextPlayCard p' s' hand2 (sid + 1) splitTree
      playCardsSim' _ _ = return (newCurrent, stocked, newScores)

  playCardsSim' choice (handValue newCards)

-- | Distribute a (shuffled) deck to the players and start the game.
--   Assume we always have sufficient amount of cards.
playHandSim ::
  [Card] ->
  Trick ->
  [GamePoints] ->
  Either GameError (HandResult, Trick, Card, Stock)
playHandSim deck prev scores = do
  -- Deal cards, with shuffling
  let (dealt, stock) = dealContinuous numDecks startingNumCards (length scores + 1) deck
  let dealerCards : playerCards = dealt

  -- Separate rich and bankrupt players
  let (rich, bankrupt) = partition (isRich . finalPoints) scores
      order = player <$> rich

  -- Pre play - bids
  (bidTrick, stock', bidGP) <- playBidsSim prev stock rich order

  -- Main play
  (tricked, newGP, stock'') <-
    playTricksSim
      (zipWith PlayerHand order playerCards)
      stock'
      dealerCards
      prev
      bidTrick
      bidGP

  let (handPoints, dealerTrick, stock''') = evaluatePlaysSim tricked stock'' dealerCards

  -- Combine players
  let resultPoints = combinePoints handPoints newGP
      pids = sort $ getId <$> order
      zeroPlayers = HandPoints 0 . getId <$> bankrupt
      nonZeroPlayers = zipWith HandPoints resultPoints pids
      players = nonZeroPlayers ++ zeroPlayers
      results = HandResult tricked players

  return (results, dealerTrick : tricked, head dealerCards, stock''')

-- | Play out a round one hand at a time until everyone either busts, stands, or has TwentyOne.
playTricksSim ::
  [PlayerHand] -> -- List of player hands, each starting with 2 cards
  Stock -> -- Stock pile
  Hand -> -- Dealer hand
  Trick -> -- Previous trick
  Trick -> -- Current trick
  [GamePoints] -> -- Current players' points
  Either GameError (Trick, [GamePoints], Stock) -- New Trick, New Scores, Updated Stock Pile
playTricksSim [] stock _ _ tricks points = return (tricks, points, stock)
playTricksSim (hand : otherHands) stock dealersHands prev current scores
  | -- Stop execution after 10000 tricks
    length current > 10000 =
    error "10000 tricks reached"
  | otherwise = do
    -- Put previous memory when playing
    (played, stocked, scored) <-
      playCardsSim
        (listToMaybe dealersHands)
        scores
        stock
        prev
        current
        hand
        0

    -- Play trick for rest of the players
    playTricksSim otherHands stocked dealersHands prev played scored

playBidsSim ::
  Trick ->
  Stock ->
  [GamePoints] ->
  [Player] ->
  Either GameError (Trick, Stock, [GamePoints])
playBidsSim prev stock rich = foldl f' (pure ([], stock, rich))
  where
    f' r p = do
      (t, s, gp) <- r
      playCardsSim Nothing gp s prev t (PlayerHand p []) 0

parseActionSim ::
  Action -> Hand -> [Card] -> Points -> (Hand, [Card], Points, Points)
parseActionSim action hand stock bid =
  let (h, t) = takeContinuousSim numDecks 1 stock
   in case action of
        Hit -> (head h : hand, t, bid, 0)
        (Split _) -> (hand, stock, bid, bid)
        (Insurance _) -> (hand, stock, bid, maxInsure bid)
        Bid b -> (hand, stock, bid + b, b)
        (DoubleDown _) -> (hand, stock, bid * 2, bid)
        Stand -> (hand, stock, bid, 0)

playActionSim ::
  Maybe Card ->
  PlayerHand ->
  [GamePoints] ->
  Trick ->
  Trick ->
  PlayNode ->
  Int ->
  Either GameError (Action, String)
playActionSim upCard hand scores prev current node sid = do
  let (PlayerHand Player {_playerId = pid, playFunc} handCards) = hand

      -- Process information for player
      pp = gamePointsToPlayerPoints <$> scores
      infos = combineWith (playToInfo . nodeValue') current prev
      userMemory = (firstJust `on` getMemory pid sid) current prev

      -- Execute user function
      (choice', raw) = playFunc upCard pp infos pid userMemory handCards

  -- Check action is valid
  updated <- checkMemory raw pid
  -- state   <- liftEither $ checkMemory updated
  choice <- validPlay choice' hand pp upCard node
  return (choice, updated)

evaluatePlaysSim :: Trick -> Stock -> Hand -> ([HandPoints], PlayNode, Stock)
evaluatePlaysSim tricked stock dealerCards =
  -- Get only the player hands
  let playerPlays =
        filter (not . isInsurance . act) (nodeValue' <$> tricked)

      -- Evaluate value of each hand
      phv = handValue . finalHand <$> playerPlays

      -- Play dealer
      noPlayResult = noPlayDealer stock dealerCards

      (dealerValues, stock', dealerTrick) =
        if all isBust phv
          then noPlayResult
          else playDealerHandSim stock dealerCards

      finalPoints = calculatePoints (nodeValue' <$> tricked) dealerValues
   in (finalPoints, dealerTrick, stock')

-- | Dealer's turn
playDealerHandSim :: Stock -> Hand -> (HandValue, Stock, PlayNode)
playDealerHandSim stock cards =
  let (dealerTrick, stocked) = dealerPlayFuncSim' stock cards Nil
      dealerHand = finalHand $ nodeValue' dealerTrick
      dealerValue = handValue dealerHand
   in (dealerValue, stocked, dealerTrick)

-- | Auxiliary function used to play for the dealer
dealerPlayFuncSim' :: Stock -> Hand -> PlayNode -> (PlayNode, Stock)
dealerPlayFuncSim' stock c tricks = do
  let (newCard, stocked) = takeContinuousSim numDecks 1 stock
  let s = head newCard
  case dealerPlayFunc c of
    Hit ->
      dealerPlayFuncSim'
        stocked
        (s : c)
        (PlayNode (dealerPlay Hit (s : c)) tricks)
    Stand -> (PlayNode (dealerPlay Stand c) tricks, stock)
    _ -> error "Dealer should not use other Actions"


-----------------------------------------
------------Utility Functions------------
-----------------------------------------


-- helper function which safely obtains the head of a list, fails if the list is empty -> returns Nothing
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x


-- replaces the last element in a list
replaceLast :: a -> [a] -> [a]
replaceLast replacement l = reverse $ replacement : tail (reverse l)


-- given an item and a list of items of the same type, split it at the first instance of the given item.
-- This is a partial function so wrap in a Maybe context, if the item does not exist return Nothing,
splitAtFirst :: Eq a => a -> [a] -> Maybe ([a], [a])
splitAtFirst x l = case elemIndex x l of -- get index of the element in the list
  (Just i) -> Just $ splitAt i l -- split at this index
  Nothing -> Nothing


-- partial function, makes list indexing safe by wrapping in a Maybe context
getIthElem :: Int -> [a] -> Maybe a
getIthElem _ [] = Nothing
getIthElem 0 (cur : _) = Just cur
getIthElem n (_ : rest) = getIthElem (n -1) rest


-- accumulates a list of numbers from left to right using foldl1
-- the 1 indicates that we don't have an initial value for the accumulator,
-- instead use the first value from the list
appendNum :: Num a => [a] -> a
appendNum = foldl1 (\acc cur -> acc * 10 + cur)


-- replace the last occurrence of an item
-- can fail if the item does not exist in the list
condReplaceLast :: Eq a => [a] -> a -> a -> Maybe [a]
condReplaceLast l item replacement = reverse <$> changeFirst (reverse l) item replacement

-- changes the first instance of the given item in the list,
-- fails if the item does not exist in the list
changeFirst :: Eq a => [a] -> a -> a -> Maybe [a]
changeFirst [] _ _ = Nothing
changeFirst (x : xs) i r
  | x == i = Just $ r : xs
  | otherwise = (x :) <$> changeFirst xs i r


-- count occurrences of a value in a list
count :: Eq a => a -> [a] -> Int
count val = foldr (\cur acc -> if cur == val then acc + 1 else acc) 0


-- to multiply a float with an int
mult :: Float -> Int -> Float
mult a b = a * fromIntegral b


-- removes an element from a list
removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem remElem (cur : rest) = if remElem == cur then removeElem remElem rest else cur : removeElem remElem rest


type LowerBound = Int

type UpperBound = Int

-- mod with a minimum value
maxMod :: LowerBound -> Int -> UpperBound -> Int
maxMod minVal val maxVal = max minVal $ mod val maxVal

type RandState = Int

-- generates a random number based on the previous random number in pure fashion
nextRand :: RandState -> RandState
nextRand prevSeed = (a * prevSeed + c) `mod` m
  where
    -- Parameters for linear congruential RNG.
    a = 1103515245
    c = 12345
    m = intPower 2 31

-- helper for the nextRand function so it doesnt have to infer type
intPower :: Int -> Int -> Int
intPower base pow = base ^ pow


-- -- | Generic function for calling player functions with a timer
-- logs the timing information in a file called times100.txt
-- timeCall :: NFData b => (Hand -> b) -> PlayerId -> Hand -> EitherIO GameError b
-- timeCall func pid handCards = EitherIO $ do
--   -- Careful, these are microsecs
--   start <- getCurrentTime
--   played <- timeout 1000000 $ return $!! func handCards -- Will force evaluation
--   end <- getCurrentTime
--   appendFile "./times100.txt" (show $ diffUTCTime end start)
--   let timed = case played of
--         Nothing -> Left $ GameError TimeError pid
--         Just c -> Right c
--   return timed