

-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import Utils
import Control.Monad

-- You can add more imports if you need them
import Data.Char
import Parser.Instances
import Data.Functor
import Control.Applicative
import Debug.Trace
import Data.Maybe (isNothing, fromMaybe)
import Data.List
import TwentyOne.Play
import Deck (shuffledDecks, sortedDeck)
import EitherIO (EitherIO(runEitherIO), liftEither, liftIO)
import GHC.Float (divideFloat, float2Int, int2Float)
import Data.Either
import Data.Foldable
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO (unsafePerformIO)


-- | This function is called once it's your turn, and keeps getting called until your turn ends.

-- for convenience
type CompleteAction = (Action, String)

-- default action (if error occurs)
defaultCompleteAction :: CompleteAction
defaultCompleteAction = (defaultAction, "")

defaultAction :: Action
defaultAction = Stand

defaultBid :: Int
defaultBid = minBid

playCard :: PlayFunc
playCard Nothing pts _ pid mem h =
    trace("Bidding round" ++ "\n" ++ "Memory: " ++ fromMaybe "No Memory" mem)
    evalBid mem pts pid h
playCard (Just dealer) pts info pid (Just mem) h =
    trace(
        "-----Player----- " ++ pid ++"\n"++
        "Points: "++ show pts ++ "\n" ++
        "Hand: " ++ show h ++
        "Player Info: " ++ show info ++
        "Memory: " ++ mem
    )
    -- playerAction mem dealer h
    updateMemAndPlay mem dealer h
    -- (Stand, mem ++
    --     show (rankToVal $ getRank (fromMaybe (Card Spade Ace) dealer)) ++
    --     conc (show .rankToVal . getRank <$> h) ++
    --     "S")
    -- where
    --     conc  = concat
playCard _ _ _ _ _ _ = (Stand, "")
-- playCard dealer pts info pid mem h = playerAction mem


evalBid :: Maybe String -> [PlayerPoints] -> PlayerId -> Hand -> CompleteAction
-- TODO: GET AVG. STRENGTH OF THE HAND (COUNT CARDS MAYBE)
-- evalBid mem pts pid h = (Bid defaultBid, fromMaybe "" mem)
evalBid Nothing pts pid h = (Bid defaultBid, retMem)
    where
        retMem = storeStartOfRound Nothing pts pid h
evalBid m@(Just mem) pts pid h =
    trace("PLAYER POINTS::: " ++ show pts)
     (Bid bidAmt, mem ++ retMem)
    where
        hist = getHistory mem
        isSim = isSimulation hist
        bidAmt = if isSim then simBid $ simSetup hist else defaultBid
        retMem = storeStartOfRound m pts pid h
-- evalBid mem pts pid h
--     = (Bid defaultBid, if isNothing mem then retMem else fromMaybe "" mem ++ retMem)
--     where
--         retMem = storeStartOfRound mem pts pid h
        -- retMem = "Bid"

-- given the playerMem determine if it is a simulation or not
isSimulation :: History -> Bool
isSimulation = (/=NoSetup) . simSetup

memoryIsEmpty :: History -> Bool
memoryIsEmpty = null . playerMem

getHistory :: String -> History
getHistory = extractFromParseResult mempty . parse parseHistory

-- rState = RoundM [] [11, 1] 0 200 2
-- rState2 = RoundM [PHit] [11, 1, 3] 0 200 4
-- rState3 = RoundM [] [2,2] 0 200 2
-- rState4 = RoundM [] [2,2] 0 200 11
-- rState5 = RoundM [PHit] [2,2] 0 200 11
-- rState6 = RoundM [PDoubleDown] [2,2] 0 200 11
-- rState7 = RoundM [PDoubleDown, PHit] [2,2] 0 200 11
-- rState8 = RoundM [PDoubleDown, PHit, PStand] [2,2] 0 200 11

rState1 = RoundM [] [10, 10] [Jack, Jack] 1000 20 2 Nothing

-- !!!BIG TODO: PLEASE FIND A BETTER WAY TO DO THIS
legalActions :: RoundMem ->Hand -> [PureAction]
legalActions rMem@RoundM {roundActions, roundDealer} hand = finalAction
    where
        defaultActions = [PHit, PStand]
        includeInsurance = [PInsurance | roundDealer == 11 && null roundActions]
        includeSplit = [PSplit | handIsPair hand]
        includeDoubleDown = [PDoubleDown | length hand == 2]
        -- if double down is one of the last 2 actions and if hit is the most recent action then stand otherwise hit
        finalAction = if elem PDoubleDown $ take 2 $ reverse roundActions then if (==PHit) $ head $ take 1 $ reverse roundActions
         then
             if length hand > 2 -- did not bust after hitting (only relevant for splits when if the previous one busts it doesnt get passed to bid)
                 then [PStand]
                 else defaultActions ++ includeInsurance ++ includeSplit ++ includeDoubleDown else [PHit]
            else defaultActions ++ includeInsurance ++ includeSplit ++ includeDoubleDown

-- testEncodeCurState = encodeCurState (History NoSetup [RoundM [PStand] [2, 10] 1000 100 2]) (Card Spade Nine) [Card Diamond Ace, Card Heart Four, Card Spade Jack]

-- add the dealer card and your hand before performing action
encodeCurState :: History -> Card -> Hand -> String
encodeCurState hist@(History _ playerMem) dCard pHand
    | null playerMem || storeAll = dCardEncoding ++ fold (show . getRank <$> pHand)
    | otherwise = newCard
    where
        dCardEncoding = encodeCard dCard
        newCard = encodeNewCard hist pHand
        mostRecent = mostRecentRound playerMem
        -- ranks = roundRanks mostRecent
        storeAll = (==0) (roundBid mostRecent) && null (roundHand mostRecent)


-- notFirstRound :: History -> Bool

-- testUMAP = updateMemAndPlay "?100BH/1000$90B23H4" (Card Spade Nine) [Card Diamond Four, Card Heart Four]

updateMemAndPlay :: String -> Card -> Hand -> CompleteAction
updateMemAndPlay oldMem dCard pHand = completeAction
    where
        oldHist = getHistory oldMem
        updatedMem = oldMem ++ encodeCurState oldHist dCard pHand
        completeAction = playerAction updatedMem dCard pHand

-- for Non-bid actions
playerAction :: String -> Card -> Hand -> CompleteAction
-- playerAction Nothing _ = defaultCompleteAction -- memory should never be nothing in non-bid rounds
playerAction mem dealerCard playerHand
    | trace(
        "Updated Memory:" ++ mem ++
        "History: "++ show hist) False = undefined
    -- in a simulation and is the first turn, perform fixed simulation action
    | isSimulation hist && hasSimulatedAction hist =
        trace("IN SIMULATION AND HAS SIMULATED ACTION" ++ "\n" ++
        "ACTION" ++ show simAction
        )
         (simAction, mem ++ simEncoding)
    -- in a simulation but not the first turn, no need to update tree (will be updated outside of simulation), choose random actions
    | isSimulation hist && not (hasSimulatedAction hist) =
        trace("IN SIMULATION BUT NO MORE SIMULATED ACTIONS" ++
        "LEGAL ACTIONS: " ++ ( if null $ playerMem hist then "" else show $ legalActions (mostRecentRound $ playerMem hist) playerHand) ++ "\n"++
        "RANDOM ACTION" ++ show randomAction
        )
        (randomAction, mem ++ randActionEncoding)
    -- in a simulation but not the first turn, choose random actions
    | otherwise = mcts mem (playerMem hist) dealerCard playerHand-- Store memory data into tree, then Get action from tree
    where
        hist = getHistory mem
        (simAction, simEncoding) = performSimAction hist
        (randomAction, randActionEncoding) = getRandomAction hist playerHand
        -- storeDealer = if missingDealerCard $ mostRecentRound $ playerMem hist then encodeCard dealerCard else ""
        -- updatedMem = mem ++ storeDealer ++ encodeNewCard hist hand
        -- updatedMem = mem ++ storeDealer ++ encodeNewCard hist hand


-- playerAction :: String -> Card -> Hand -> CompleteAction
-- -- playerAction Nothing _ = defaultCompleteAction -- memory should never be nothing in non-bid rounds
-- playerAction mem dealerCard hand
--     | trace(
--         "Updated Memory:" ++ mem ++
--         "History: "++ show hist) False = undefined
--     -- in a simulation and is the first turn, perform fixed simulation action
--     | isSimulation hist && firstActionThisRound hist =
--         trace("IN SIMULATION AND FIRST TURN" ++ "\n" ++
--         "ACTION" ++ show simAction
--         )
--          (simAction, mem ++ simEncoding)
--     -- in a simulation but not the first turn, no need to update tree (will be updated outside of simulation), choose random actions
--     | isSimulation hist && not (firstActionThisRound hist) =
--         trace("IN SIMULATION BUT NOT FIRST TURN" ++
--         "LEGAL ACTIONS: " ++ show ( legalActions $ mostRecentRound $ playerMem hist) ++ "\n"++
--         "RANDOM ACTION" ++ show randomAction
--         )
--         (randomAction, mem ++ randActionEncoding)
--     -- in a simulation but not the first turn, choose random actions
--     | otherwise = undefined -- Store memory data into tree, then Get action from tree
--     where
--         hist = getHistory mem
--         (simAction, simEncoding) = performSimAction hist
--         (randomAction, randActionEncoding) = getRandomAction hist hand
--         -- storeDealer = if missingDealerCard $ mostRecentRound $ playerMem hist then encodeCard dealerCard else ""
--         -- updatedMem = mem ++ storeDealer ++ encodeNewCard hist hand
--         -- updatedMem = mem ++ storeDealer ++ encodeNewCard hist hand

hasSimulatedAction :: History -> Bool
hasSimulatedAction hist = roundNum < numSimActs
    where
        numSimActs = length $ simAction $ simSetup hist
        roundNum = getTurnNum hist

firstActionThisRound :: History -> Bool
firstActionThisRound hist = null $ roundActions $ mostRecentRound $ playerMem hist

-- get random action for simulation
getRandomAction :: History -> Hand -> Encoding Action
getRandomAction history hand = (fromMaybe defaultAction randomAction, show pureRandAction)
    where
        curRound = mostRecentRound $ playerMem history
        randSeed = getRandSeed curRound hand
        validActions = legalActions curRound hand
        pureRandAction = chooseRandomAction randSeed validActions
        randomAction = pureActionToAction pureRandAction $ roundBid curRound

-- get as different a seed as possible from the current hand
getRandSeed :: RoundMem -> Hand -> RandState
getRandSeed rMem h = length (roundActions rMem) * roundStartPts rMem * product (toPoints <$> h) + sum (length . show <$> h)

missingDealerCard :: RoundMem -> Bool
missingDealerCard = (==0) . roundDealer


-- dealerCard = Card Spade Two
-- playerHand = [Card Heart Five, Card Diamond Two, Card Spade Four]
-- roundMem = RoundM [PHit] [5, 2, 4] 1000 200 2

-- testGetAction = getActionFromMCT dealerCard playerHand roundMem

testStrMem = "/12$91B485H6SWin/29$28B777DH2SLoss/48$22B845"
testMemMCTS = playerMem $ getHistory testStrMem

card = Card Spade Eight

hand = [Card Diamond Four, Card Heart Five]

testMCTS = mcts testStrMem testMemMCTS card hand

mcts :: String -> Memory -> Card -> Hand -> Encoding Action
mcts strMem mem dealerCard playerHand = fromMaybe (defaultAction, "") action
    where
        action = do
            let (cur:rest) = mem
            prevRoundGameStates <- memoryToGameState rest
            (updatedMCT, _) <- storeMemoryInMCT prevRoundGameStates
            (suggestedAction, pureAct) <- getActionFromMCT dealerCard playerHand cur updatedMCT
            let randState = getRandSeed cur playerHand
            simInfos <- simulateNTimes 50 cur pureAct randState
            updatedMem <- encodeSimulations strMem simInfos
            pure (suggestedAction, updatedMem)

-- update memory string to include simulation info
encodeSimulations :: String -> [SimInfo] -> Maybe String
encodeSimulations memStr allSimMem = do
    simStr <- traverse encodeSingleSimulation allSimMem
    (reversedCur, reversedRest) <- splitAtFirst '/' $ reverse memStr
    pure $ reverse reversedRest ++ tail (fold simStr) ++  "/" ++ reverse reversedCur
    -- ""
    -- where
    --     simStr = fromMaybe "" $ traverse encodeSingleSimulation allSimMem
    --     (reversedCur, reversedRest) = splitAtFirst '/' $ reverse memStr
    --     updatedPrev = (reverse reversedRest) ++ simStr

splitAtFirst :: Eq a => a -> [a] -> Maybe ([a], [a])
splitAtFirst x l= case elemIndex x l of
    (Just i) -> Just $ splitAt i l
    Nothing -> Nothing

-- unread :: Char -> String -> Maybe String
-- unread _ "" = Nothing
-- unread c str@(char:rest) = if c == char then Just str else unread c rest


testEncodeSim = encodeSingleSimulation $ RoundM [PStand, PHit] [5,2, 10] [Five, Two, King] 482 10 9 (Just Win)

-- make this RoundMem instance of show
encodeSingleSimulation :: SimInfo -> Maybe String
encodeSingleSimulation (RoundM simActs _ simRanks simPts simBid simDealer simOutcome) = do
    dRank <- generateRankFromVal simDealer
    sRes <- simOutcome
    let allActs = fold $ show <$> simActs
        allRanks = fold $ show <$> simRanks
    pure $ "/" ++ show simPts ++ "$" ++
        show simBid ++ "B" ++
        show dRank ++
        allRanks ++
        allActs ++
        show sRes
    -- let
    --     retStr = (\dRank -> "/" ++ show simPts ++ "$" ++ 
    --         show simBid ++ "B" ++ 
    --         show dRank ++ 
    --         show simRanks ++ 
    --         show simActs ++ 
    --         show simOutcome) =<< generateRankFromVal simDealer
    -- in pure retStr


getActionFromMCT :: Card -> Hand -> RoundMem -> Tree -> Maybe (Action, PureAction)
getActionFromMCT dealerCard hand roundMem tree = action
    where
        gameState = getCurrentGameState dealerCard hand
        possibleActionIDs = show <$> legalActions roundMem hand
        -- seedFromHand = getRandSeed roundMem hand
        action = do
            gameStateTree <- findSubTree gameState tree  -- tree corresponding to the current game state
            actionSpace <- moveLeft gameStateTree
            chosenActionTree <- getActionOnLargestUCB actionSpace possibleActionIDs
            let actionTree =  getTree chosenActionTree
            actionTreeToAction roundMem actionTree
            -- simResult <- simulateNTimes 50 roundMem pureActionFromTree seedFromHand
            -- pure actionFromTree



        -- action = fromMaybe defaultAction $ actionTreeToAction roundMem actionTree

-- playerAction (Just s) = case parse strictParseAll s of
--     Error _ -> defaultCompleteAction
--         -- case action of
--         -- Just act -> (act, "")
--         -- Nothing -> defaultCompleteAction -- default to stand if error (should not be called)
--         -- where
--         --     simAction = parse parseSim s
--         --     action = simActionToAction simAction
--     Result rest p ->
--         (Hit, "") -- CHANGE WITH THE NON-SIMULATION ACTION
--         where
--             thisRound : prevRounds = p
--             legalActionsThisRound = legalActions thisRound
--             -- zip

type Encoding a = (a, String)

-- perform the fixed action 
performSimAction :: History -> Encoding Action
performSimAction hist = (fromMaybe defaultAction action, show pureAct)
    where
        turnNum = getTurnNum hist
        (Setup bid simActs) = simSetup hist
        pureAct = simActs !! turnNum
        action = pureActionToAction pureAct bid

getTurnNum :: History -> Int
getTurnNum = length . roundActions . mostRecentRound . playerMem


chooseRandomAction :: RandState -> [PureAction] -> PureAction
chooseRandomAction seed pureActions = snd $ head $ sortOn fst $ zip (generateRandSeq $ nextRand seed) pureActions

testChooseRand s = sortOn fst $ zip (generateRandSeq $ nextRand s) [PHit, PStand, PDoubleDown, PSplit]

generateRandSeq :: RandState -> [RandState]
generateRandSeq = iterate nextRand


-- simActionToAction :: ParseResult SimAction -> Maybe Action
-- simActionToAction (Error _) = Nothing
-- simActionToAction (Result _ (SimAction b a)) = pureActionToAction a b

--------------------------------------------------------
---------- PARSING STRING TO CUSTOM DATA TYPE ----------
--------------------------------------------------------

-- only stores the current round's actions
-- data Memory = Memory {
--     actions :: [Action],
--     hand :: Hand
--     -- TODO: mc tree
-- }


-- change the current turn sep (*) to previous term sep (/)
-- only keep the hand
updateLastOnBid :: String -> String
updateLastOnBid mem = replaceChar '*' '/' mem ++ "*"
-- updateLastOnBid [] = ""
-- updateLastOnBid ('*':xs) =  '/' : filter isNumber xs -- only keep numbers
-- updateLastOnBid (x:xs) = x : updateLastOnBid xs


-- updateCharConverter ::

----------DESERIALIZATION----------

data PureAction = PHit | PStand | PDoubleDown | PSplit | PInsurance
    deriving (Eq)

instance Show PureAction where
    show act = case act of
        PHit -> "H"
        PStand -> "S"
        PDoubleDown -> "D"
        PSplit -> "P"
        PInsurance -> "I"

pureActionMapping :: Points -> [(PureAction, Action)]
pureActionMapping p = [(PStand, Stand),(PHit, Hit),(PDoubleDown, DoubleDown p),(PSplit, Split p),(PInsurance, Insurance p)]

-- testActionMapping = snd <$> find ((==PStand) . fst) (pureActionMapping 20)

pureActionToAction :: PureAction -> Int -> Maybe Action
pureActionToAction pAction bid = snd <$> pair
    where
        pair = find ((==pAction) . fst) (pureActionMapping bid)

    -- let
    --     pair = find ((==pAction) . fst) (pureActionMapping bid)
    --     getAction :: Maybe (PureAction)

actionMapping :: [(Char, PureAction)]
actionMapping = [('S', PStand), ('H', PHit), ('D', PDoubleDown), ('P', PSplit), ('I', PInsurance)]

charToPureAction :: Char -> Maybe PureAction
charToPureAction c = snd <$> pair
    where pair = find ((==c) . fst) actionMapping

type CardInfo = (Char, Int)

data RoundMem = RoundM {
    roundActions :: [PureAction],
    roundHand :: [Int],
    roundRanks :: [Rank],
    roundStartPts :: Points,
    roundBid :: Int,
    roundDealer :: Int,
    roundOutcome :: Maybe Outcome
}

instance Show RoundMem where
    show (RoundM a h r p b d out) =
        "Actions this round: " ++ show a ++
        "  Hands this round: "++ show h ++
        "  Ranks in hand: " ++ show r ++
        "  Points at start of round: " ++ show p ++
        "  Bids this round: " ++ show b ++
        "  Dealer's first card this round: " ++ show d ++
        "  Outcome: " ++ show out

-- roundMemToStr :: RoundMem -> String
-- roundMemToStr (RoundM rActions rHand rDealer rBid) = 
--     show rBid ++ "B" --bid
--     ++ show rDealer -- dealer
--     -- ++ 

-- actionHandOrdering :: PureAction -> [Int] -> String
-- actionHandOrdering pA h = case pA of
--     PHit -> showLstHead h
--     PStand -> ""
--     PDoubleDown -> ""
--     PSplit -> 
--     where
--         showFirstTwo = showLstHead . showLstHead

showLstHead :: Show a => [a] -> String
showLstHead (x:_) = show x
showLstHead [] = ""

-- there should only be one dealer and bid, so just take the first instance
instance Semigroup RoundMem where
    (<>) (RoundM a1 h1 r1 p1 b1 d1 out1) (RoundM a2 h2 r2 p2 b2 d2 out2) =
        RoundM (a1++a2) (h1++h2) (r1++r2) (max p1 p2) (max b1 b2) (max d1 d2) (max out1 out2)

instance Monoid RoundMem where
    mempty = RoundM [] [] [] 0 0 0 Nothing

type Memory = [RoundMem]

data SimSetup = NoSetup | Setup {
    simBid :: Int,
    simAction :: [PureAction]
} deriving (Show, Eq)

data History = History {
    -- isSimulation :: Bool,
    simSetup :: SimSetup,
    playerMem :: Memory
} deriving (Show)

instance Semigroup History where
    (<>) (History setup1 mem1) (History _ mem2)=
        History setup1 $ mem1 <> mem2

instance Monoid History where
    mempty = History NoSetup []

parseMem :: Parser RoundMem
parseMem = P (\i -> case parse parseMemAction i of
    Result rest res -> Result rest $ memInitializeAction res
    Error _ -> case parse parseHand i of
        Result rest res -> Result rest $ memInitializeHand res
        Error e -> Error e
    )

parseSim :: Parser Char
parseSim = is '?'

parseHistory :: Parser History
parseHistory = P (\s -> case parse parseSim s of
    Result rest _ ->
        case parse parseSimSetup rest of
            Result rest2 parsed -> History parsed <$> parse looseParseAll rest2
            Error e -> Error e
    Error _ -> History NoSetup <$> parse strictParseAll s
    )

parseSimSetup :: Parser SimSetup
parseSimSetup = do
    bid <- parseNumAndSeparator 'B'
    simActions <- list1 parseMemAction
    pure $ Setup bid simActions

looseParseAll :: Parser Memory
looseParseAll = parseAll list

strictParseAll :: Parser Memory
strictParseAll = parseAll list1

parseAll :: (Parser RoundMem -> Parser [RoundMem]) -> Parser Memory
parseAll f = do
    l <- f (is '/' >> parseMoreMem) -- has to succeed at least once
    -- outcome <- list
    pure $ reverse l -- latest round is at the head
    -- pure(tempMeml)
    -- (foldl (\(M act1 h1) (M act2 h2) -> M ([act2, act1]) [h2, h1]) (mempty)) =<< (list (is '/' >> parseMoreMem))


-- parseOutcome :: Parser Outcome
-- parseOutcome 

strRankMapping :: [(Char, Rank)]
strRankMapping = flip zip [Ace ..] $ head . show <$> [Ace ..]

strToRank :: Char -> Maybe Rank
strToRank str = snd <$> pair
    where
        pair = find ((==str).fst) strRankMapping

memInitializeAction :: PureAction -> RoundMem
memInitializeAction act = mempty {roundActions=[act]}

memInitializeHand :: CardInfo -> RoundMem
memInitializeHand (str, val) = resultingMem $ strToRank str
    where
        resultingMem :: Maybe Rank -> RoundMem
        resultingMem Nothing = mempty
        resultingMem (Just r) = mempty {roundHand=[val], roundRanks=[r]}


-- memInitializeDealer :: Int -> RoundMem
-- memInitializeDealer d = mempty {roundDealer=d}

-- memInitializeBid :: Int -> RoundMem
-- memInitializeBid b = mempty {roundBid=b}

-- memInitializePts :: Points -> RoundMem
-- memInitializePts pts = mempty {roundStartPts=pts}

memInitializeRound :: Points -> Int -> Int -> Maybe Outcome -> RoundMem
memInitializeRound = RoundM [] [] []

parseMemList :: Parser RoundMem -> Parser RoundMem
parseMemList p1 = parseMemList1 p1 ||| pure mempty

parseMemList1 :: Parser RoundMem -> Parser RoundMem
parseMemList1 p = do
    p' <- p
    p'' <- parseMemList p
    pure (p' <> p'')


-- parseStartPts :: Parser Points
-- parseStartPts = do
--     startPts <- list parseDigit
--     _ <- is ''

parseNumAndSeparator :: Char -> Parser Int
parseNumAndSeparator c = do
    digits <- list parseDigit
    _ <- is c
    pure $ appendNum digits

-- parses PureActions + Int does not deal with separators
parseMoreMem :: Parser RoundMem
parseMoreMem = do
    pts <- parseNumAndSeparator '$'
    bid <- parseNumAndSeparator 'B'
    (_, dealer) <- parseHand
    tm <- parseMemList parseMem
    out <- list parseOutcome
    let roundOutcome = if null out then Nothing else Just $ head out
        updatedTm = replaceLatestHand tm <$> fromMaybe (roundHand tm) $ evaluateAce (roundHand tm)
        ret = updatedTm <> memInitializeRound pts bid dealer roundOutcome
    pure ret


-- parseOutcome :: Parser Outcome
-- parseOutcome :: Parser [Char]
parseOutcome :: Parser Outcome
parseOutcome = foldr1 (|||) $ mapping stringConverter outcomeMapping
-- parseOutcome = foldr1 (|||) $ traverse is . show <$> [Win ..]

stringConverter :: Converter String a a
stringConverter str r = traverse_ is str >> pure r

outcomeMapping :: [(String, Outcome)]
outcomeMapping = [("Win", Win), ("Loss", Loss), ("Draw", Draw)]

-- replace hand (only occurs if there is an ace and it flips from 11 to 1)
replaceLatestHand :: RoundMem -> [Int] -> RoundMem
replaceLatestHand mem newHand = mem {roundHand= newHand}


-- data TempMem = M {
--     action :: History PureAction,
--     hand :: History Int,
--     dealer :: [Int],
--     bid :: [Int]
--     -- bid :: History Int
-- }



-- instance Show TempMem where
--     show (M a h d b) =
--         "Actions so far: " ++ show a ++
--         "  Hands so far: "++ show h ++
--         "  Dealer's first card so far: " ++ show d ++
--         "  Bids so far: " ++ show b

-- instance Semigroup TempMem where
--     (<>) (M a1 h1 d1 b1) (M a2 h2 d2 b2) =
--         M (concatInnerList a1 a2) (concatInnerList h1 h2) (d1++d2) (b1++b2)

-- instance Monoid TempMem where
--     mempty = M [[]] [[]] [] []

-- memtest = M [[PStand]] [[1]] <> M [[]] [[]]
-- test = liftM2 (++) [[1]] [[2]] 

handMapping :: [(Char, Int)]
handMapping = [('A', 11), ('T', 10), ('J', 10), ('K', 10), ('Q', 10)]

valToSymbol :: Int -> Maybe Char
valToSymbol val = fst <$> pair
    where pair = find ((==val) . snd) handMapping


-- parseMem :: String -> Maybe TempMem
-- parseMem s = case parse parseAction s of
--     Error _ -> case parse parseHand s of
--         Result rest res -> Just $ M {action=[], hand=[res]}
--         e -> Nothing
--     Result rest res -> Just $ M {action=[res], hand=[]}

-- parseMem :: Parser TempMem
-- parseMem = P (\i -> case parse parseMemAction i of
--     Result rest res -> Result rest $ memInitializeAction res
--     Error _ -> case parse parseHand i of
--         Result rest res -> Result rest $ memInitializeHand res
--         Error e -> Error e
--     )

-- memInitializeAction :: PureAction -> TempMem
-- memInitializeAction act = mempty {action=[[act]]}

-- memInitializeHand :: Int -> TempMem
-- memInitializeHand h = mempty {hand=[[h]]}

-- memInitializeDealer :: Int -> TempMem
-- memInitializeDealer d = mempty {dealer=[d]}

-- memInitializeBid :: Int -> TempMem
-- memInitializeBid b = mempty {bid=[b]}

-- connects pureAction with |||, i.e. is Action 'S' PStand ||| is Action 'H' PHit ||| ...
parseMemAction :: Parser PureAction
parseMemAction = foldr (|||) (head pureAction) pureAction

parseHand :: Parser CardInfo
parseHand = parseSpecialHand ||| parseNumericHand

parseSpecialHand :: Parser CardInfo
parseSpecialHand = foldr (|||) (head handVal) handVal


-- map isAction to actionMapping where character is first element of the tuple and
-- PureAction is the second element
pureAction :: [Parser PureAction]
pureAction = convertTypeChar actionMapping

pairConverter :: Char -> b -> Parser (Char, b)
pairConverter c r = is c >> pure (c, r)

handVal :: [Parser CardInfo]
handVal = convertToPair handMapping

convertToPair :: [(Char, a)] -> [Parser (Char, a)]
convertToPair = mapping pairConverter

convertTypeChar :: [(Char, a)] -> [Parser a]
convertTypeChar = mapping charConverter

mapping :: Converter a b c -> [(a, b)] -> [Parser c]
mapping f l = uncurry f <$> l

type Converter a b c = a -> b -> Parser c
-- type CharConverter a = Char -> a -> Parser a

-- if it is that character convert it to a Parser of the given PureAction
-- isAction :: CharConverter PureAction
-- isAction c act = is c >> pure act

-- parseHand = ace ||| ten ||| parseDigit
parseNumericHand :: Parser (Char, Int)
parseNumericHand = do
    digit <- parseDigit
    pure (head $ show digit, digit)

parseDigit :: Parser Int
-- TODO: dont allow 0 by using elem and \\ (list difference)
-- <&> is the flipped version of <$>
-- takes context then function
parseDigit = satisfy isDigit <&> digitToInt

-- isSpecialDigit :: CharConverter Int
-- isSpecialDigit = charConverter

charConverter :: Converter Char a a
charConverter c r = is c >> pure r

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- character
    if f c then pure c else unexpectedCharParser c

-- parseBid :: Parser Int
-- parseBid = do
--     bidDigits <- list parseDigit
--     _ <- is 'B'
--     pure $ appendNum bidDigits

-- parses PureActions + Int does not deal with separators
-- parseMoreMem :: Parser TempMem
-- parseMoreMem = do
--     bid <- parseBid
--     dealer <- parseHand
--     tm <- tempMemList parseMem
--     let (Just updatedTm) = replaceLatestHand tm <$> evaluateAce (head $ hand tm)
--     let ret = updatedTm <> memInitializeDealer dealer <> memInitializeBid bid
--     pure ret

appendNum :: Num a => [a] -> a
appendNum = foldl1 (\acc cur -> acc*10 + cur)

-- replace hand (only occurs if there is an ace and it flips from 11 to 1)
-- replaceLatestHand :: TempMem -> [Int] -> TempMem
-- replaceLatestHand mem@(M _ h _ _) newHand = mem {hand=newHand : tail h}
-- -- replaceLatestHand mem newHand = M a (newHand : tail h) d

-- tempMemList :: Parser TempMem  -> Parser TempMem
-- tempMemList p1 = tempMemList1 p1 ||| pure mempty

-- tempMemList1 :: Parser TempMem -> Parser TempMem
-- tempMemList1 p = do
--     p' <- p
--     p'' <- tempMemList p
--     pure (p' <> p'')

-- shallowMemConcat :: TempMem -> TempMem -> TempMem
-- shallowMemConcat (M act1 h1 d1 b1) (M act2 h2 d2 b2) = M (act2++act1) (h2++h1) (d2++d1) (b2++b1)

-- parseAll :: Parser TempMem
-- -- TODO: MAKE THIS FOLD AN MCONCAT INSTEAD, I.E. MOVE SHALLOWMEMCONCAT TO MCONCAT
-- parseAll = do
--     tempMeml <- list1 (is '/' >> parseMoreMem) -- has to succeed at least once
--     pure (foldl1 shallowMemConcat tempMeml)
--     -- pure(tempMeml)
--     -- (foldl (\(M act1 h1) (M act2 h2) -> M ([act2, act1]) [h2, h1]) (mempty)) =<< (list (is '/' >> parseMoreMem))

-- data SimAction = SimAction {
--     simBid :: Int,
--     simAction :: PureAction
-- }

-- parseSim :: Parser SimAction
-- parseSim = do
--     simBid <- parseNumAndSeparator 'B'
--     SimAction simBid <$> parseMemAction

-- playerAction :: Maybe String -> CompleteAction
-- playerAction Nothing = (Hit, "") -- playerMem should never be nothing in non-bid rounds
-- playerAction (Just s) = case parse parseAll s of
--     Error _ -> case action of
--         Just act -> (act, "")
--         Nothing -> defaultCompleteAction -- default to stand if error (should not be called)
--         where
--             simAction = parse parseSim s
--             action = simActionToAction simAction
--     Result rest p -> (Hit, "") -- CHANGE WITH THE NON-SIMULATION ACTION


-- simActionToAction :: ParseResult SimAction -> Maybe Action
-- simActionToAction (Error _) = Nothing
-- simActionToAction (Result _ (SimAction b a)) = pureActionToAction a b



-- sepby1 :: Parser a -> Parser s -> Parser [a]
-- sepby1 p1 sep = do
-- --   first <- p1
-- --   l <- list (sep >> p1)  
-- --   pure (first:l)
--     list (sep >> p1)

-- test = sepby1 (parseMoreMem) (is '/')

-- sepby :: Parser a -> Parser s -> Parser [a]
-- sepby p sep = sepby1 p sep ||| pure []


list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = do
  p' <- p
  p'' <- list p
  pure (p' : p'')

----------HELPER FUNCTIONS----------
replaceChar :: Char -> Char -> String -> String
replaceChar lookc replacec = map' (==lookc) (const replacec)
-- convertChar lc rc = \c -> if c == lc then rc else 

concatInnerList :: [[a]] -> [[a]] -> [[a]]
concatInnerList = liftM2 (++)

evaluateAce :: [Int] -> Maybe [Int]
evaluateAce h = if sum h > 21 then changeFirst h 11 1 else Just h

changeFirst :: Eq a => [a] -> a -> a -> Maybe [a]
changeFirst [] _ _ = Nothing
changeFirst (x:xs) i r
    | x == i = Just $ r:xs
    | otherwise = (x:) <$>  changeFirst xs i r


----------SERIALIZATION----------

-- TODO!!

-- 

-- getPlayerMem :: Input -> Memory
-- getPlayerMem = extractFromParseResult [] . parse parseAll



-- takes a ParseResult and a default value to fall back on, 
-- gets the result or returns the default value
extractFromParseResult :: a -> ParseResult a -> a
extractFromParseResult def (Error _) = def
extractFromParseResult _ (Result _ result) = result

-- to store outcome of round (win/loss/draw)
data Outcome = Win | Draw | Loss deriving(Show, Enum, Eq, Ord)


-- BIG TODO!! REFACTOR
storeStartOfRound :: Maybe String -> [PlayerPoints] -> PlayerId -> Hand -> String
storeStartOfRound (Just mem) allPts pId pHand=
    let
        h@(History sim playerMem) = getHistory mem
        -- memorySoFar = getPlayerMem mem
        -- prevRoundStartPts =  roundStartPts $ mostRecentRound memorySoFar
        curRoundStartPts = getPlayerPoints allPts pId
        prevOutcome = getPrevOutcome playerMem curRoundStartPts
        defaultStr :: Int -> String
        defaultStr bid = "/" ++ show curRoundStartPts ++ "$" ++ show bid ++ "B"

        retStr = if isSimulation h then defaultStr $ simBid sim
            else show prevOutcome ++ defaultStr defaultBid
    in
        trace("Memory in store start of round: " ++ mem ++ " History in store start of round: " ++ show h)
        retStr
storeStartOfRound Nothing allPts pId pHand = let
    curRoundStartPts = getPlayerPoints allPts pId
    in
        trace ("allPts: " ++ show allPts)
        "/" ++ show curRoundStartPts ++ "$" ++ show defaultBid
        ++ "B"

getPrevOutcome :: Memory -> Points -> Outcome
getPrevOutcome mem = outcome prevRoundStartPts
    where
        prevRoundStartPts =  roundStartPts $ mostRecentRound mem


outcome :: Points -> Points -> Outcome
outcome prevP curP
    | curP > prevP = Win
    | curP == prevP = Draw
    | otherwise = Loss

storeAction :: String -> Action -> String
storeAction mem act = mem ++ show act



-- take in memory add in any new cards
encodeNewCard :: History -> Hand -> String
encodeNewCard hist h
    | trace ("IN ENCODE CARD:" ++ show h) False = undefined
    | null roundMems || toUpdate = encodeCard $ head h
    | otherwise = ""
    where
        roundMems = playerMem hist
        mostRecentHand = if null roundMems then [] else roundHand $ mostRecentRound roundMems
        toUpdate = length h /= length mostRecentHand

testPerformedSplit = RoundM [PSplit] [5,4] [Five, Four] 1000 22 10 Nothing

-- check if 
performedSplit :: RoundMem -> Bool
performedSplit = elem PSplit . roundActions

encodeCard :: Card -> String
encodeCard card = case valToSymbol $ toPoints card of
    Just symbol -> [symbol]
    Nothing -> show $ toPoints card
-- storeUpdatedHand mem h =
--     let
--         memorySoFar =  mem
--         handInMem = roundHand $ mostRecentRound memorySoFar
--         addCard = length h == length handInMem
--         (mostRecentCard: _) = h
--     in
--         if addCard
--             then mem ++ show (rankToValDeck (sum handInMem) (getRank mostRecentCard))
--             else mem

-- convert rank to val considering the sum of the hand (Aces might flip)
rankToValDeck :: Int -> Rank -> Int
rankToValDeck hVal r
    | hVal > 10 && indVal == 11 = 1
    | otherwise = indVal
    where
        indVal = rankToVal r

rankToVal :: Rank -> Int
rankToVal r
    | isDigit $ head strRank = read strRank
    | strRank == "A" = 11
    | otherwise = 10
    where
        strRank = show r


testCurrentGameState = getCurrentGameState (Card Spade Four) testHand2

-- testRMem = RoundM [PHit, PStand] [4, 3, 9] 1000 200 7 (Just Win)

-- testRMtoGS = roundMemToGameState testRMem (Just []) []

testRM1 = RoundM [PHit, PStand] [9, 2, 4] [Nine, Two, Four] 1000 200 8 (Just Win)
testRM2 = RoundM [PHit, PHit] [10, 2, 8] [Jack, Two, Eight] 1000 200 11 (Just Loss)
testRM3 = RoundM [PDoubleDown, PHit, PStand] [10, 2, 7] [Queen, Two, Seven] 1000 200 4 (Just Draw)
testRM5 = RoundM [PHit, PStand] [10,5, 4] [Ten, Five, Four] 1000 200 4 (Just Win)

testRM4 = RoundM [PSplit, PHit, PStand] [4, 4, 9] [Four, Four, Nine] 1000 200 8 (Just Win)

testConversion = roundMemToGameState testRM4 (Just []) []

testMem = [testRM1, testRM2, testRM3, testRM4, testRM5]

testAllConversion = memoryToGameState testMem

-- convert all previous rounds in memory to gamestate
memoryToGameState :: Memory -> Maybe [[GameState]]
memoryToGameState = mapM roundMemConversion
    where
        roundMemConversion cur = roundMemToGameState cur (Just []) []

-- to convert a single round to game states, doesnt do it for current rount see getCurrentGameState
roundMemToGameState :: RoundMem -> Maybe [GameState] -> [Int] ->Maybe [GameState]
roundMemToGameState _ Nothing _= Nothing
-- processed all the actions and all cards
-- roundMemToGameState (RoundM [] _ _ _ _ _) gStateSoFar _= gStateSoFar
roundMemToGameState RoundM {roundActions=[]} gStateSoFar _= gStateSoFar
roundMemToGameState rMem _ []= roundMemToGameState rMem (Just []) (take 2 $ roundHand rMem)
roundMemToGameState rMem@RoundM{roundActions, roundHand, roundDealer, roundOutcome} gStateSoFar handForAction
    | performedSplit rMem = Just [splitGState]
    | otherwise =
        trace ("ROUND ACTIONS: " ++ show roundActions ++
        " Action being processed: "++ show action ++
        " cards Obtained: " ++ show cardsObtained ++
        " Passed in hand: " ++ show handForAction ++
         "  New hand: " ++ show newHandForAction)
        roundMemToGameState rMem{roundActions=rest} updatedGStateSoFar newHandForAction
    where
        -- for split
        splitCards = take 2 roundHand
        splitHandInfo = HandInfo (isSoft splitCards) True (sum splitCards)
        splitGState = GState roundDealer (length splitCards) splitHandInfo (Just PSplit) roundOutcome
        -- for non split
        -- action = last roundActions
        -- rest = take (length roundActions-1) roundActions
        (action:rest) = roundActions
        numCards = length handForAction
        handInfo = HandInfo (isSoft handForAction) (isPair rMem) (sum handForAction)
        cardsObtained = numCardObtainedFromAction action
        newHandForAction = handForAction ++ take cardsObtained (drop numCards roundHand)
        newGameState = GState roundDealer numCards handInfo (Just action) roundOutcome
        updatedGStateSoFar :: Maybe [GameState]
        updatedGStateSoFar = do
            state <- gStateSoFar
            pure $ newGameState : state

-- currentRound = RoundM [] [9, 2] 1000 293 2

-- otherRound = RoundM [PDoubleDown, PHit, PStand] [11, 2, 6] 2000 12 11

-- testMemory = [currentRound,testRMem, otherRound]

convertAllMemoryToGameState :: Memory -> [GameState]
convertAllMemoryToGameState memory = fromMaybe [] maybeGstates
    where
        maybeGstates = join <$> sequence ((\rMemory -> roundMemToGameState rMemory (Just []) []) <$> tail memory)



-- !!!! REMEMBER TO ADD IN SPLIT LATER !!!
numCardObtainedFromAction :: PureAction -> Int
numCardObtainedFromAction PHit = 1
numCardObtainedFromAction _ = 0

testGState = getCurrentGameState (Card Spade Two) [Card Heart Four, Card Diamond Ace]

getTreeForCurGState = fromMaybe (None, []) $ findSubTree testGState mct

-- getLegalActions = show <$> legalActions (RoundM [] [4, 11] 1000 100 2)

-- testgetAction = fst $ fromMaybe (None, []) $ getActionOnLargestUCB (fromMaybe (None, []) $ moveLeft $ fromMaybe (None, []) $ findSubTree testGState mct) (getLegalActions)

testFind = findSubTree testGState mct

-- convert state for the current round to GameState
getCurrentGameState :: Card -> Hand -> GameState
getCurrentGameState dealerCard pHand = GState dCardVal handLen handInfo Nothing Nothing
    where
        dCardVal = rankToValDeck 0 (getRank dealerCard)
        handLen = length pHand
        handInfo = HandInfo (handIsSoft pHand) (handIsPair pHand) (sum $ calcHandVal pHand)

testHand2 = [Card Spade Two, Card Diamond Two]

calcHandVal :: Hand -> [Int]
calcHandVal h = fromMaybe [] $ evaluateAce defaultEval
    where
        defaultEval = rankToValDeck 0 . getRank<$> h

-- extractFromMaybe :: a -> Maybe a -> a
-- extractFromMaybe _ (Just x) = x
-- extractFromMaybe def Nothing = def


handIsSoft :: Hand -> Bool
handIsSoft = elem 11 . calcHandVal

isSoft :: [Int] -> Bool
isSoft = elem 11

handIsPair :: Hand -> Bool
handIsPair h = length h == 2 && c1 == c2
    where
        intVals = rankToVal . getRank <$> h
        [c1, c2] = intVals

-- have to have exactly 2 cards and same rank and same value
isPair :: RoundMem -> Bool
isPair RoundM {roundActions, roundHand, roundRanks} = length roundHand == pairLen && c1 == c2
    where
        numSplits = count PSplit roundActions
        pairLen = 2 + numSplits
        [c1 , c2] = drop numSplits $ zip roundHand roundRanks

-- count occurrences of a value in a list
count :: Eq a => a -> [a] -> Int
count val = foldr (\cur acc -> if cur == val then acc + 1 else acc) 0



-- instance Eq PlayerPoints where
--     (PlayerPoints id1 _) == (PlayerPoints id2 _) = id1 == id2

-- give a name to head Memory so its not confusing
mostRecentRound :: Memory -> RoundMem
mostRecentRound = head

getPlayerPoints :: [PlayerPoints] -> PlayerId -> Points
getPlayerPoints l pId = extractPlayerPts $ find ((==pId) . getId) l
    where
        -- auxiliary function which defaults to 0 if player not found
        extractPlayerPts :: Maybe PlayerPoints -> Points
        extractPlayerPts (Just p) = getPoints p
        extractPlayerPts Nothing = 0

-- Converts nested list to string based on their show
-- ex: [[PStand, PHit], [PSplit]] = [["S","H"], ["P"]]
-- historyToStr :: (a -> String) -> History a -> History String
-- historyToStr = (<$$>)

-- showHand :: Int -> String
-- showHand n
--   | n == 10 = "T"
--   | n == 11 || n == 1 = "A"
--   | otherwise = show n

-- testActionHistoryToStr = historyToStr show [[PStand, PHit], [PSplit]]
-- testHandHistoryToStr = historyToStr showHand [[1, 4, 2], [8]]


-- groupMem :: TempMem -> RoundMem
-- groupMem (a h d b) = foldr () [RoundM ]
--     where
--         numRounds = length a
--         firstRound = RoundM 

-- recreateMem :: TempMem -> String
-- recreateMem (actions hands dealers bids) = 
--     foldr 

---------------------------------------------------
-------------------- MCTS TREE --------------------
---------------------------------------------------

--reference: http://learnyouahaskell.com/zippers

-- data CrumbInfo = CrumbI {node :: NodeInfo, r :: Tree}

-- TODO: TURN CRUMB INTO RECORD AND OPTIMIZE FUNCTIONS USING IT
data Crumb = LeftCrumb NodeInfo Tree | RightCrumb NodeInfo Tree
    deriving(Show)

type Breadcrumbs = [Crumb]

type Zipper = (Tree, Breadcrumbs)

getTree :: Zipper -> Tree
getTree (tree, _) = tree

moveLeft :: Zipper -> Maybe Zipper
moveLeft (None, _) = Nothing -- fail for empty tree
moveLeft (Node (Children l r) info, bs) = Just (l, LeftCrumb info r :bs)


-- note: if right child is empty, does not fail, instead returns Just (None, ...)
moveRight :: Zipper -> Maybe Zipper
moveRight (None, _) = Nothing -- fail for empty tree
moveRight (Node (Children l r) info, bs) = Just (r, RightCrumb info l : bs)

moveUp :: Zipper -> Maybe Zipper
moveUp (_, []) = Nothing -- no breadcrumbs to follow fail
moveUp (t, LeftCrumb info r : bs) = Just (Node (Children t r) info, bs)
moveUp (t, RightCrumb info l : bs) = Just (Node (Children l t) info, bs)

returnToRoot :: Maybe Zipper -> Maybe Zipper
returnToRoot (Just (t, [])) = Just (t, [])
returnToRoot (Just z) = returnToRoot (moveUp z)
returnToRoot Nothing = Nothing


modifyNode :: (MCInfo -> MCInfo) -> Zipper -> Maybe Zipper
modifyNode f (Node c (NodeInfo nid mcInfo), bs) = Just (Node c (NodeInfo nid (f mcInfo)), bs)
modifyNode _ _ = Nothing


findAtLevel :: NodeID -> Zipper -> Maybe Zipper
findAtLevel nid z@(t, _)
    | found = Just z
    | not found = moveRight z >>= findAtLevel nid
    where
        found = foundNode t nid
findAtLevel _ _ = Nothing

start :: Zipper
start = (mct, [])

findDealer :: Maybe Zipper
findDealer = findAtLevel "2" start

findCardNum :: Maybe Zipper
findCardNum = findAtLevel "3" (cardNumTree2, [])

modifyCardNum3 :: Maybe Zipper
modifyCardNum3 = do
    node <- findCardNum
    returnToRoot $ modifyNode (incrementMCWins 1) node

-- partially apply to use in tree funcs
incrementMCWins :: Float -> MCInfo -> MCInfo
incrementMCWins _ NoMCInfo = NoMCInfo
incrementMCWins n info = info {wins = wins info + n}

incrementNumSims :: MCInfo -> MCInfo
incrementNumSims NoMCInfo = NoMCInfo
incrementNumSims info = info { numSimsThisNode= numSimsThisNode info + 1 }

incrementNumSimsLevel :: MCInfo -> MCInfo
incrementNumSimsLevel NoMCInfo = NoMCInfo
incrementNumSimsLevel info = info { numSimsSubtreeTotal= numSimsSubtreeTotal info + 1 }

attachLeft :: Tree -> Zipper -> Maybe Zipper
attachLeft t (Node (Children None r) info, bs) = Just (Node (Children t r) info, bs)
attachLeft _ _ = Nothing

attachRight :: Tree -> Zipper -> Maybe Zipper
attachRight t (Node (Children l None) info, bs) = Just (Node (Children l t) info, bs)
attachRight _ _ = Nothing


-- is only called when not in Simulation
storeHistoryInMCT :: History -> Tree
-- does not pass in the current round to store in the MCT
storeHistoryInMCT (History _ (cur:rest)) = undefined

testG1 = GState 1 6 (HandInfo False False 10) (Just PStand) (Just Win)
testG2 = GState 1 5 (HandInfo False False 9) (Just PHit) (Just Loss)
testG3 = GState 2 6 (HandInfo False False 8) (Just PHit) (Just Draw)
testG4 = GState 2 7 (HandInfo False False 10) (Just PStand) (Just Win)

testGl = [testG1, testG2, testG3, testG4]

-- testStoreMem = storeGameStateInMCT testGl (Just (shorterTestMCT, []))

-- storeMemoryInMCT :: [[GameState]]

-- testStoreGState = storeGameStateInMCT testGl (Just (shorterTestMCT, []))
testGStateForStore = GState 4 3 (HandInfo False False 19) (Just PStand) Nothing

testStoreMemInMCT = do
    (storedTree, _) <- storeMemoryInMCT =<< testAllConversion
    gState <- testConversion
    (tree, _) <- findSubTree testGStateForStore storedTree
    pure tree
    -- (\(tree, bs) ->findSubTree testConversion tree) =<< storeMemoryInMCT =<< testAllConversion

-- MCTAction :: 

storeMemoryInMCT :: [[GameState]] -> Maybe Zipper
storeMemoryInMCT = foldr storeGameStateInMCT (Just (mct, []))

storeGameStateInMCT :: [GameState] -> Maybe Zipper -> Maybe Zipper
storeGameStateInMCT [] zipper = zipper -- if memory is empty, base case return accumulator
storeGameStateInMCT (cur:rest) (Just (t, _)) = do
    zipperForThisMem <- findSubTree cur t
    updatedNodeSims <- modifyNode incrementNumSims zipperForThisMem -- num sims for that node + 1
    rewardFunc <- rewardIncrement <$> gameOutcome cur
    updatedWins <- modifyNode rewardFunc updatedNodeSims
    -- updatedLevelSims <- modifyAllInLevel updatedWins incrementNumSimsLevel
    let updatedTree = returnToRoot $ modifyAllInLevel updatedWins incrementNumSimsLevel
    -- let updatedTree = returnToRoot $ modifyNode incrementMCWins updatedNodeSims -- increment wins for that node
    storeGameStateInMCT rest updatedTree
storeGameStateInMCT _ _ = Nothing

rewardIncrement :: Outcome -> (MCInfo -> MCInfo)
rewardIncrement gameOutcome = case gameOutcome of
    Win -> incrementMCWins 1
    Draw -> incrementMCWins 0.5
    Loss -> incrementMCWins 0


findSubTree :: GameState -> Tree -> Maybe Zipper
findSubTree (GState d num hInfo Nothing _) t = do
    let startZ = (t, [])
    -- DONT FORGET TO UNCOMMENT ^
    -- let startZ = (shorterTestMCT, [])
    dealerZ <- findAtLevel (show d) startZ
    cardNumNode <- findAtLevel (show num) =<< moveLeft dealerZ
    findAtLevel (show hInfo) =<< moveLeft cardNumNode
findSubTree (GState d num hInfo (Just act) _) t = do
    let startZ = (t, [])
    -- DONT FORGET TO UNCOMMENT ^
    -- let startZ = (shorterTestMCT, [])
    dealerZ <- findAtLevel (show d) startZ
    cardNumNode <- findAtLevel (show num) =<< moveLeft dealerZ
    cardValNode <- findAtLevel (show hInfo) =<< moveLeft cardNumNode
    findAtLevel (show act) =<< moveLeft cardValNode


actionTreeToAction :: RoundMem -> Tree -> Maybe (Action, PureAction)
actionTreeToAction _ None = Nothing
actionTreeToAction rMem tree = do
    convertedPureAction <- charToPureAction $ head $ nid $ nodeInfo tree
    convertedAction <- pureActionToAction convertedPureAction $ roundBid rMem
    pure (convertedAction, convertedPureAction)


testaf1 = do
    dealerZ <- findAtLevel (show 2) (shorterTestMCT, [])
    cardNumNode <- findAtLevel (show 5) =<< moveLeft dealerZ
    cardValNode <- findAtLevel (show 9) =<< moveLeft cardNumNode
    findAtLevel (show PStand) =<< moveLeft cardValNode

-- testf1 = do
--     dealerZ <- findAtLevel (show 4) (mct, [])
--     findAtLevel (show num) =<< moveLeft dealerZ
-- testF1 = extractRes $ findSubTree $ GState 11 4 (HandInfo False False 20)

extractRes :: Maybe Zipper -> Tree
extractRes Nothing = None
extractRes (Just (t, _)) = t

modifyAllInPath :: (MCInfo -> MCInfo) -> Zipper -> Maybe Zipper
modifyAllInPath f z@(_, []) = modifyNode f z
modifyAllInPath f z = do
    modifiedNode <- modifyNode f z
    goUp <- moveUp modifiedNode
    modifyAllInPath f goUp
    -- pure modifiedNode

-- testModTree = fromMaybe (None, []) $  findSubTree (GState 2 5 (HandInfo False False 9) (Just PStand)) shorterTestMCT
-- testModifyAllInLevel = modifyAllInLevel testModTree incrementNumSimsLevel

-- assume given a node in that level itself
modifyAllInLevel :: Zipper -> (MCInfo -> MCInfo) -> Maybe Zipper
modifyAllInLevel z@(t, _) f
    | rightChild /= None = flip modifyAllInLevelAux f =<< getRightMostInLevel z
    | rightChild == None = modifyAllInLevelAux z f
    where
        rightChild = getRightChild t
modifyAllInLevel _ _ = Nothing

modifyAllInLevelAux :: Zipper -> (MCInfo -> MCInfo) -> Maybe Zipper
modifyAllInLevelAux z@(t, bs) f
    | isLeftCrumb $ head bs = modifyNode f z --reached top of level
    | otherwise = do
        modified <- modifyNode f z
        parent <- moveUp modified
        modifyAllInLevelAux parent f



getRightMostInLevel :: Zipper -> Maybe Zipper
getRightMostInLevel z@(None, _) = moveUp z
getRightMostInLevel z = getRightMostInLevel =<< moveRight z



modifyNodeAndParents :: (MCInfo -> MCInfo) -> NodeID -> Zipper  -> Maybe Zipper
modifyNodeAndParents f nid z@(t, b:bs) = do
    let nodeToModify = foundNode t nid
    let modifyParent = isLeftCrumb b
    let idToModify = if modifyParent then extractCrumbId b else ""
    modifiedNode <- if nodeToModify then modifyNode f z else pure z
    goUp <- moveUp modifiedNode
    modifyNodeAndParents f idToModify goUp
modifyNodeAndParents f nid z@(t, []) = do
    let nodeToModify = foundNode t nid
    if nodeToModify then modifyNode f z else pure z

-- t = do
--     res <- testaf1
--     modifyNodeAndParents incrementMCWins (show PStand) res


isLeftCrumb :: Crumb -> Bool
isLeftCrumb (LeftCrumb _ _) = True
isLeftCrumb (RightCrumb _ _) = False

extractCrumbId :: Crumb -> NodeID
extractCrumbId (LeftCrumb info _) = nid info
extractCrumbId (RightCrumb info _) = nid info

findAndModify :: (MCInfo -> MCInfo) -> GameState -> Maybe Zipper -> Maybe Zipper
findAndModify f state (Just (t, _)) = modifyAllInPath f =<< findSubTree state t
findAndModify _ _ Nothing = Nothing


-- TODO: CHANGE THE SHOW VAL CURHAND TO USE THE =<<, MAYBE TURN TO DO?
findAndModifySome :: (MCInfo -> MCInfo) -> GameState -> Maybe Zipper -> Maybe Zipper
findAndModifySome f state@GState{actionTaken=(Just act)} (Just (t,_)) = modifyNodeAndParents f (show act) =<< findSubTree state t
-- findAndModifySome f state@(GState _ _ _ (Just act)) (Just (t, _)) = modifyNodeAndParents f (show act) =<< findSubTree state t
findAndModifySome f GState{actionTaken=Nothing} (Just (t, _)) = Nothing
findAndModifySome _ _ Nothing = Nothing

-- testFM1 = extractRes $ findAndModifySome incrementMCWins $ GState 11 4 (HandInfo False False 20)

-- finds and modify a list of GameStates using findAndModifySome
modifyOnData :: (MCInfo -> MCInfo) -> [GameState] -> Maybe Zipper
modifyOnData f = foldr (findAndModifySome f) (Just (shorterTestMCT, [])) -- REMEMBER TO CHANGE TO MCT

-- testmODp1 = findAndModify 

-- testmOD = modifyOnData incrementMCWins [GState 2 6 (HandInfo False False 9) (Just PStand),GState 2 5 (HandInfo False False 10) (Just PHit)]

-- findAndModify4 = findAndModify incrementMCWins mockGSTate

    -- modifyAllInPath =<< moveUp =<< (modifyNode f z)

-- mockGSTate = GState 2 3 (HandInfo 4 False)

-- TODO: make this an enumeration of GameState 

data ActionState = AState {
    gameState :: GameState,
    pAction :: PureAction
}

-- pairs :: [ActionState]
-- pairs = AState <$> (GState <$> dealerVals <*> cardNums <*> cardVals) <*> possibleActions

-- checkLength = length pairs

dealerVals :: [Int]
dealerVals = [2..11]

cardNums :: [Int]
cardNums = [2..4]

cardVals :: [HandInfo]
-- cardVals = (createHandConditional (const True) <$> [4..21]) ++ (createHandConditional (>= 13) <$> [4..21])
cardVals = considerHands =<< [4..21]
-- cardVals = map' (>= 13) (\n -> [HandInfo n False, HandInfo n True])

-- test = cardVals <$> cardNums

-- !!!!!BIG TODO: PLEASE SIMPLIFY THIS!!!!!
considerHands :: Int -> [HandInfo]
considerHands n
    | isEven && possibleAce = pair : ace : noAce
    | isEven = pair : noAce
    | possibleAce = ace : noAce
    | otherwise = noAce
    where
        isEven = even n
        possibleAce = possibleAceRange n
        noAce = [basicHand n]
        pair = pairHand n
        ace = aceHand n

possibleActions = [PHit, PStand, PDoubleDown, PSplit, PInsurance]


createLevel :: [String] -> Tree
createLevel [] = None
createLevel [x] = Node noChildren (NodeInfo x defaultMCInfo)
createLevel (x:xs) = Node (Children None (createLevel xs)) (NodeInfo x defaultMCInfo)

-- creates a new level for the given node and attaches it to its left
createLevelBelow :: [String] -> Tree -> Tree
createLevelBelow _ None = None
createLevelBelow nids t@(Node c@(Children _ _) _) = t {children=c{left=createLevel nids}}
-- createLevelBelow nids t = createLevelBelow nids t

test = createLevel $ show <$> dealerVals
test2 = createLevelBelow (show <$> cardNums) test

testFmapL = fmapB (createLevelBelow ["*", "/"]) (fmapB (createLevelBelow ["A","B"]) (fmapB (createLevelBelow (show <$> cardNums)) test))


-- !!!BIG TODO: MAKE THIS BETTER
fmapB :: (Tree -> Tree) -> Tree -> Tree
fmapB _ None = None
fmapB f t@(Node (Children None None) _) = f t
fmapB f t@(Node c@(Children l None) _ ) = t{children=c{left=fmapB f l}}
-- apply function to bottom level
fmapB f t@(Node (Children None r) _) = resTree {children=resChildren{left=resL, right=fmapB f r} }
    where
        resTree = f t
        resChildren = children resTree
        resL = left resChildren
fmapB f t@(Node c@(Children l r) _) = t{children=c{left=fmapB f l, right=fmapB f r}}

attachLevel :: [String] -> Tree -> Tree
attachLevel = fmapB . createLevelBelow

mct :: Tree
mct =
    let
        dealerLevel = createLevel $ show <$> dealerVals
        cardNumLevel = attachLevel (show <$> cardNums) dealerLevel
        handLevel = attachLevel (show <$> cardVals) cardNumLevel
        resTree = attachLevel (show <$> possibleActions) handLevel
    in resTree

shorterTestMCT :: Tree
shorterTestMCT =
    let
        dealerLevel = createLevel $ show <$> [1..2]
        cardNumLevel = attachLevel (show <$> [5..7]) dealerLevel
        handLevel = attachLevel (show <$> [8..10]) cardNumLevel
        actionLevel = attachLevel (show <$> [PStand, PHit]) handLevel
    in
        actionLevel

shorterGState = GState 2 6 (HandInfo False False 10)
-- testFM2 = extractRes $ findAndModifySome incrementMCWins shorterGState

getLeftChild :: Tree -> Tree
getLeftChild = left . children

getRightChild :: Tree -> Tree
getRightChild None = None
getRightChild (Node (Children _ r) _) = r

testT1 = (Node (Children None testT2) (NodeInfo "H" (MCInfo 2 4 16)))
testT2 = (Node (Children None testT3) (NodeInfo "S" (MCInfo 5 10 16)))
testT3 = (Node (Children None testT4) (NodeInfo "P" (MCInfo 1 1 16)))
testT4 = (Node (Children None testT5) (NodeInfo "I" (MCInfo 2 9 16)))
testT5 = (Node (Children None None) (NodeInfo "D" (MCInfo 9 12 16)))

testTlist = [testT1, testT2, testT3, testT4]

ucbs = calcUCB <$> testTlist

tstValid = show <$> [PHit, PStand, PInsurance, PDoubleDown, PSplit]

-- testGetAction = getActionOnLargestUCB (testT1, []) tstValid

getActionOnLargestUCB :: Zipper -> [NodeID] -> Maybe Zipper
-- initialize current t as the largest and move right
getActionOnLargestUCB z@(tree,_) validActs
    | nid (nodeInfo tree) `elem` validActs = getActionOnLargestUCBAux (moveRight z) validActs (Just z)
    | otherwise = getActionOnLargestUCBAux (moveRight z) validActs Nothing

-- aux function for getActionOnLargestUCB
-- params: current tree, valid nodes, best zipper
getActionOnLargestUCBAux :: Maybe Zipper -> [NodeID] -> Maybe Zipper -> Maybe Zipper
-- if traversed the whole branch and no more right child and still no best tree, return nothing
getActionOnLargestUCBAux (Just (None, _)) _ Nothing = Nothing
-- no more right child
getActionOnLargestUCBAux mZ@(Just (None,_)) _ bestZ =
    -- trace ("maybe Zipper: " ++ show mZ)
    bestZ
getActionOnLargestUCBAux (Just curZ@((Node _ nInfo), _)) validActs maybeZ@(Just bestZ)
    -- | trace ("curZ: " ++ show curZ) False = undefined
    | nid nInfo `elem` validActs = getActionOnLargestUCBAux (moveRight curZ) validActs (Just updatedBest)
    | otherwise = getActionOnLargestUCBAux (moveRight curZ) validActs maybeZ
    where
        updatedBest = chooseMaxUCBTree bestZ curZ
getActionOnLargestUCBAux _ _ _ = Nothing


-- testZipper1 = 


type Statistic = Float

chooseMaxUCBTree :: Zipper -> Zipper -> Zipper
chooseMaxUCBTree z1@(t1,_) z2@(t2,_)
    | isNothing ucbT1 = z1
    | isNothing ucbT2 = z2
    | ucbT2 > ucbT1 = z2
    | otherwise = z1
    where
        [ucbT1, ucbT2] = calcUCB <$> [t1, t2]

-- getUCB :: Tree -> Statistic
-- getUCB t = case calcUCB t of
--     Just ucb -> ucb
--     Nothing -> 99 -- replace with some concrete largest number

-- calcUCBtest = calcUCB testT1
-- calc UCB statistic
calcUCB :: Tree -> Maybe Statistic
calcUCB tree = do
    let (MCInfo wins sims simsThisLevel) = mcInfo $ nodeInfo tree
        exploitation = wins / fromIntegral sims
    logRes <- safeLogInts simsThisLevel
    divRes <- safeDiv logRes $ fromIntegral sims
    sqrtRes <- safeSqrt divRes
    let exploration = explorationParam * sqrtRes
    pure $ exploitation + exploration


-- -- calc UCB statistic
-- calcUCB :: MCInfo -> Maybe Statistic
-- calcUCB NoMCInfo = Nothing
-- calcUCB (MCInfo wins sims simsThisLevel) = Just $ exploitation + exploration
--     where
--         exploitation = divInts wins sims
--         exploration = explorationParam * sqrt (logInt simsThisLevel / fromIntegral sims)


safeSqrt :: Float -> Maybe Statistic
safeSqrt f
    | f < 0 = Nothing
    | otherwise = Just $ sqrt f


safeDiv :: Float -> Float -> Maybe Statistic
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a / b

--divide ints to produce a float
safeDivInts :: Int -> Int -> Maybe Statistic
safeDivInts _ 0 = Nothing
safeDivInts a b = Just $ fromIntegral a / fromIntegral b

safeLogInts :: Int -> Maybe Statistic
safeLogInts n
    | n < 1 = Nothing
    | otherwise = Just $ log $ fromIntegral n




-- considerActions :: GState -> PossibleActions
-- considerActions (d cNum (HandInfo a p val))
--     |
--     where
--         defaultActions = [PHit, PStand]


-- considerAce n = if n >= 13 then possibleAce n else noAce n
--     where
--         possibleAce n = createHandWithAce n : noAce n
--         noAce n = [createHandNoAce n]

basicHand :: Int -> HandInfo
basicHand = HandInfo False False

pairHand :: Int -> HandInfo
pairHand = HandInfo False True

aceHand :: Int -> HandInfo
aceHand = HandInfo True False

possibleAceRange n = n >= 13 && n < 21

-- createHandConditional :: (Int -> Bool) -> Int -> HandInfo
-- createHandConditional f n = HandInfo n (f n)
-- dealerVals = undefined 
-- dealerVals = (\num -> HandInfo num (num == 11)) <$> [1..11]

-- test = [1..11] >>= (\n -> pure n)
-- num >>= (num==11) >>= HandInfo
-- data Node = Node {
--     left :: Tree,
--     right :: Tree,
--     handValue :: Int,
--     containsAce :: Bool
-- }

-- instance Eq Node where
--     (==) (Node v1 hasA1) (Node v2 hasA2) = v1 == v2 && hasA1 == hasA2

-- -- Having an ace is less because it is a soft total
-- instance Ord Node where
--     compare (Node v1 hasA1) (Node v2 hasA2) = 
--         let valueComp = compare v1 v2
--             decide :: Ordering -> Ordering
--             decide EQ = compare hasA2 hasA1
--             decide other = other
--         in decide valueComp

-- data Tree = 

-- data DealerTree = Nil | DNode { 
--     dleft :: DealerTree,
--     dright :: DealerTree,
--     dhandValue :: Int,
--     dcontainsAce :: Bool}

-- data PlayerTree = Nil | PNode {}

-- exploration parameter c for MCTS
explorationParam :: Float
explorationParam = sqrt 2

-- data HandInfo = NoHandInfo | HandInfo {containsAce::Bool}

data MCInfo = NoMCInfo | MCInfo {
    wins :: Float, numSimsThisNode :: Int, numSimsSubtreeTotal :: Int } deriving (Eq)

-- data NodeValue = Val Int | Act PureAction
type NodeID = String
data NodeInfo =  NodeInfo {
    nid :: NodeID, mcInfo :: MCInfo} deriving (Eq)

data Children = Children {left :: Tree, right :: Tree} deriving (Eq)

data Tree = None | Node {
    children :: Children,
    nodeInfo :: NodeInfo
} deriving (Eq)


-- instance Functor

-- test0 :: Tree
-- test0 = Node (Children None None) NoNodeInfo
-- test1 :: Tree
-- test1 = Node (Children None None) NoNodeInfo

defaultTree = Node noChildren defaultNodeInfo

defaultNodeInfo = NodeInfo "" defaultMCInfo

defaultMCInfo = MCInfo 0 0 0

mctTest = defaultTree {
    children = Children cardNumTree2 None,
    nodeInfo = defaultNodeInfo {nid ="2"}
}

cardNumTree2 = defaultTree {children = Children None cardNumTree3,
    nodeInfo = defaultNodeInfo {nid="2"}}

cardNumTree3 = defaultTree {children = Children cardValTree None,
    nodeInfo = defaultNodeInfo {nid="3"} }

cardValTree = defaultTree {nodeInfo = defaultNodeInfo {nid="4"}}


noChildren :: Children
noChildren = Children None None

data HandInfo = HandInfo {
    hasAce :: Bool, -- TODO: change from hasAce to soft total
    hasPair :: Bool, -- TODO: change to canSplit
    val::Int}

instance Show HandInfo where
    show (HandInfo a p v) = show v ++ aceStr ++ pairStr
        where
            pairStr = if p then "P" else ""
            aceStr = if a then "A" else ""


data GameState = GState {
    curDealerCard :: Int,
    cardNum :: Int,
    curHand :: HandInfo, -- should be smth like "4" or "4A" if has Ace
    actionTaken :: Maybe PureAction,
    gameOutcome :: Maybe Outcome
}

-- exGState = GState 3 3 (HandInfo 4 False)

-- findState :: GameState -> Maybe Tree
-- -- TODO: use =<<[d n h]
-- findState (GState d n h) = do
--     -- l1 <- getLeftTree <$> findNodeAtLevel mct d
--     -- l1 <- findNodeAtLevel mct d

--     l1 <- getLeftTree <$> findNodeAtLevel mct d
--     -- if isNothing l1 then putStrLn "Nothing" else putStrLn "dealer found"
--     l2 <- getLeftTree <$> findNodeAtLevel l1 n
--     findNodeAtLevel l2 h
--     -- pure l2

-- testFindState = findState exGState

-- findOrCreate :: Tree -> a -> Maybe Tree
-- findOrCreate t item = 
--     if foundNode
--         then findNodeAtLevel t item
--         else 

-- findFromSubtree :: Tree -> Maybe Tree  -> a -> Maybe Tree
-- findFromSubtree prev Nothing i = Undefined
-- findFromSubtree _ (Just t) i = getLeftTree <$> findNodeAtLevel t i

--creates Node at the same level as the input tree node
-- createNodeAtLevel :: Tree -> a -> Maybe Tree
-- createNodeAtLevel None i = Just $ Node noChildren $ setId i
-- createNodeAtLevel p@(Node (Children l r) _) i = 
--     let
--         newNode = Node {children=setRightChild r, nodeInfo=setId i}
--         parentNode = p {children=Children l newNode}
--     in 
--         Just parentNode
-- creates a child node as the child of the input tree node
-- createNodeBelowLevel 

-- findNodeAtLevel :: Show a => Tree -> a -> Maybe Tree
-- findNodeAtLevel None _ = Nothing
-- findNodeAtLevel t item
--     -- if foundNode then Just t else findNodeAtLevel (getRightTree t) item
--     | found || isNothing retVal = Just t
--     | otherwise = retVal
--     where
--         found = foundNode t item
--         retVal = findNodeAtLevel (getRightTree t) item

getLeftTree :: Tree -> Tree
getLeftTree = left . children

getRightTree :: Tree -> Tree
getRightTree = right . children

foundNode :: Tree -> String -> Bool
foundNode None _ = False
foundNode t s = nid (nodeInfo t) == s

setLeftChild :: Tree -> Children
setLeftChild = flip Children None

setRightChild :: Tree -> Children
setRightChild = Children None

setId :: Show a => a -> NodeInfo
setId = (`NodeInfo` NoMCInfo) . show

-- testT = findNodeAtLevel cardNumTree2 3


-- SHOW INSTANCES --

-- instance Show HandInfo where
--     show NoHandInfo = "No Hand Info"
--     show (HandInfo a) =
--         "Hand Info: { Contains Ace: " ++ show a

instance Show MCInfo where
    show NoMCInfo = "No Monte Carlo-related info"
    show (MCInfo w numThisNode numSubtree) =
        "Monte Carlo related info: ["
        ++ " Wins: " ++ show w
        ++ " Number of simulations for this node: " ++ show numThisNode
        ++ " Total number of simulations for this subtree: " ++ show numSubtree
        ++ "]"

-- instance Show NodeValue where
--     show (Val n) = show n
--     show (Act a) = show a

instance Show NodeInfo where
    -- show NoNodeInfo = "No Node Info\n"
    show (NodeInfo v mc) =
        "Node Info: {\n"
            ++ "\tValue: " ++ show v ++ "\n"
            ++ "\t" ++ show mc ++ "\n"
            -- ++ "\t" ++ show h ++ "\n"
        ++ "\t}\n"

instance Show Children where
    show (Children l r) = "LEFT: " ++ show l ++ "RIGHT: " ++ show r

instance Show Tree where
    show None = "None\n"
    show (Node c n) = "(\n" ++ show n ++ show c ++ ")\n"

instance Show GameState where
    show (GState d cNum hInfo act gameOutcome) =
        "Dealer: "++ show d
        ++ " Card num: " ++ show cNum
        ++ " Hand Info: " ++ show hInfo
        ++ safeShow act " Action: "
        ++ safeShow gameOutcome " Outcome: "
        where
            safeShow :: Show a => Maybe a -> String -> String
            safeShow (Just a) str = str ++ show a
            safeShow Nothing _ = ""
-- test2 = Node (Children test0 test1) (NodeInfo 4 5 6 NoHandInfo)


-- find :: Tree 

-- data DealerTree = Nil | Node 

-- data PlayerTree = PTree {

-- }


-- n1 = Node 12 False 
-- n2 = Node 11 False

-- n3 = Node 12 True 
-- n4 = Node 12 False


-- data PlayerNode = PNode {
--     playerHandVal :: Int,
--     playerHasAce :: Bool
-- }



-- Tree node
-- data Node = Node {
--  Rank :: Int,
--     curHand :: [Int],
--     handHasAce :: Bool,
--     dealerHasAce :: Bool
-- }

-- data MCT = 

singleton :: a -> [a]
singleton i = [i]


----------------------------------------------------
-------------------- SIMULATION --------------------
----------------------------------------------------

-- plays a single round till completion, since the hand is not carried over to the next round it's not
-- necessary to play out a whole game

--IDEA: call the same func but only pass in playerMem "$" and the action
-- look at Play.hs, to do this we only need to modify previous trick
-- use dealer up card + random card for the dealer's hand
-- testPlayer = Player "11" 

threeDecks :: Stock
threeDecks = join $ replicate 3 sortedDeck

-- take in a seed and generate a random deck (random length + random cards)
randomizeDeck :: RandState -> Int -> Stock
randomizeDeck seed maxLen = map snd $ take len sortedRandDeck
    where
        next = nextRand seed
        len = maxMod 10 next maxLen
        indices = foldr (\_ (prevSeed, previs) -> (nextRand prevSeed, prevSeed : previs)) (nextRand next, []) [1..length threeDecks]
        sortedRandDeck = sortOn fst $ zip (snd indices) threeDecks


-- simulate :: 

-- given starting points, determine if loss or win or draw
-- determineSimOutcome :: Points -> IO (Either GameError (HandResult, Trick, Card, Stock)) -> Outcome
-- determineSimOutcome startPts (IO ())




testTrickSim = [PlayNode (Play "99" 0 500 (Bid 100) "?40BP" []) Nil]

-- testdef :: (HandResult, Trick, Card, Stock)
-- testdef = ((HandResult [] []), [], (Card Spade Two), [])
-- test4 n = unsafePerformIO $ runEitherIO $ runSimulation (randomizeDeck n) [testTrickSim]
-- testStr4 = fromRight [] <$> (get2nd <$$> test4 1)

-- getMem :: PlayerNode -> String
-- getMem Play 

get2nd (_,trick, _, _) = trick

tstDeck = [Card Spade Two, Card Diamond Three, Card Heart Five, Card Diamond Two, Card Heart Five, Card Spade Ace,
    Card Diamond Two, Card Spade Jack, Card Diamond Queen]

-- testSim2 = runEitherIO $ runSimulation tstDeck [testTrickSim]

testrMem1 = RoundM [PHit] [2, 4] [Two, Four] 100 69 2 Nothing
testrMem2 = RoundM [PSplit, PHit] [2, 4] [Two, Four] 100 69 2 Nothing

-- testSimSetup = setupSimulation testrMem1 PStand
testSimOnce = simulateOnce testrMem1 PStand 241241324
testSimMult = simulateNTimes 10 testrMem1 PStand 1212

-- constant, simulation always starts with 1000 pts
simStartPts :: Int
simStartPts = 1000


-- simulationToMemory :: [SimInfo] -> 

simulateNTimes :: Int -> RoundMem -> PureAction -> RandState -> Maybe [SimInfo]
simulateNTimes n rMem simAct seed =
    sequence $ snd $ foldr (\_ (prevSeed, simInfos) -> (nextRand prevSeed, simulation prevSeed : simInfos)) (seed, []) [1..n]
    where
        simulation = simulateOnce rMem simAct -- partially applied, only requires seed

simulateOnce :: RoundMem -> PureAction -> RandState -> Maybe SimInfo
simulateOnce rMem actionToSimulate seed = do
    simTrick <- generateSimTrick rMem actionToSimulate
    simDeck <- recreateDeck rMem seed
    getInfoFromSimulation $ runSimulation simDeck simTrick

generateSimTrick :: RoundMem -> PureAction -> Maybe Trick
generateSimTrick rMem@RoundM {roundBid, roundActions} act
    | performedSplit rMem = Nothing
    | otherwise =  Just simTrick
    where
        simActions = fold $ show <$> roundActions ++ [act]
        simMem = "?" ++ show roundBid ++ "B" ++ simActions
        simTrick = [PlayNode (Play "99" 0 500 (Bid 100) simMem []) Nil]


testRState = RoundM [] [2,2] [Two, Two] 1000 40 8 Nothing
testRecreateDeck = recreateDeck testRState 1231200


recreateDeck :: RoundMem -> RandState -> Maybe Stock
recreateDeck RoundM {roundDealer, roundRanks} seed = do
    dealerUpCard <- generateCardFromVal roundDealer
    dealerDownCard <- generateCardFromVal $ maxMod 1 seed 12
    let playerCards = Card Spade <$> roundRanks -- again don't care about suit
    -- (dealerUpCard : playerUpCards) <- sequence $ generateCardFromVal <$> (roundDealer : roundHand)
        randomizedRestDeck = randomizeDeck seed (length playerCards + 2)
    pure $ dealerUpCard : dealerDownCard : playerCards ++ randomizedRestDeck
    -- ranks <- sequence $ generateRankFromVal <$> (roundDealer : roundHand)
    -- let (dealerUpCard : playerUpCards) = generateCardFromRank <$> ranks
    -- let randDealerCard = 
    -- let deck = dealerUpCard : (generateCardFromRank $ generateRankFromVal $ mod seed 12)


valToRank :: [(Int, Rank)]
valToRank = [(1, Ace), (11, Ace)] ++ zip [2..10] [Two .. Ten]


generateCardFromVal :: Int -> Maybe Card
generateCardFromVal rank = generateCardFromRank <$> generateRankFromVal rank

-- for the purposes of simulation, the suit is unimportant so convert all to spades
generateCardFromRank :: Rank -> Card
generateCardFromRank = Card Spade

generateRankFromVal :: Int -> Maybe Rank
generateRankFromVal num = snd <$> find ((==num) . fst) valToRank

-- data SimInfo = SimInfo {
--     simMemory :: Memory,
--     simResult :: Outcome
-- } deriving (Show)

type SimInfo = RoundMem

-- testSimulation = runSimulation tstDeck testTrickSim

-- testGetInfoFromSim = getInfoFromSimulation (runSimulation tstDeck testTrickSim) testRState

getInfoFromSimulation :: Either GameError (HandResult, Trick, Card, Stock) -> Maybe SimInfo
getInfoFromSimulation (Left _) = Nothing
getInfoFromSimulation (Right (res, trick, _, _)) = do
    simMemory <- head <$> getSimMemory trick
    let simOutcome = extractSimOutcome res
        updatedSimMemory = simMemory {roundOutcome= Just simOutcome} -- update the roundOutcome with the result of the simulation
    pure updatedSimMemory

getSimMemory :: Trick -> Maybe Memory
getSimMemory (dealer:players) =
    -- trace ("PLAYER TRICK: " ++ show playerTrick ++ " \n" ++ 
    --         "DEALER TRICK: " ++ show dealer)
    extractSimMemory playerTrick
    where
        playerTrick = last players
getSimMemory _ = Nothing

extractSimMemory :: PlayNode -> Maybe Memory
extractSimMemory node = do
    memStr <- memory <$> nodeValue node
    index <- elemIndex '/' memStr
    let useableMem =
            -- trace ("MEMORY STRING: " ++ memStr ++ " INDEX: " ++ show index)
            drop index memStr
    safeExtractParseResult $ parse strictParseAll useableMem

safeExtractParseResult :: ParseResult a -> Maybe a
safeExtractParseResult (Result rest a) = Just a
safeExtractParseResult (Error _) = Nothing

extractSimOutcome :: HandResult ->Outcome
extractSimOutcome HandResult{handPoints} =
    -- trace ("HANDRESULT: " ++ show handPoints )
    outcome simStartPts $ getPoints $ head handPoints

runSimulation :: Stock -> Trick -> Either GameError (HandResult, Trick, Card, Stock)
-- Need to extract from the IO context because we are not doing any IO
runSimulation deck trick = unsafePerformIO $ runEitherIO $ playHand deck trick simGamePoints
    where
        simPlayer = Player "99" playCard
        simGamePoints = [GamePoints simPlayer $ Rich simStartPts]





testPlayer = Player "2" playCard

testHand = [Card Spade Jack, Card Heart Jack]

testPlayerHand = PlayerHand testPlayer testHand

testDealerHand = [Card Heart Ace, Card Spade Ten]

testDeck = [
    -- dealer's first two cards
    Card Diamond Queen,
    Card Spade Nine,
    -- player's two cards
    Card Spade Jack,
    Card Heart Jack
    ]

testGameScore = [GamePoints testPlayer (Rich 1000)]

-- testPrevTrick :: Trick
-- testPrevTrick = []

-- testCurTrick :: Trick
-- testCurTrick = [testPnode]

-- testSim = runEitherIO $ playTricks [testPlayerHand] testDeck testDealerHand testPrevTrick [] testGameScore

-- Returns EitherIO GameError (HandResult, Trick, Card)
-- the thing we want is in HandResult
-- For bidding it takes the bid from playCard which automatically bids 100
testSim = playHand testDeck [] testGameScore

-- testPlayerPoints = gamePointsToPlayerPoints  <$> testGameScore

-- testInfo = combineWith (playToInfo . nodeValue) ([]) ([])

-- testTimeCall :: EitherIO GameError (Action, String)
-- testTimeCall = timeCall 
--     (playCard (Just $ Card Heart Ace) testPlayerPoints [] "2" Nothing)
--     "2"
--     testHand

-- testDo = do
--     (choice', raw) <- testTimeCall
--     updated <- liftEither $ checkMemory raw "2"
--     choice  <- liftEither $ validPlay choice' 
--         testPlayerHand testPlayerPoints (Just $ Card Heart Ace) []
--     pure updated

-- testUpdated = liftEither $ checkMemory "" "2"

-- testPlay = Play "2" 0 200 (Bid 300) "" testHand

-- testPnode = PlayNode testPlay Nil

-- testPlayerNode = fromMaybe Nil $ find
--             (\(PlayNode p _) -> "2" == getId p && 0 == secondId p)
--             testCurTrick

-- testDo2 = do
--     te <- liftIO
--         $ parseAction Hit testHand testDeck (getPoints testPnode)
--     pure te

-- mod with a minimum value
type LowerBound = Int
type UpperBound = Int

maxMod :: LowerBound -> Int -> UpperBound -> Int
maxMod minVal val maxVal = max minVal $ mod val maxVal


--Reference: Week 10 Workshop exercises
type RandState = Int

nextRand :: RandState -> RandState
nextRand prevSeed = (a*prevSeed + c) `mod` m
  where -- Parameters for linear congruential RNG.
    a = 1103515245
    c = 12345
    m = intPower 2 31

-- helper for ^ so it doesnt have to infer type
intPower :: Int -> Int -> Int
intPower base pow = base ^ pow
