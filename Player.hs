

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


-- | This function is called once it's your turn, and keeps getting called until your turn ends.

-- for convenience
type CompleteAction = (Action, String)

-- default action (if error occurs)
defaultAction :: CompleteAction
defaultAction = (Stand, "")

defaultBid :: Int
defaultBid = minBid

playCard :: PlayFunc
playCard Nothing pts _ pid mem h =
    trace("Bidding round" ++ "\n" ++ "Memory: " ++ fromMaybe "" mem)
    evalBid mem pts pid h
playCard dealer pts info pid (Just mem) h =
    trace(
        "-----Player----- " ++ pid ++"\n"++
        "Points: "++ show pts ++ "\n" ++
        "Memory: " ++ mem
    )
    (Stand, mem ++
        show (rankToVal $ getRank (fromMaybe (Card Spade Ace) dealer)) ++
        conc (show .rankToVal . getRank <$> h) ++
        "S")
    where
        conc  = concat
playCard _ _ _ _ _ _ = (Stand, "")
-- playCard dealer pts info pid mem h = playerAction mem


evalBid :: Maybe String -> [PlayerPoints] -> PlayerId -> Hand -> CompleteAction
-- TODO: GET AVG. STRENGTH OF THE HAND (COUNT CARDS MAYBE)
evalBid mem pts pid h = (Bid defaultBid, if isNothing mem then retMem else fromMaybe "" mem ++ retMem)
    where
        retMem = storeStartOfRound mem pts pid h
        -- retMem = "Bid"

rState = RoundM [] [11, 1] 0 200 2
rState2 = RoundM [PHit] [11, 1, 3] 0 200 4
rState3 = RoundM [] [2,2] 0 200 2
rState4 = RoundM [] [2,2] 0 200 11
rState5 = RoundM [PHit] [2,2] 0 200 11
rState6 = RoundM [PDoubleDown] [2,2] 0 200 11
rState7 = RoundM [PDoubleDown, PHit] [2,2] 0 200 11
rState8 = RoundM [PDoubleDown, PHit, PStand] [2,2] 0 200 11


-- !!!BIG TODO: PLEASE FIND A BETTER WAY TO DO THIS
legalActions :: RoundMem -> [PureAction]
legalActions (RoundM acts hand _ bid dealer)=finalAction
    where
        defaultActions = [PHit, PStand]
        includeInsurance = [PInsurance | dealer == 11 && null acts]
        includeSplit = [PSplit | isPair hand]
        includeDoubleDown = [PDoubleDown | length hand == 2]
        finalAction = if elem PDoubleDown $ take 2 $ reverse acts then if (==PHit) $ head $ take 1 $ reverse acts
         then [PStand] else [PHit]
            else defaultActions ++ includeInsurance ++ includeSplit ++ includeDoubleDown


playerAction :: Maybe String -> CompleteAction
playerAction Nothing = (Hit, "") -- memory should never be nothing in non-bid rounds
playerAction (Just s) = case parse parseAll s of
    Error _ -> case action of
        Just act -> (act, "")
        Nothing -> defaultAction -- default to stand if error (should not be called)
        where
            simAction = parse parseSim s
            action = simActionToAction simAction
    Result rest p ->
        (Hit, "") -- CHANGE WITH THE NON-SIMULATION ACTION
        where
            thisRound : prevRounds = p
            legalActionsThisRound = legalActions thisRound
            -- zip

chooseRandomAction :: RandState -> [PureAction] -> PureAction
chooseRandomAction seed pureActions = snd $ head $ sortOn fst $ zip (generateRandSeq $ nextRand seed) pureActions

testChooseRand s = sortOn fst $ zip (generateRandSeq $ nextRand s) [PHit, PStand, PDoubleDown, PSplit]

generateRandSeq :: RandState -> [RandState]
generateRandSeq = iterate nextRand


simActionToAction :: ParseResult SimAction -> Maybe Action
simActionToAction (Error _) = Nothing
simActionToAction (Result _ (SimAction b a)) = pureActionToAction a b

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

testActionMapping = snd <$> find ((==PStand) . fst) (pureActionMapping 20)

pureActionToAction :: PureAction -> Int -> Maybe Action
pureActionToAction pAction bid = snd <$> pair
    where
        pair = find ((==pAction) . fst) (pureActionMapping bid)

    -- let
    --     pair = find ((==pAction) . fst) (pureActionMapping bid)
    --     getAction :: Maybe (PureAction)

actionMapping :: [(Char, PureAction)]
actionMapping = [('S', PStand), ('H', PHit), ('D', PDoubleDown), ('P', PSplit), ('I', PInsurance)]

type History a = [[a]]

data RoundMem = RoundM {
    roundActions :: [PureAction],
    roundHand :: [Int],
    roundStartPts :: Points,
    roundBid :: Int,
    roundDealer :: Int
}

instance Show RoundMem where
    show (RoundM a h p b d) =
        "Actions this round: " ++ show a ++
        "  Hands this round: "++ show h ++
        "  Points at start of round: " ++ show p ++
        "  Bids this round: " ++ show b ++
        "  Dealer's first card this round: " ++ show d

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
    (<>) (RoundM a1 h1 p1 b1 d1) (RoundM a2 h2 p2 b2 d2) =
        RoundM (a1++a2) (h1++h2) (max p1 p2) (max b1 b2) (max d1 d2)

instance Monoid RoundMem where
    mempty = RoundM [] [] 0 0 0

type Memory = [RoundMem]

parseMem :: Parser RoundMem
parseMem = P (\i -> case parse parseMemAction i of
    Result rest res -> Result rest $ memInitializeAction res
    Error _ -> case parse parseHand i of
        Result rest res -> Result rest $ memInitializeHand res
        Error e -> Error e
    )

parseAll :: Parser Memory
-- TODO: MAKE THIS FOLD AN MCONCAT INSTEAD, I.E. MOVE SHALLOWMEMCONCAT TO MCONCAT
parseAll = do
    l <- list1 (is '/' >> parseMoreMem) -- has to succeed at least once
    pure $ reverse l -- latest round is at the head
    -- pure(tempMeml)
    -- (foldl (\(M act1 h1) (M act2 h2) -> M ([act2, act1]) [h2, h1]) (mempty)) =<< (list (is '/' >> parseMoreMem))


memInitializeAction :: PureAction -> RoundMem
memInitializeAction act = mempty {roundActions=[act]}

memInitializeHand :: Int -> RoundMem
memInitializeHand h = mempty {roundHand=[h]}

-- memInitializeDealer :: Int -> RoundMem
-- memInitializeDealer d = mempty {roundDealer=d}

-- memInitializeBid :: Int -> RoundMem
-- memInitializeBid b = mempty {roundBid=b}

-- memInitializePts :: Points -> RoundMem
-- memInitializePts pts = mempty {roundStartPts=pts}

memInitializeRound :: Points -> Int -> Int -> RoundMem
memInitializeRound = RoundM [] []

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
    dealer <- parseHand
    tm <- parseMemList parseMem
    let (Just updatedTm) = replaceLatestHand tm <$> evaluateAce (roundHand tm)
    let ret = updatedTm <> memInitializeRound pts bid dealer
    pure ret

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
handMapping = [('A', 11), ('T', 10)]

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

parseHand :: Parser Int
parseHand = parseSpecialHand ||| parseDigit

parseSpecialHand :: Parser Int
parseSpecialHand = foldr (|||) (head handVal) handVal


-- map isAction to actionMapping where character is first element of the tuple and
-- PureAction is the second element
pureAction :: [Parser PureAction]
pureAction = convertTypeChar actionMapping

handVal :: [Parser Int]
handVal = convertTypeChar handMapping

convertTypeChar :: [(Char, a)] -> [Parser a]
convertTypeChar = mapping charConverter

mapping :: Converter a b -> [(a, b)] -> [Parser b]
mapping f l = uncurry f <$> l

type Converter a b = a -> b -> Parser b
-- type CharConverter a = Char -> a -> Parser a

-- if it is that character convert it to a Parser of the given PureAction
-- isAction :: CharConverter PureAction
-- isAction c act = is c >> pure act

-- parseHand = ace ||| ten ||| parseDigit


parseDigit :: Parser Int
-- TODO: dont allow 0 by using elem and \\ (list difference)
-- <&> is the flipped version of <$>
-- takes context then function
parseDigit = satisfy isDigit <&> digitToInt

-- isSpecialDigit :: CharConverter Int
-- isSpecialDigit = charConverter

charConverter :: Converter Char a
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

data SimAction = SimAction {
    simBid :: Int,
    simAction :: PureAction
}

parseSim :: Parser SimAction
parseSim = do
    simBid <- parseNumAndSeparator 'B'
    SimAction simBid <$> parseMemAction

-- playerAction :: Maybe String -> CompleteAction
-- playerAction Nothing = (Hit, "") -- memory should never be nothing in non-bid rounds
-- playerAction (Just s) = case parse parseAll s of
--     Error _ -> case action of
--         Just act -> (act, "")
--         Nothing -> defaultAction -- default to stand if error (should not be called)
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

getPlayerMem :: Input -> Memory
getPlayerMem = extractFromParseResult [] . parse parseAll

-- takes a ParseResult and a default value to fall back on, 
-- gets the result or returns the default value
extractFromParseResult :: a -> ParseResult a -> a
extractFromParseResult def (Error _) = def
extractFromParseResult _ (Result _ result) = result

-- to store outcome of round (win/loss/draw)
data Outcome = W | L | D deriving(Show)

-- BIG TODO!! REFACTOR
storeStartOfRound :: Maybe String -> [PlayerPoints] -> PlayerId -> Hand -> String
storeStartOfRound (Just mem) allPts pId pHand=
    let
        memorySoFar = getPlayerMem mem
        prevRoundStartPts =  roundStartPts $ mostRecentRound memorySoFar
        curRoundStartPts = getPlayerPoints allPts pId
        prevOutcome = outcome prevRoundStartPts curRoundStartPts
    in
        trace("Memory in store start of round: " ++ mem)
        show prevOutcome ++ "/" ++ show curRoundStartPts
        ++ "$" ++ show defaultBid  -- DONT FORGET TO CHANGE BID TO TAKE INTO ACCOUNT MCTS
        ++ "B"
storeStartOfRound Nothing allPts pId pHand = let
    curRoundStartPts = getPlayerPoints allPts pId
    in
        trace ("allPts: " ++ show allPts)
        "/" ++ show curRoundStartPts ++ "$" ++ show defaultBid
        ++ "B"

outcome :: Points -> Points -> Outcome
outcome prevP curP
    | curP > prevP = W
    | curP == prevP = D
    | otherwise = L

storeAction :: String -> Action -> String
storeAction mem act = mem ++ show act

storeUpdatedHand :: String -> Hand -> String
storeUpdatedHand mem h =
    let
        memorySoFar = getPlayerMem mem
        handInMem = roundHand $ mostRecentRound memorySoFar
        addCard = length h == length handInMem
        (mostRecentCard: _) = h
    in
        if addCard
            then mem ++ show (rankToValDeck (sum handInMem) (getRank mostRecentCard))
            else mem

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


-- to convert all prev round mem to game states, doesnt do it for current rount see getCurrentGameState
roundMemToGameState :: RoundMem -> Maybe [GameState] -> [Int] ->Maybe [GameState]
roundMemToGameState _ Nothing _= Nothing
-- processed all the actions and all cards
roundMemToGameState (RoundM [] _ _ _ _) gStateSoFar _= gStateSoFar
roundMemToGameState rMem _ []= roundMemToGameState rMem Nothing (take 2 $ roundHand rMem)
roundMemToGameState rMem@(RoundM acts hand _ _ dealerVal) gStateSoFar handForAction=
    roundMemToGameState rMem{roundActions=rest} updatedGStateSoFar newHandForAction
    where
        (action : rest) = acts
        numCards = length handForAction
        handInfo = HandInfo (isSoft handForAction) (isPair handForAction) (sum handForAction)
        cardsObtained = numCardObtainedFromAction action
        newHandForAction = take cardsObtained hand
        newGameState = GState dealerVal numCards handInfo (Just action)
        updatedGStateSoFar :: Maybe [GameState]
        updatedGStateSoFar = do
            state <- gStateSoFar
            pure $ newGameState : state

-- !!!! REMEMBER TO ADD IN SPLIT LATER !!!
numCardObtainedFromAction :: PureAction -> Int
numCardObtainedFromAction PHit = 1
numCardObtainedFromAction _ = 0

testGState = getCurrentGameState (Card Spade Two) [Card Heart Four, Card Diamond Ace]

testFind = findSubTree testGState mct

-- convert state for the current round to GameState
getCurrentGameState :: Card -> Hand -> GameState
getCurrentGameState dealerCard pHand = GState dCardVal handLen handInfo Nothing
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

isPair :: [Int] -> Bool
isPair l = length l == 2 && c1 == c2
    where
        [c1, c2] = l



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


moveLeft :: Zipper -> Maybe Zipper
moveLeft (None, _) = Nothing -- fail for empty tree
moveLeft (Node (Children l r) info, bs) = Just (l, LeftCrumb info r :bs)

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
    returnToRoot $ modifyNode incrementMCWins node

incrementMCWins :: MCInfo -> MCInfo
incrementMCWins NoMCInfo = NoMCInfo
incrementMCWins info = info {wins = wins info + 1}

attachLeft :: Tree -> Zipper -> Maybe Zipper
attachLeft t (Node (Children None r) info, bs) = Just (Node (Children t r) info, bs)
attachLeft _ _ = Nothing

attachRight :: Tree -> Zipper -> Maybe Zipper
attachRight t (Node (Children l None) info, bs) = Just (Node (Children l t) info, bs)
attachRight _ _ = Nothing


findSubTree :: GameState -> Tree -> Maybe Zipper
findSubTree (GState d num hInfo Nothing) t = do
    let startZ = (t, [])
    -- DONT FORGET TO UNCOMMENT ^
    -- let startZ = (shorterTestMCT, [])
    dealerZ <- findAtLevel (show d) startZ
    cardNumNode <- findAtLevel (show num) =<< moveLeft dealerZ
    findAtLevel (show hInfo) =<< moveLeft cardNumNode
findSubTree (GState d num hInfo (Just act)) t = do
    let startZ = (t, [])
    -- DONT FORGET TO UNCOMMENT ^
    -- let startZ = (shorterTestMCT, [])
    dealerZ <- findAtLevel (show d) startZ
    cardNumNode <- findAtLevel (show num) =<< moveLeft dealerZ
    cardValNode <- findAtLevel (show hInfo) =<< moveLeft cardNumNode
    findAtLevel (show act) =<< moveLeft cardValNode


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

t = do
    res <- testaf1
    modifyNodeAndParents incrementMCWins (show PStand) res

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
findAndModifySome f state@(GState _ _ _ (Just act)) (Just (t, _)) = modifyNodeAndParents f (show act) =<< findSubTree state t
findAndModifySome f state@(GState _ _ _ Nothing) (Just (t, _)) = Nothing
findAndModifySome _ _ Nothing = Nothing

-- testFM1 = extractRes $ findAndModifySome incrementMCWins $ GState 11 4 (HandInfo False False 20)

modifyOnData :: (MCInfo -> MCInfo) -> [GameState] -> Maybe Zipper
modifyOnData f = foldr (findAndModifySome f) (Just (shorterTestMCT, [])) -- REMEMBER TO CHANGE TO MCT

-- testmODp1 = findAndModify 

testmOD = modifyOnData incrementMCWins [GState 2 6 (HandInfo False False 9) (Just PStand),GState 2 5 (HandInfo False False 10) (Just PHit)]

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

testGetAction = getActionOnLargestUCB (testT1, []) tstValid

getActionOnLargestUCB :: Zipper -> [NodeID] -> Maybe Zipper
-- initialize current t as the largest and move right
getActionOnLargestUCB z@(t,_) validActs
    | nid (nodeInfo t) `elem` validActs = getActionOnLargestUCBAux (moveRight z) validActs (Just z)
    | otherwise = getActionOnLargestUCBAux (moveRight z) validActs Nothing

-- params: current tree, valid nodes, best zipper
getActionOnLargestUCBAux :: Maybe Zipper -> [NodeID] -> Maybe Zipper -> Maybe Zipper
getActionOnLargestUCBAux Nothing _ Nothing = Nothing
getActionOnLargestUCBAux Nothing _ bestZ = bestZ
getActionOnLargestUCBAux (Just curZ@(curT, )) validActs bestZ
    | nid (nodeInfo curT) `elem` validActs = getActionOnLargestUCBAux (moveRight curZ) validActs updatedBest
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


-- calc UCB statistic
calcUCB :: Tree -> Maybe Statistic
calcUCB t = do
    let (MCInfo wins sims simsThisLevel) = mcInfo $ nodeInfo t
    exploitation <- safeDivInts wins sims
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
    wins :: Int, numSimsThisNode :: Int, numSimsSubtreeTotal :: Int }

-- data NodeValue = Val Int | Act PureAction
type NodeID = String
data NodeInfo = NoNodeInfo | NodeInfo {
    nid :: NodeID, mcInfo :: MCInfo}

data Children = Children {left :: Tree, right :: Tree}

data Tree = None | Node {
    children :: Children,
    nodeInfo :: NodeInfo
}


-- instance Functor

test0 :: Tree
test0 = Node (Children None None) NoNodeInfo
test1 :: Tree
test1 = Node (Children None None) NoNodeInfo

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
    actionTaken :: Maybe PureAction
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
    show NoNodeInfo = "No Node Info\n"
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
    show (GState d cNum hInfo act) =
        "Dealer: "++ show d
        ++ "Card num: " ++ show cNum
        ++ "Hand Info: " ++ show hInfo
        ++ actStr act
        where
            actStr :: Maybe PureAction -> String
            actStr (Just a) = show a
            actStr Nothing = ""
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
--     dealerUpCard :: Int,
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

--IDEA: call the same func but only pass in memory "$" and the action
-- look at Play.hs, to do this we only need to modify previous trick
-- use dealer up card + random card for the dealer's hand
-- testPlayer = Player "11" 

threeDecks :: Stock
threeDecks = join $ replicate 3 sortedDeck

-- take in a seed and generate a random deck (random length + random cards)
randomizeDeck :: RandState -> Stock
randomizeDeck seed = map snd $ take len sortedRandDeck
    where
        next = nextRand seed
        len = mod next (length threeDecks)
        indices = foldr (\_ (prevSeed, previs) -> (nextRand prevSeed, prevSeed : previs)) (nextRand next, []) [1..length threeDecks]
        sortedRandDeck = sortOn fst $ zip (snd indices) threeDecks


test4 n = runEitherIO $ runSimulation (randomizeDeck n)

runSimulation :: Stock -> EitherIO GameError (HandResult, Trick, Card, Stock)
runSimulation deck = playHand deck [] simGamePoints
    where
        simPlayer = Player "99" playCard
        simGamePoints = [GamePoints simPlayer $ Rich 1000]





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


--Reference: Week 10 Workshop exercises
type RandState = Int

nextRand :: RandState -> RandState
nextRand prevSeed = (a*prevSeed + c) `mod` m
  where -- Parameters for linear congruential RNG.
    a = 1103515245
    c = 12345
    m = 2^31


