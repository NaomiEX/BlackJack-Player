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


-- | This function is called once it's your turn, and keeps getting called until your turn ends.

-- for convenience
type CompleteAction = (Action, String)




playCard :: PlayFunc
playCard Nothing _ _ _ _ = evalBid
playCard a b c d e = undefined


evalBid :: Hand -> CompleteAction
-- TODO: GET AVG. STRENGTH OF THE HAND (COUNT CARDS MAYBE)
evalBid h = (Bid 100, "")

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

instance Show PureAction where
    show act = case act of
        PHit -> "H"
        PStand -> "S"
        PDoubleDown -> "D"
        PSplit -> "P"
        PInsurance -> "I"

actionMapping :: [(Char, PureAction)]
actionMapping = [('S', PStand), ('H', PHit), ('D', PDoubleDown), ('P', PSplit), ('I', PInsurance)]


data TempMem = M {
    action :: [[PureAction]],
    hand :: [[Int]],
    dealer :: [[Int]]
}

instance Show TempMem where
    show (M a h d) = "Actions so far: " ++ show a ++ "  Hands so far: "++ show h ++ "  Dealer's first card so far: " ++ show d

instance Semigroup TempMem where
    (<>) (M a1 h1 d1) (M a2 h2 d2) = M (concatInnerList a1 a2) (concatInnerList h1 h2) (concatInnerList d1 d2)

instance Monoid TempMem where
    mempty = M [[]] [[]] [[]]

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

parseMem :: Parser TempMem
parseMem = P (\i -> case parse parseAction i of
    Result rest res -> Result rest $ memInitializeAction res
    Error _ -> case parse parseHand i of
        Result rest res -> Result rest $ memInitializeHand res
        Error e -> Error e
    )

memInitializeAction :: PureAction -> TempMem
memInitializeAction act = M [[act]] [[]] [[]]

memInitializeHand :: Int -> TempMem
memInitializeHand h = M [[]] [[h]] [[]]

memInitializeDealer :: Int -> TempMem
memInitializeDealer d = M [[]] [[]] [[d]]

-- connects pureAction with |||, i.e. is Action 'S' PStand ||| is Action 'H' PHit ||| ...
parseAction :: Parser PureAction
parseAction = foldr (|||) (head pureAction) pureAction

parseHand :: Parser Int
parseHand = parseSpecialHand ||| parseDigit

parseSpecialHand :: Parser Int
parseSpecialHand = foldr (|||) (head handVal) handVal


-- map isAction to actionMapping where character is first element of the tuple and
-- PureAction is the second element
pureAction :: [Parser PureAction]
pureAction = convertType actionMapping

handVal :: [Parser Int]
handVal = convertType handMapping

convertType :: [(Char, a)] -> [Parser a]
convertType = mapping charConverter

mapping :: CharConverter a -> [(Char, a)] -> [Parser a]
mapping f l = uncurry f <$> l

type CharConverter a = Char -> a -> Parser a

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

charConverter :: CharConverter a
charConverter c r = is c >> pure r

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- character
    if f c then pure c else unexpectedCharParser c

-- parses PureActions + Int does not deal with separators
parseMoreMem :: Parser TempMem
parseMoreMem = do
    dealer <- parseHand
    tm <- tempMemList parseMem
    let (Just updatedTm) = (replaceLatestHand tm) <$> (evaluateAce $ head $ hand tm)
    let ret = updatedTm <> memInitializeDealer dealer
    pure ret

replaceLatestHand :: TempMem -> [Int] -> TempMem
replaceLatestHand (M a h d) newHand = M a (newHand : tail h) d

tempMemList :: Parser TempMem  -> Parser TempMem
tempMemList p1 = tempMemList1 p1 ||| pure mempty

tempMemList1 :: Parser TempMem -> Parser TempMem
tempMemList1 p = do
    p' <- p
    p'' <- tempMemList p
    pure (p' <> p'')

shallowMemConcat :: TempMem -> TempMem -> TempMem
shallowMemConcat (M act1 h1 d1) (M act2 h2 d2) = M (act2++act1) (h2++h1) (d1 ++ d2)

parseAll :: Parser TempMem
-- TODO: MAKE THIS FOLD AN MCONCAT INSTEAD, I.E. MOVE SHALLOWMEMCONCAT TO MCONCAT
parseAll = do
    tempMeml <- list (is '/' >> parseMoreMem)
    pure (foldl1 shallowMemConcat tempMeml)
    -- pure(tempMeml)
    -- (foldl (\(M act1 h1) (M act2 h2) -> M ([act2, act1]) [h2, h1]) (mempty)) =<< (list (is '/' >> parseMoreMem))

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


---------------------------------------------------
-------------------- MCTS TREE --------------------
---------------------------------------------------

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
explorationParam :: Double
explorationParam = sqrt 2

-- data HandInfo = NoHandInfo | HandInfo {containsAce::Bool}

data MCInfo = NoMCInfo | MCInfo {
    wins :: Int, numSimsThisNode :: Int, numSimsSubtreeTotal :: Int }

-- data NodeValue = Val Int | Act PureAction

data NodeInfo = NoNodeInfo | NodeInfo {
    nid :: String, mcInfo :: MCInfo}

data Children = Children {left :: Tree, right :: Tree}

data Tree = None | Node {
    children :: Children,
    nodeInfo :: NodeInfo
}

test0 :: Tree
test0 = Node (Children None None) NoNodeInfo
test1 :: Tree
test1 = Node (Children None None) NoNodeInfo

defaultTree = Node noChildren defaultNodeInfo

defaultNodeInfo = NodeInfo "" defaultMCInfo

defaultMCInfo = MCInfo 0 0 0

mct = defaultTree {
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

data HandInfo = HandInfo {val :: Int, hasAce :: Bool}

instance Show HandInfo where
    show (HandInfo v a) = show v ++ aceStr
        where
            aceStr = if a then "A" else ""

data GameState = GState {
    curDealerCard :: Int,
    cardNum :: Int,
    curHand :: HandInfo -- should be smth like "4" or "4A" if has Ace
}

exGState = GState 2 3 (HandInfo 4 False)

findState :: GameState -> Maybe Tree
findState (GState d n h) = do 
    l1 <- getLeftTree <$> findNodeAtLevel mct d
    l2 <- getLeftTree <$> findNodeAtLevel l1 n
    findNodeAtLevel l2 h
    -- pure l2

testFindState = findState exGState

findNodeAtLevel :: Show a => Tree -> a -> Maybe Tree
findNodeAtLevel None _ = Nothing
findNodeAtLevel t item = if foundNode then Just t else findNodeAtLevel (getRightTree t) item
    where
        foundNode = show item == nid (nodeInfo t)

getLeftTree :: Tree -> Tree
getLeftTree = left . children

getRightTree :: Tree -> Tree
getRightTree = right . children

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