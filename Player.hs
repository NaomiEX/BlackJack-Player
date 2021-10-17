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
    hand :: [[Int]]
}

instance Show TempMem where
    show (M a h) = show a ++ show h

instance Semigroup TempMem where
    (<>) (M a1 h1) (M a2 h2) = M (liftM2 (++) a1  a2) (liftM2 (++) h1 h2)

instance Monoid TempMem where
    mempty = M [[]] [[]]

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
    Result rest res -> Result rest (M [[res]] [[]])
    Error _ -> case parse parseHand i of
        Result rest res -> Result rest (M [[]] [[res]])
        Error e -> Error e
    )

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
isAction :: CharConverter PureAction
isAction c act = is c >> pure act

-- parseHand = ace ||| ten ||| parseDigit

parseDigit :: Parser Int
-- <&> is the flipped version of <$>
-- takes context then function
parseDigit = satisfy isDigit <&> digitToInt

isSpecialDigit :: CharConverter Int
isSpecialDigit = charConverter

charConverter :: CharConverter a
charConverter c r = is c >> pure r

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- character
    if f c then pure c else unexpectedCharParser c

parseMoreMem :: Parser TempMem
parseMoreMem = tempMemList parseMem

tempMemList :: Parser TempMem  -> Parser TempMem
tempMemList p1 = tempMemList1 p1 ||| pure mempty

tempMemList1 :: Parser TempMem -> Parser TempMem
tempMemList1 p = do
    p' <- p
    p'' <- tempMemList p
    pure (p' <> p'')

parseAll :: Parser TempMem
parseAll = do
    tempMeml <- list (is '/' >> parseMoreMem)
    pure (foldl1 (\(M act1 h1) (M act2 h2) -> M (act2++act1) (h2++h1)) tempMeml)
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
