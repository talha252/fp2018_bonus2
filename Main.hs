import Data.Char

data Color = Red | Black 
             deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace
            deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank }
            deriving (Show, Eq)
data Move = Draw | Discard Card
            deriving Show
data State = Cont | Stop

type Goal = Int
type CardList = [Card]
type HeldCards = CardList

cardColor :: Card -> Color
cardColor (Card {suit = s, rank = _})
   | s == Hearts || s == Diamonds = Red
   | otherwise                    = Black
  
cardValue :: Card -> Int
cardValue (Card {suit = _, rank = r}) = case r of
   Num n -> n 
   Ace     -> 11
   _       -> 10
   
removeCard :: CardList -> Card -> CardList
removeCard cs c 
   | c `notElem` cs = error "Not in deck"
   | otherwise         = removeCard' [] cs c
      where
      removeCard' :: [Card] -> [Card] -> Card -> [Card]
      removeCard' acc []     _ = acc
      removeCard' acc (c':cs) c''
         | c' == c''   = acc ++ cs
         | otherwise = removeCard' (c':acc) cs c'' 

sumCards :: CardList -> Int
sumCards cs = sumCards' 0 cs
   where
   sumCards' :: Int -> [Card] -> Int
   sumCards' acc []     = acc
   sumCards' acc (c:cs) = sumCards' (acc + cardValue c) cs

allSameColor :: [Card] -> Bool
allSameColor []        = True
allSameColor [x]       = True
allSameColor (x:y:xs)
   | cardColor x == cardColor y = allSameColor (y:xs)
   | otherwise                  = False

score :: [Card] -> Goal -> Int
score cs goal 
   | allSameColor cs = preliminary `div` 2
   | otherwise       = preliminary
      where
      sum = sumCards cs 
      preliminary
         | sum > goal = 3 * (sum - goal)
         | otherwise  = goal - sum    

runGame :: [Card] -> [Move] -> Goal -> Int
runGame cs ms g = runGame' [] cs ms
   where
      runGame' :: HeldCards -> [Card] -> [Move] -> Int
      runGame' hs cs []      = score hs g
      runGame' hs cs (m:ms') = case m of
         Discard c -> runGame' (removeCard hs c) cs ms'
         Draw      -> drawCard cs
            where
            drawCard :: CardList -> Int
            drawCard []     = score hs g
            drawCard (c:cs) 
               | sumCards hs' > g    = score hs' g
               | otherwise           = runGame' hs' cs ms'
                  where
                  hs' = (c:hs)
         
convertSuit :: Char -> Suit
convertSuit c
   | c `elem` "dD" = Diamonds
   | c `elem` "cC" = Clubs
   | c `elem` "hH" = Hearts
   | c `elem` "sS" = Spades
   | otherwise     = error "Unknown suit"
   
convertRank :: Char -> Rank
convertRank c
   | c `elem` "tT" = Num 10
   | c `elem` "jJ" = Jack
   | c `elem` "qQ" = Queen
   | c `elem` "kK" = King 
   | c == '1'      = Ace
   | isDigit c     = Num (digitToInt c)
   | otherwise     = error "Unknown rank"
   
convertCard :: Char -> Char -> Card
convertCard s r = Card (convertSuit s) (convertRank r)

convertMove :: Char -> Char -> Char -> Move
convertMove m s r
   | m `elem` "dD" = Draw
   | m `elem` "rR" = Discard (convertCard s r)
   | otherwise     = error "Unrecognized move"

readCards :: IO [Card]
readCards = readCards' []
   where
   readCards' :: [Card] -> IO [Card]
   readCards' cs = do
      s <- getChar
      if s == '.'
         then do getChar -- to get new line character
                 return cs
         else do r <- getChar
                 getChar -- to get new line character
                 let c = convertCard s r
                 readCards' (cs ++ [c])

readMoves :: IO [Move]
readMoves = readMoves' []
   where
   readMoves' :: [Move] -> IO [Move]
   readMoves' ms = do
      m <- getChar
      if m == '.'
         then do getChar -- to get new line character
                 return ms
         else if m `elem` "dD"
              then do let m' = convertMove m ' ' ' '
                      getChar  -- to get new line character
                      readMoves' (ms ++ [m'])
              else do s <- getChar
                      r <- getChar
                      getChar -- to get new line character
                      let m' = convertMove m s r
                      readMoves' (ms ++ [m'])

main = do putStrLn "Enter cards:"
          cards <- readCards
          putStrLn "Enter moves:"
          moves <- readMoves
          putStrLn "Enter goal:"
          line <- getLine
          let goal = read line :: Int
          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)   
   
   
   
   
   
   
   
   
   
   
   
   
   
   

