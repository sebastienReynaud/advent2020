import Text.Parsec
import Control.Monad.State
import Data.List
import Data.Array

type SeatGrid = Array (Int,Int) (Maybe Bool)


loadInput :: IO String
loadInput = readFile "../inputs/input_11.txt"

oneSeat :: Parsec String () (Maybe Bool)
oneSeat = occupiedSeat <|> emptySeat <|> noSeat
    where occupiedSeat = Just True <$ char '#'
          emptySeat = Just False <$ char 'L'
          noSeat = Nothing <$ char '.'

seatRow :: Parsec String () [Maybe Bool]
seatRow =  many oneSeat <* char '\n'
      
      
parseInput :: String -> [[Maybe Bool]]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many seatRow) "" input  


            
iniSeatGrid :: [[Maybe Bool]] -> SeatGrid
iniSeatGrid l = array ((1,1),(95,91)) [((i,j) , l!!(i-1)!!(j-1)) | i <- [1..95], j <- [1..91]]


cleanCoord :: (Int,Int) -> (Int,Int)
cleanCoord (0,0) = (1,1)
cleanCoord (96,92) = (95,91)
cleanCoord (0,92) = (1,91)
cleanCoord (96,0) = (95,1)
cleanCoord (0,x) = (1,x)
cleanCoord (96,x) = (95,x)
cleanCoord (x,0) = (x,1)
cleanCoord (x,92) = (x,91)
cleanCoord x = x

getFirstSeat :: SeatGrid -> (Int,Int) -> (Int,Int) -> Int
getFirstSeat s pos dir = let minDis = case dir of
                                           (1,0) -> 96-fst pos
                                           (0,1) -> 92 - snd pos
                                           (1,1) -> min (96 - fst pos) (92 -snd pos)
                                           (-1,0) -> fst pos
                                           (-1,1) -> min (fst pos) (92 - snd pos)
                                           (-1,-1) -> min (fst pos) (snd pos)
                                           (1,-1) -> min (96-fst pos) (snd pos)
                                           (0,-1) -> snd pos
                             listLine = if minDis > 1
                                        then [s!(fst pos + n * fst dir, snd pos + n * snd dir) | n <- [1..(minDis-1)] ]
                                        else [Nothing]
                             moveInDir = dropWhile (== Nothing) listLine
                             f (Just True : rest ) = 1
                             f (Just False : rest ) = 0
                             f ([]) = 0
                          in f (moveInDir)



nCanSee :: SeatGrid -> (Int,Int) -> Int
nCanSee s pos = let dirs = [(i,j) | i<-[-1..1] , j <- [-1..1], (i,j) /= (0,0)]
                 in sum $ getFirstSeat s pos <$> dirs 



incrSeat :: SeatGrid -> (Int,Int) -> ((Int,Int), Maybe Bool)
incrSeat thear cord = case thear!cord of
                           Just True -> if n >= 5 then (cord, Just False) else (cord, Just True) 
                           Just False -> if n == 0 then (cord, Just True) else (cord, Just False)
                           Nothing -> (cord, Nothing)
                        where n = nCanSee thear cord

increm :: Control.Monad.State.State SeatGrid ()
increm = do 
        theArr <- get
        let indlist = indices theArr
        let newAr = incrSeat theArr <$> indlist
        put $ theArr // newAr
        
didFreeze :: Control.Monad.State.State SeatGrid Bool
didFreeze = do 
            theArr <- get
            let ans = (incrSeat theArr <$> indices theArr)
            return (theArr == (theArr // ans)) 
            
countSteps :: SeatGrid -> Int
countSteps s = if  fst (runState didFreeze s )
               then countOccupied s 
               else countSteps (snd (runState increm s))
               where countOccupied a = length . filter (== Just True) $ elems a
            
main :: IO ()
main = do

l <- parseInput <$> loadInput 
let iniArray = iniSeatGrid l

putStrLn $ show $   countSteps iniArray