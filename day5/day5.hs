import Text.Parsec
import Data.List


loadInput :: IO String
loadInput = readFile "../inputs/input_5.txt"
 

data Move = B | F | L | R
    deriving(Show)
    
move ::  Parsec String () Move
move = back <|> forward <|> left <|> right
  where 
     back = B <$ char 'B' 
     forward = F <$ char 'F'
     left = L <$ char 'L'
     right = R <$ char 'R'
        
oneline :: Parsec String () [Move] 
oneline = many move  <* (char '\n') 

parseInput :: String -> [[Move]]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many oneline) "" input 

data SeatRange  = SeatRange {rowMin :: Int,
                             rowMax :: Int,
                             coluMin :: Int,
                             coluMax :: Int}    


                             
oneMove :: SeatRange -> Move -> SeatRange
oneMove (SeatRange rmin rmax cmin cmax) b = case b
                                              of
                                                B ->  SeatRange  ((div (rmin+rmax) 2)+1) rmax cmin cmax
                                                F ->  SeatRange  rmin (div (rmin+rmax) 2) cmin cmax
                                                L ->  SeatRange  rmin rmax cmin (div (cmin+cmax) 2) 
                                                R ->  SeatRange  rmin rmax ((div (cmin+cmax) 2)+1) cmax
                                                
getSeat :: [Move] -> (Int,Int)
getSeat moves=  let lastSeat = foldl oneMove  (SeatRange 0 127 0 7) moves
                 in (rowMin lastSeat, coluMin lastSeat)

missingSeat :: [[Move]] -> [(Int,Int)]
missingSeat moves = let seats = getSeat <$> moves
                        seatslist = [(i,j)|i<-[0..127],j<-[0..7]]
                      in filter (not . (flip elem) seats) seatslist
                      
getMySeat :: [(Int,Int)] -> Int
getMySeat seats = let nobords = filter (\(x,y) -> x /= 0 && x/=127 && y /= 0 && y /= 7) seats
                      ids = (\(x,y)->8*x+y) <$> nobords
                      goodID (a : ass) = if (elem (a+1) ids) && (elem (a-1) ids) then goodID ass else a  
                      goodID _ = error "none of them workds"
                   in goodID ids  
                     
--getMaxId = maximum. (<$>) (\(x,y) -> 8*x+y)                      
                 
main = do
l <-  parseInput<$>loadInput
let solu = getMySeat.missingSeat $ l


putStrLn $ show $ solu