import Text.Parsec
import Control.Monad.State


loadInput :: IO String
loadInput = readFile "../inputs/input_9.txt"

int :: Parsec String () Int
int =  read <$>  many digit <* char '\n'
      
      
parseInput :: String -> [Int]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many int) "" input  

data CState = CState {actuPos :: Int,
                      book :: [Int]}
 

isSumOfTwo :: Int -> [Int] -> Bool 
isSumOfTwo a ints = (length . filter (\(a,b)-> a/=b))[(i,j) | i<-ints, j<-ints, i+j == a] > 0

    
isOK :: Control.Monad.State.State CState (Bool,Int)     
isOK = do
        ss <- get
        let pos = actuPos ss
        let theBook = book ss
        let pream = take 25 . reverse $ fst $ (splitAt pos) theBook
        let val = theBook!!pos
        if isSumOfTwo val pream
        then return (True, val)
        else return (False, val)
       
nextNum :: Control.Monad.State.State CState ()
nextNum = do 
        ss <- get
        let pos = actuPos ss
        put $ ss {actuPos = pos+1}
        
       
findWeakness :: CState -> Int  
findWeakness s =  if fst isWeak 
                then findWeakness $ snd $ runState nextNum  s
                else snd isWeak
         where isWeak = fst $ runState isOK s 
         
         

         
sumOfSublistGivenLength :: Int -> [Int] -> [Int]
sumOfSublistGivenLength l nums = if length nums < l
                                 then []
                                 else sum (take l nums) : sumOfSublistGivenLength l (tail nums)
                                 
getSize ::  Int -> [Int] -> Int
getSize n nums =head $ ((<$>) length)  (filter or $  numtobool $ (\x -> sumOfSublistGivenLength x) <$> [2..(length nums)] <*> [nums])
        where numtobool :: [[Int]] -> [[Bool]]
              numtobool a = f <$> a
              f  = (<$>) ( == n)

              
solu2 :: Int -> Int -> [Int] -> Int
solu2 n sumsize nums = let substart =  length (takeWhile (/= n) (sumOfSublistGivenLength sumsize nums))
                           sub = take sumsize (drop substart nums) 
                         in maximum sub + minimum sub
main :: IO ()        
main = do

l <- parseInput <$> loadInput 
let iniState = CState 25 l
let solu = solu2 (findWeakness iniState) ((length l + 1)- getSize (findWeakness iniState) l) l 



putStrLn $ show $ solu