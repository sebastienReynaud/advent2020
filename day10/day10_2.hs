import Text.Parsec
import Control.Monad.State
import Data.List


loadInput :: IO String
loadInput = readFile "../inputs/input_10.txt"

int :: Parsec String () Int
int =  read <$>  many digit <* char '\n'
      
      
parseInput :: String -> [Int]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many int) "" input  


getListDifferences :: [Int] -> [Int]
getListDifferences (a:b:ass) = b-a : getListDifferences (b : ass)
getListDifferences (a:ass) = []    

splitInSub :: [Int] -> [[Int]]
splitInSub l@(a:ass) = (takeWhile (/=3) l ++ [3]) : (if rest /= [] then splitInSub $ tail rest  else [])
                where rest = dropWhile (/=3)  l
splitInSub _ = []  

countSubNum :: [Int] -> Int
countSubNum (1 : 1 : 1 : 3 : []) = 4
countSubNum (1 : 1: 3 : []) = 2 
countSubNum (1 : 3 : []) = 1
countSubNum (3 : []) = 1
countSubNum (1 : 1 : 1 : 1 : bs) = countSubNum (1: 1: 1: bs)+ countSubNum(1:1:bs) + countSubNum (1:bs)
countSubNum _ = error "unexpected list was fed ! fuck oyu dude" 

                
main :: IO ()
main = do

l <- parseInput <$> loadInput 

let lorderbounded = [0] ++ sort l ++ [ (maximum l) + 3]
let solu2 = product $ countSubNum <$> splitInSub (getListDifferences lorderbounded)

putStrLn $ show $  solu2