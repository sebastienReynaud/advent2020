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

--data PileOfJolt = PileOfJolt {} deriving (Show, Eq) 

getListDifferences :: [Int] -> [Int]
getListDifferences (a:b:ass) = b-a : getListDifferences (b : ass)
getListDifferences (a:ass) = []    

getSolu :: [Int] -> Int
getSolu l = let difs = getListDifferences$ 0 : sort  l
                n1n3 :: [[Int]]
                n1n3 = [filter (==1), filter (==3)] <*> [difs]
            in  (length.head $ n1n3)+(product ( length <$> n1n3))
main :: IO ()
main = do

l <- parseInput <$> loadInput 

putStrLn $ show $ getSolu l 