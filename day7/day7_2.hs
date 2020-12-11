import Text.Parsec
import Data.List


loadInput :: IO String
loadInput = readFile "../inputs/input_7.txt"

data Bag = Bag {adjective :: String,
                color :: String} deriving (Show, Eq)

 
data Rule = Rule  {bagType :: Bag,
                   contains :: [(Int, Bag)]} deriving (Show)

bag :: Parsec String () Bag
bag = Bag <$> many (noneOf " ") <* spaces <*> many (noneOf " ") <* spaces <* string "bags"


bags :: Parsec String () (Int,Bag)
bags =  noOther <|> oneBag <|> severalBags 
    where noOther = (0,Bag "NOOBODY" "NOBODY") <$ string  "no other bags."
          oneBag = (,) <$>  (read <$> string "1") <* string " " <*> (Bag <$> many (noneOf " ") <* spaces <*> many (noneOf " ") <* spaces <* string "bag" <* ending) 
          severalBags = (,) <$> (read <$> many digit) <* string " " <*> (Bag <$> many (noneOf " ") <* spaces <*> many (noneOf " ")<* spaces <* string "bags" <* ending) 
          ending = moreBs <|> finalB
          moreBs = string ", "
          finalB = string "."
          
                   
                   
rule :: Parsec String () Rule 
rule = Rule <$> bag  <* string " contain " <*> many bags  <* char '\n'           

parseInput :: String -> [Rule]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many rule) "" input  

countSubBags :: Bag -> [Rule] -> Int
countSubBags b rules = let inirule = findBagRule b rules
                           subrules = contains inirule
                        in if bagType inirule == Bag "fuck" "you"
                         then 0
                         else sum $ (\(n, bag) -> n*(1+ countSubBags bag rules) ) <$> subrules

findBagRule :: Bag -> [Rule] -> Rule
findBagRule  b rules = case filter (\x -> bagType x == b) rules 
                         of _ :[] -> head $  filter (\x -> bagType x == b) rules 
                            otherwise -> Rule (Bag "fuck" "you") []


main = do
l <-  parseInput <$> loadInput

let solu = countSubBags (Bag "shiny" "gold") l


putStrLn $ show $ solu