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

shinyGoldBasis :: [Rule] -> [Bag]
shinyGoldBasis l =  concat $ canHold <$> l
    where canHold (Rule b contained) = if or ((\(x,y) -> adjective y == "shiny" && color y == "gold") <$> contained) then [b] else []  


canMyShinyGoldFit :: Bag -> [Rule] -> [Bag] -> Bool   
canMyShinyGoldFit b rules basis = if elem b basis
                                then True
                                else if b == Bag "NOOBODY" "NOBODY"
                                   then False 
                                   else or $ (\x -> canMyShinyGoldFit x rules basis) <$> tolook
                                        where tolook = nub $ concat [ snd <$> (contains x) | x<-rules, bagType x == b]
    

countTot :: [Bag] -> [Rule] -> [Bag] -> Int
countTot bs rules basis= length $ filter (\x -> canMyShinyGoldFit x rules basis) bs    
  

main = do
l <-  parseInput <$> loadInput
let basis = shinyGoldBasis  l
let listOfBags = (\(Rule b c)-> b) <$> l
let solu = countTot listOfBags l basis



putStrLn $ show $ solu