import Text.Parsec

loadInput :: IO String
loadInput = readFile "../inputs/input_1.txt"

integer :: Parsec String () Integer
integer =  read <$> many digit <* char '\n' 
                   

parseInput :: String -> [Integer]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many integer) "" input
    
 
getproductOfThree :: [Integer] -> [Integer]
getproductOfThree l = [a*b*c | a <- l, b <- l, c <- l,  a+b+c==2020] 

main :: IO ()
main = do
l <- parseInput <$> loadInput
--putStrLn $ show $ l
let product = head $ getproductOfThree l 

putStrLn $ show $ product