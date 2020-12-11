import Text.Parsec
import Data.Bool

loadInput :: IO String
loadInput = readFile "../inputs/input_2.txt"

integer :: Parsec String () Integer
integer =  read <$> many digit

passSpec :: Parsec String () PassCode
passSpec =  PassCode  <$> integer <* char '-' <*> integer <* spaces <*> letter <* char ':' <*  spaces <*> many (noneOf "\n") <* optional (char '\n')
                   
                   
data PassCode = PassCode {minR :: Integer,
                          maxR :: Integer,
                          whichChar :: Char,
                          pswd :: String} deriving (Show)
                   
                   
parseInput :: String -> [PassCode]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many passSpec) "" input
    


xor :: Bool -> Bool -> Bool 
xor a b = (a || b)  && (not (a && b))  
    
areValid :: [] PassCode -> [Bool]
areValid  l = isValid <$> l
  where isValid (PassCode amin amax cchar pword) = (one ==cchar) `xor` ( two == cchar)  
                                                   where  one = pword!!(fromIntegral amin -1)
                                                          two = pword!!(fromIntegral amax -1) 
                                                          
       
    
tot :: [] Bool -> Int
tot = length . filter (id)    

solu = tot.areValid 

main :: IO ()
main = do
l <- parseInput <$> loadInput
let a = solu l
putStrLn $ show $ a

--let sol = getSolu l
--putStrLn $ show $ sol