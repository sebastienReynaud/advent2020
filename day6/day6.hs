import Text.Parsec
import Data.List


loadInput :: IO String
loadInput = readFile "../inputs/input_6.txt"
 

separateGroups :: String -> [String]
separateGroups s = let l = lines s
                       f (a : b : ass) = if (b /= "") then   f ((a++" "++ b) : ass) else a : f ass
                       f (a : [] ) = [a] 
                       f [] = []
                    in f l                      
 
 
 
countCommonYES :: String -> Int 
countCommonYES s = let peopleList :: String -> [String]
                       peopleList aa@(a:ass) = fst (splitChar ' ' aa) : peopleList (snd (splitChar ' ' aa)) 
                       peopleList [] = []
                       splitChar :: Char -> String -> (String,String)
                       splitChar c s = (takeWhile (/= c) s, if thenext == [] then [] else tail thenext) where thenext = dropWhile (/=c) s    
                       plist = peopleList s
                    in length $ foldr intersect (head plist) plist
                    
peopleList :: String -> [String]
peopleList aa@(a:ass) = fst (splitChar ' ' aa) : peopleList (snd (splitChar ' ' aa)) 
peopleList [] = []
splitChar :: Char -> String -> (String,String)
splitChar c s = (takeWhile (/= c) s, if thenext == [] then [] else tail thenext) where thenext = dropWhile (/=c) s                  

                    
 
getNQ :: String -> Int
getNQ = length.nub
 
main = do
l <-  loadInput
let sol = sum $ countCommonYES <$> separateGroups l


putStrLn $ show $ sol