import Text.Parsec

loadInput :: IO String
loadInput = readFile "../inputs/input_4.txt"
 

data Spec = Birthday String | Height String | IssuDate String |  ExpirationDate String | HairColor String | EyeColor String | PassportID String | CountryId String
    deriving (Show)
 
spec :: Parsec String () Spec
spec = (birth <|> startWithH <|> issu <|> startWithE <|> passId <|> countId)
  where
    height = Height <$> (string "gt:"*> many (noneOf " ") <* spaces)
    haircolor = HairColor <$> (string "cl:"*>  many (noneOf " ") <* spaces)
    expi = ExpirationDate <$> (string "yr:" *>  many (noneOf " ") <* spaces)
    eyecol  = EyeColor <$> (string "cl:" *> many (noneOf " ") <* spaces)
    birth =
       Birthday <$> (string "byr:" *>  many (noneOf " ") <*  spaces)
    startWithH = char 'h' *> (height <|> haircolor)
    issu =
       IssuDate <$> (string "iyr:" *> many (noneOf " ") <*  spaces)
    startWithE = char 'e' *> (eyecol <|> expi)
    passId =
       PassportID <$> (string "pid:" *>  many (noneOf " ") <*  spaces)
    countId =
       CountryId <$> (string "cid:" *>  many (noneOf " ") <*  spaces)       
 
  
 
parseOneID :: String -> [Spec]
parseOneID input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many spec) "" input 
 

isOK :: [Spec] -> Bool
isOK specs  |  length specs < 7 = False 
            |  length specs == 7 =  if foldr combin 0 specs == 1 then False else True
            | length specs == 8 = True
            |  otherwise = error "wrong number of entries TOOMANY"
              where combin :: Spec -> Int -> Int
                    combin (CountryId _) 0 = 1
                    combin _ x = x
                    
areSpecsOK ::[Spec] -> Bool
areSpecsOK (a : ass) = isFeatOk a && areSpecsOK ass
    where isFeatOk (Birthday s@(_:_:_:_:[])) = read s >= 1920 && read s <= 2002
          isFeatOk (Birthday _) = False
          isFeatOk (IssuDate s@(_:_:_:_:[])) = read s >= 2010 && read s <= 2020
          isFeatOk (IssuDate _) = False
          isFeatOk (ExpirationDate s@(_:_:_:_:[])) = read s >= 2020 && read s <= 2030
          isFeatOk (ExpirationDate _) =False
          isFeatOk (Height (s1:s2:s3: "cm")) = read [s1,s2,s3] >= 150 && read [s1,s2,s3]<= 193
          isFeatOk (Height (s1:s2:"in")) = read [s1,s2] >= 59 && read [s1,s2] <= 76
          isFeatOk (Height _) = False
          isFeatOk (HairColor ('#':rest)) = (length rest == 6) && and (elem <$> rest <*> ["abcdef0123456789"])
          isFeatOk (HairColor _ )= False
          isFeatOk (EyeColor x) = x `elem` ["amb","blu","brn", "gry", "grn","hzl","oth"]
          isFeatOk (PassportID x) = (length x == 9) && and (elem <$> x <*> ["0,1,2,3,4,5,6,7,8,9"])
          isFeatOk (CountryId x) = True
areSpecsOK [] = True                    

okTot :: [Spec] -> Bool 
okTot x = areSpecsOK x && isOK x                   

separatePasPort :: String -> [String]
separatePasPort s = let l = lines s
                        f (a : b : ass) = if (b /= "") then   f ((a++" "++ b) : ass) else a : f ass
                        f (a : [] ) = [a] 
                        f [] = []
                     in f l

                     
countTotOk :: String -> Int
countTotOk ss = length . filter (okTot) $ listports
              where listports =  parseOneID <$> separatePasPort ss
                      
 
main = do
l <-  loadInput


let solu = countTotOk l


putStrLn $ show $  solu