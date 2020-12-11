import Text.Parsec

loadInput :: IO String
loadInput = readFile "../inputs/input_3.txt"
 

lineToTree :: String -> [Bool]
lineToTree = (<$>) (\x -> case () of 
                             _ | x == '#' -> True
                               | x == '.' -> False
                               | otherwise -> error " no a tree wtf")

howManyTrees31 :: [[Bool]] -> Int -> Integer 
howManyTrees31 (a : ass) pos = onTree + howManyTrees31 ass ((pos+3) `mod` length a) where onTree = if (a!!pos) then 1 else 0
howManyTrees31 [] _ = 0   



howManyTrees11 :: [[Bool]] -> Int -> Integer 
howManyTrees11 (a : ass) pos = onTree + howManyTrees11 ass ((pos+1) `mod` length a) where onTree = if (a!!pos) then 1 else 0
howManyTrees11 [] _ = 0



howManyTrees51 :: [[Bool]] -> Int -> Integer 
howManyTrees51 (a : ass) pos = onTree + howManyTrees51 ass ((pos+5) `mod` length a) where onTree = if (a!!pos) then 1 else 0
howManyTrees51 [] _ = 0                             




howManyTrees71 :: [[Bool]] -> Int -> Integer 
howManyTrees71 (a : ass) pos = onTree + howManyTrees71 ass ((pos+7) `mod` length a) where onTree = if (a!!pos) then 1 else 0
howManyTrees71 [] _ = 0



howManyTrees12 :: [[Bool]] -> Int -> Integer 
howManyTrees12 (a : b : ass) pos = onTree + howManyTrees12 ass ((pos+1) `mod` length a) where onTree = if (a!!pos) then 1 else 0
howManyTrees12 (a:[]) pos=  if (a!!pos) then 1 else 0
howManyTrees12 [] _ = 0

main = do
l <- (lines <$> loadInput)
let theTrees = lineToTree <$> l


--let sol = getSolu l
putStrLn $ show $   product ([howManyTrees11, howManyTrees12, howManyTrees31, howManyTrees51, howManyTrees71] <*> [theTrees] <*> [0] )