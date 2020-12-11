import Text.Parsec
import Data.List
import Control.Monad.State


loadInput :: IO String
loadInput = readFile "../inputs/input_8.txt"

int :: Parsec String () Int
int = (posit <|> negat)
           where posit = read <$> ( char '+'*> many digit)
                 negat = (\x -> - (read x ::Int)) <$> (char '-' *> many digit )  


data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show,Eq)           
                   
instruction :: Parsec String () Instruction 
instruction = acc <|> jmp <|> nop
            where acc = Acc <$>( string "acc " *> int <* char '\n')
                  jmp = Jmp <$> (string "jmp " *> int <* char '\n')
                  nop = Nop <$> (string "nop " *> int <* char '\n')

parseInput :: String -> [Instruction]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many instruction) "" input  


data HistAcc = HistAcc {instructs :: [Instruction],
                        history :: [Int],
                        accumulator :: Int}    
    
exeLine :: Control.Monad.State.State HistAcc ()
exeLine = do 
   theState <- get 
   let hist = history theState
   let presentLine = head hist
   let toExec :: Instruction
       toExec = instructs theState !! presentLine
   let accu =  accumulator theState     
   case toExec 
     of Acc n -> put $ theState {accumulator = accu + n, history = (presentLine +1) : hist }
        Jmp n -> put $ theState {history = (presentLine + n) : hist}
        Nop n -> put $ theState {history = (presentLine +1) : hist}

hasEnded :: Control.Monad.State.State HistAcc Bool
hasEnded = do
             theState <- get
             let nowLine = head $ history theState
             let insts = instructs theState
             return $ length insts - 1 < nowLine

hasloop :: Control.Monad.State.State HistAcc Bool
hasloop = do
            theState <- get
            let hist = history theState
            return $ elem (head hist) $ tail hist 
           
            
getAccu :: Control.Monad.State.State HistAcc Int
getAccu = do 
            theState <- get
            return $ accumulator theState
            
fillTillLoopOrEnd :: HistAcc -> (Bool, Int)
fillTillLoopOrEnd astte = if fst $ runState hasloop astte
                        then (False, fst $ runState getAccu astte)
                        else if fst $ runState hasEnded astte
                           then (True, fst $ runState getAccu astte)
                           else fillTillLoopOrEnd (snd $ runState exeLine astte)

splitAtJumpOrNop :: [Instruction] -> ([Instruction], [Instruction])
splitAtJumpOrNop l = (takeWhile (not.isJumpOrNop)  l, if thenext == [] then [] else thenext) where thenext = dropWhile (not.isJumpOrNop) l
                                                                                                   isJumpOrNop :: Instruction -> Bool
                                                                                                   isJumpOrNop x = case x 
                                                                                                                     of Jmp _ -> True
                                                                                                                        Nop _ -> True
                                                                                                                        Acc _ -> False

                                                                                                                       
                                                                                                                       
allInstr :: [Instruction] -> [[Instruction]]
allInstr inss =  if inss == []
               then []
               else modifiedIns : ((\x -> fspart ++ [head sndpart] ++ x) <$> sndDone) 
    where splitted = splitAtJumpOrNop inss
          fspart = fst splitted
          sndpart = snd splitted
          flipJmpNop :: Instruction -> Instruction
          flipJmpNop (Jmp n) = Nop n
          flipJmpNop (Nop n) = Jmp n
          flipJmpNop _ = error "asekd to lfip unknown guy"
          modifiedIns = fspart ++ (flipJmpNop (head $ sndpart) : tail (sndpart))
          sndDone = allInstr (tail sndpart)


getFinished :: [[Instruction]] -> [( Bool,Int)]          
getFinished = filter (\(x,y) -> x) . ( fillTillLoopOrEnd <$> (\x -> HistAcc x [0] 0) <$>) -- (Histcc -> Boolint) (->) [instr] HistAcc ------->>>> (->) [instr] Boolint
        
main = do

l <- parseInput <$> loadInput 
let solu = getFinished.allInstr $ l

putStrLn $ show $ solu