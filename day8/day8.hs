import Text.Parsec
import Data.List
import Control.Monad.State


loadInput :: IO String
loadInput = readFile "../inputs/input_8.txt"

int :: Parsec String () Int
int = (posit <|> negat)
           where posit = read <$> ( char '+'*> many digit)
                 negat = (\x -> - (read x ::Int)) <$> (char '-' *> many digit )  


data Instruction = Acc Int | Jmp Int | Nop deriving (Show)           
                   
instruction :: Parsec String () Instruction 
instruction = acc <|> jmp <|> nop
            where acc = Acc <$>( string "acc " *> int <* char '\n')
                  jmp = Jmp <$> (string "jmp " *> int <* char '\n')
                  nop = Nop <$ (string "nop " <* int <* char '\n')

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
        Nop -> put $ theState {history = (presentLine +1) : hist}


hasloop :: Control.Monad.State.State HistAcc Bool
hasloop = do
            theState <- get
            let hist = history theState
            return $ elem (head hist) $ tail hist 
           
            
getAccu :: Control.Monad.State.State HistAcc Int
getAccu = do 
            theState <- get
            return $ accumulator theState
            
fillTillLoop :: HistAcc -> Int
fillTillLoop astte = if fst $ runState hasloop astte
                   then fst $ runState getAccu astte
                   else fillTillLoop (snd $ runState exeLine astte)
 

        
main = do

l <- parseInput <$> loadInput 
let solu = fillTillLoop (HistAcc l [0] 0)

putStrLn $ show $ solu