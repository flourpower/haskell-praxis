--First, we need some new types
type State = Integer
type Symbol = Char
data Direction = L | R deriving(Eq,Ord,Read,Show)
type Tape = ([Symbol],Integer)
type Instruction = (State, Symbol, Symbol, Direction,State)
type TM = ([Instruction],[State],State)


atHead :: Tape -> Symbol
atHead (symbols,index) = symbols !! (fromIntegral index)

moveHead :: Tape -> Direction -> Tape
moveHead (symbols,0) L = (symbols,0)
moveHead (symbols,index) L = (symbols,(index-1))
moveHead (symbols,index) R = (symbols,(index+1))

write :: Tape -> Symbol -> Tape
write (symbols,index) newSymbol = (newSymbols, index)
  where k = fromIntegral index
        newSymbols = (take k symbols) ++ [newSymbol] ++ (drop (k+1) symbols)

getState :: TM -> State
getState (instructions, states, state) = state

swapState :: TM -> State -> TM
swapState (instructions, states, state) newState = (instructions, states, newState)

getInstructions :: TM -> [Instruction]
getInstructions (instructions,_,_) = instructions

--this is where we do the calling from
run :: TM -> Tape -> Tape
run tm tape
  | state == (-1) = tape
  | otherwise     = run newTm newTape
    where state = getState tm
          (newTm,newTape) = execute tm tape

--we're gonna call execute until our current state goes to -1, then we do something else
execute :: TM -> Tape -> (TM, Tape)
execute tm tape = (newTM,newTape)
  where (symbol,state) = (atHead tape, getState tm)
        (q1,s1,s2,d,q2) = grab state symbol instructions
        newTM = swapState tm q2
        tape' = write tape s2
        newTape = moveHead tape' d
        instructions = getInstructions tm
        

--grab takes the current state and symbol and and instruction list and finds the right instruction
--we assume that grab never fails
grab :: State -> Symbol -> [Instruction] -> Instruction
grab state symbol instructions = head . (dropWhile f) $ instructions
  where f (state',symbol',_,_,_) = not $ (state == state') && (symbol == symbol')


i1 = (0, '1', '1', R, 0)
i2 = (0, '+', '1', R, 1)
i3 = (1, '1', '1', R, 1)
i4 = (1, '_', '_', L, 2)
i5 = (2, '1', '_', L, (-1))

is = [i1,i2,i3,i4,i5]
addStates = [0,1,2,(-1)]
adderTM = (is,addStates,0)

input = ( ("111+1" ++ (cycle "_") ) , 0 )



