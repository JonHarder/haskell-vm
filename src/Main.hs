module Main where

import qualified Data.Map as Map

data Register =
    EAX
  | EBX
  | ECX
  | EDX
  deriving (Show, Ord, Eq)

type Memory = Map.Map Address Value
type Registers = Map.Map Register Value



data VM = VM {
    registers :: Registers
  , instructions :: Program
  , instructionPointer :: Int
}


lpad :: [a] -> Int -> a -> [a]
lpad as num d = replicate (num - length as) d ++ as


showRegister :: (Register, Value) -> String
showRegister (register, value) = show register  ++ ": " ++ show value

instance Show VM where
  show vm =
    let reg = map showRegister $ Map.toAscList (registers vm)
    in unlines [
        "registers"
      , "----------"
      , unlines reg
      ]

type Size = Int

data Address = Address Int
  deriving (Ord, Eq)

instance Show Address where
  show (Address n) = "0x" ++ lpad (show n) 8 '0'

data VMConfig = Conf
  {size :: Size
  }


defaultConfig :: VMConfig
defaultConfig = Conf { size = 10 }


initRegisters :: Registers
initRegisters = Map.fromList [(EAX, 0), (EBX, 0), (ECX, 0), (EDX, 0)]


initVM :: VMConfig -> Program -> VM
initVM conf program = VM
  { registers = initRegisters
  , instructions = program
  , instructionPointer = 0
  }


type Value = Int


data VMExecutionError =
    MemoryOutOfBoundsError
  | NotImplementedError
  deriving Show


data Command =
    Add Register Value
  | Sub Register Value
  | Move Register Value
  deriving Show


command :: Command -> VM -> Either VMExecutionError VM
command (Move register val) vm =
  let reg = registers vm
      ip = instructionPointer vm
  in Right $ vm { instructionPointer = ip + 1, registers = Map.insert register val reg }
command (Add register val) vm =
  let reg = registers vm
      ip = instructionPointer vm
  in Right $ vm { instructionPointer = ip + 1, registers = Map.insertWith (+) register val reg }
command (Sub register val) vm =
  let reg = registers vm
      ip = instructionPointer vm
  in Right $ vm { instructionPointer = ip + 1, registers = Map.insertWith (flip (-)) register val reg }


data Program = Program [Command]


p1 = Program [
    Move EAX 8
  , Move EDX (-1)
  , Add EDX 7
  , Sub EDX 2
  ]



run :: VM -> VM
run vm =
  let ip = instructionPointer vm
      (Program is) = instructions vm
  in if ip >= length is then
       vm
     else
       let c = is !! ip
       in case command c vm of
         Left e -> error $ show e
         Right vm' -> run vm'


main :: IO ()
main = do
  let vm = initVM defaultConfig p1
  print $ run vm
