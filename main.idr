module Main

import Data.String
import Data.SortedMap
import Data.List1
import Data.Maybe
import System.File
import System

data Operator : Type where
  (+) : Operator
  (*) : Operator
  Store : Operator
  Restore : Operator
  Pop : Operator
  Print : Operator
  Nop : Operator

implementation Cast String Operator where
  cast c = case c of
                "^" => (+)
                "w" => (*)
                "hiss" => Pop
                "mew" => Store
                "awa" => Restore
                "purr" => Print
                _   => Nop

data Token : Type where
  Niteral : Integer -> Token
  Op     : Operator -> Token


as_token : String -> Token
as_token str with (parseInteger str)
  as_token str | Nothing = Op (cast str) --parseInteger "+" == Just 0 which does not make any sense to me
  as_token str | (Just n) = Niteral n


parser : List String -> List Token
parser [] = []
parser (str :: strs) = as_token str :: parser strs

tokenizer : String -> List String
tokenizer str = filter (\s => length s /= 0) $ forget (split (=='*') str)

eval_ind : {stack : List Integer} -> {storage : SortedMap Integer Integer} -> {msg : List Char} -> List Token -> Maybe (Integer, List Char)  
eval_ind [] {stack = [i]} = Just (i, msg)
eval_ind ((Niteral n) :: xs) = eval_ind xs {stack = (n :: stack)} {storage = storage} {msg = msg} 
eval_ind ((Op Pop) :: xs) {stack = (i :: is)} = eval_ind xs {stack = is} {storage = storage} {msg = msg}
eval_ind ((Op Store) :: xs) {stack = (i1 :: (i2 :: is))} = eval_ind xs {stack = is} {storage = insert i1 i2 storage} {msg = msg}
eval_ind ((Op Restore) :: xs) {stack = (i1 :: is)} = case lookup i1 storage of
                                                        Nothing => Nothing
                                                        (Just val) => eval_ind xs {stack = val :: is} {storage = delete i1 storage} {msg = msg}
eval_ind ((Op (+)) :: xs) {stack = (i1 :: (i2 :: is))} = eval_ind xs {stack = (i1 + i2) :: is} {storage = storage} {msg = msg}
eval_ind ((Op (*)) :: xs) {stack = (i1 :: (i2 :: is))} = eval_ind xs {stack = (i1 * i2) :: is} {storage = storage} {msg = msg}
eval_ind ((Op Print) :: xs) {stack = (i :: is)} = eval_ind xs {stack = i :: is} {storage = storage} {msg = (cast i) :: msg}
eval_ind ((Op Nop) :: xs) = eval_ind xs {stack = stack} {storage = storage} {msg = msg}
eval_ind _ = Nothing 

eval_str : String -> Maybe (Integer, List Char)
eval_str str = eval_ind {stack = []} {storage = empty} {msg = []} $ parser $ tokenizer str

eval_file : (handle : File) -> IO ()
eval_file handle = do 
  res_io <- fRead handle
  case res_io of
      (Left err) => putStr $ "Something went seriously wrong while reading the file\n" ++ (show err)
      (Right content) => case eval_str content of
                              Nothing => putStr "Eval error"
                              (Just (res, msg)) => putStr $ (show res) ++ "\n" ++ (fastPack msg) 

eval_filename : (filename : String) -> IO ()
eval_filename filename with (length filename == 0)
  eval_filename filename | False = do 
    res_io <- openFile filename Read 
    case res_io of
         (Left err) => putStr $ "Something went wrong while opening the file\n" ++ (show err)
         (Right handle) => eval_file handle
  eval_filename filename | True = eval_file stdin 

main : IO ()
main = do
  args <- getArgs
  case args of
       [] => putStr "empty argv list?"--eval "" 
       (arg0 :: []) => eval_filename ""
       (_ :: arg1 :: args) => eval_filename arg1
