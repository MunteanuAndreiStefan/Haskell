import System.Environment
import Data.Char

uppercase :: String -> String
uppercase = map (\c -> if c >= 'a' && c <= 'z' 
                            then toEnum (fromEnum c - 32) 
                       else c)

main = do
    args <- getArgs
    contents <- readFile (head args) 
    putStr (interpret contents)