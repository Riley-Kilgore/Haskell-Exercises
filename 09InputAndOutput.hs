{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import System.Environment
import System.Random
import Data.Char
import Data.List

main = do
       messages <- getArgs
       let message = intercalate " " messages
       putStrLn message

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = lottery' gen []

lottery' :: StdGen -> [Int] -> [Int]
lottery' gen xs
         | length xs >= 6 = xs
         | otherwise =
         if num `elem` xs
            then lottery' newGen xs
            else lottery' newGen (num:xs)
         where (num, newGen) = randomR (1, 49) gens