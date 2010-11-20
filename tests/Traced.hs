module Main ( main ) where

import Debug.Trace
import Control.Exception ( evaluate )

main :: IO ()
main = do
    putStrLn "The test is successful if the word 'Evaluated' appears only once below:"
    evaluate $ let x = trace "Evaluated" (1 + 1) in x + (trace "Evaluated" (1 + 1)) + x
    return ()