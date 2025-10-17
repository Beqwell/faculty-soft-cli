module Main (main) where

import System.Environment (getArgs)
import App (runApp)

main :: IO ()
main = getArgs >>= runApp
