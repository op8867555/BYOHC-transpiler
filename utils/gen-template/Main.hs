{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import Language.Haskell.TH


main :: IO ()
main = putStrLn $(run)
