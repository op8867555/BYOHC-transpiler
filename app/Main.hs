module Main where

import Trans
import System.IO (getContents)
import System.Environment (getArgs)
import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    args <- getArgs
    (name, input) <- case args of
                       [] -> (,) <$> return "<interact>" <*> getContents
                       (x:[]) -> (,) <$> return x <*> readFile x
    bindings <- transpile name input :: IO [(TName, Expr Int)]
    let jsonBinds = map toJSON bindings
    BS.putStrLn . encode $ toJSON jsonBinds


