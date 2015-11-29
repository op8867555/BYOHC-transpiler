{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Trans
import System.IO (getContents)
import System.Environment (getArgs)
import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Options.Applicative

data OutputLevel = Ast | Expr | Json
                 deriving (Show, Read)

data Options =
    Options { file :: Maybe String
            , outputLevel :: OutputLevel
            } deriving (Show)


parser :: Parser Options
parser =
    Options
        <$> optional (strOption (long "file"
                                 <> metavar "FILE")
                     <|> (argument str (metavar "FILE")))
        <*> (option auto (long "output-level"
                          <> help "output-level: Ast, Expr, Json")
             <|> pure Json)

main :: IO ()
main =
    do
        Options {file, outputLevel} <- execParser opts
        (name, input) <- case file of
                           Nothing -> (,) <$> return "<interact>" <*> getContents
                           Just x -> (,) <$> return x <*> readFile x
        bindings <- transpile name input :: IO [(TName, Expr ())]
        case outputLevel of
          Json -> BS.putStrLn . encode . toJSON $ map toJSON bindings
          Expr -> mapM_ print bindings
          _ -> error "not implemented yet"

    where opts = info (helper <*> parser)
                    (fullDesc
                    <> progDesc "transforming Haskell programs to lambda calculus programs"
                    <> header "Transpiler")
