{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Trans
import Desugar
import System.IO (getContents)
import System.Environment (getArgs)
import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Options.Applicative
import Language.Haskell.Exts.Annotated hiding (parseFile)

data OutputLevel = Ast | Desugared | Expr | Json
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
                          <> help "output-level: Ast, Desugared, Expr, Json")
             <|> pure Json)

main :: IO ()
main =
    do
        Options {file, outputLevel} <- execParser opts
        (name, input) <- case file of
                           Nothing -> (,) <$> return "<interact>" <*> getContents
                           Just x -> (,) <$> return x <*> readFile x
        let parsed = parseFile name input
        let desugared = parsed >>= \m -> return $ runDesugarM (desugar m) emptyDesugarState
        let bindings = desugared >>= transModule
        let program = bindings >>= build
        case outputLevel of
          Ast -> print $ evalTranspiler parsed
          Desugared -> putStrLn . prettyPrint $ evalTranspiler desugared
          Json -> BS.putStrLn . encode . toJSON $ (evalTranspiler program :: Expr ())
          Expr -> mapM_ print $ evalTranspiler bindings

    where opts = info (helper <*> parser)
                    (fullDesc
                    <> progDesc "transforming Haskell programs to lambda calculus programs"
                    <> header "Transpiler")
