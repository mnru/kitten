{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (bracket)
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.Console.CmdArgs.Explicit
import System.Exit
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import Arguments
import Kitten.C
import Kitten.Compile (compile, locateImport)
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Name (NameGen, mkNameGen)
import Kitten.Typed (Typed)
import Kitten.Util.Text (toText)
import Kitten.Yarn (yarn)

import qualified Kitten.Compile as Compile
import qualified Kitten.HTML as HTML
import qualified Kitten.Infer.Config as Infer
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.Text as T

main :: IO ()
main = do
  arguments <- parseArguments
  let
    defaultConfig prelude filename program = Compile.Config
      { Compile.dumpResolved = argsDumpResolved arguments
      , Compile.dumpScoped = argsDumpScoped arguments
      , Compile.firstLine = 1
      , Compile.inferConfig = Infer.Config
        { Infer.enforceBottom = True }
      , Compile.libraryDirectories = argsLibraryDirectories arguments
      , Compile.name = filename
      , Compile.prelude = prelude
      , Compile.source = program
      , Compile.stackTypes = V.empty
      }

  preludes <- locateImport
    (argsLibraryDirectories arguments)
    "Prelude"

  let nameGen = mkNameGen  -- TODO(strager): Use StateT NameGen.

  (nameGen', prelude) <- if not (argsEnableImplicitPrelude arguments)
    then return (nameGen, mempty)
    else case preludes of

    [] -> do
      hPutStrLn stderr "No module 'Prelude' found."
      exitFailure

    [filename] -> do
      source <- T.readFileUtf8 filename
      mPrelude <- compile (defaultConfig mempty filename source) nameGen

      (nameGen', Fragment{..}) <- case mPrelude of
        Left compileErrors -> do
          printCompileErrors compileErrors
          exitFailure
        Right (nameGen', prelude, _type)
          -> return (nameGen', prelude)

      when (V.any containsCode fragmentTerms) $ do
        print fragmentTerms
        hPutStrLn stderr "Prelude includes executable code."
        exitFailure

      return (nameGen', mempty { fragmentDefs = fragmentDefs })

    _ -> do
      hPutStrLn stderr . unlines
        $ "Too many Prelude candidates:"
        : preludes
      exitFailure

  case argsEntryPoints arguments of
    [] -> return ()  -- runRepl prelude nameGen'
    entryPoints -> interpretAll entryPoints
      (argsCompileMode arguments)
      (argsOutputPath arguments)
      prelude
      (defaultConfig prelude)
      nameGen

containsCode :: Typed -> Bool
containsCode (Typed.Compose terms _ _) = V.any containsCode terms
containsCode _ = True

interpretAll
  :: [FilePath]
  -> CompileMode
  -> Maybe FilePath
  -> Fragment Typed
  -> (FilePath -> Text -> Compile.Config)
  -> NameGen
  -> IO ()
interpretAll entryPoints compileMode outputPath prelude config nameGen
  = mapM_ interpretOne entryPoints

  where
  output :: Vector Text -> IO ()
  output = case outputPath of
    Nothing -> V.mapM_ (hPutStrLn stdout . T.unpack)
    Just path
      -> \items -> bracket (openFile path WriteMode) hClose
      $ \handle -> V.mapM_ (hPutStrLn handle . T.unpack) items

  interpretOne
    :: FilePath
    -> IO ()
  interpretOne filename = do
    source <- T.readFileUtf8 filename
    mResult <- compile (config filename source) nameGen
    case mResult of
      Left compileErrors -> do
        printCompileErrors compileErrors
        exitFailure
      Right (_nameGen', result, _type) -> case compileMode of
        CheckMode -> return ()
        CompileMode format
          -> let yarned = yarn (prelude <> result)
          in output $ case format of
            C -> toC yarned
            Yarn -> V.map toText yarned
        InterpretMode -> void $ interpret [] $ yarn (prelude <> result)
        HTMLMode -> do
          html <- HTML.fromFragmentsM T.readFileUtf8 [prelude, result]
          T.putStrLn html
