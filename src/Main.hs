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
import qualified Data.Vector as V

import Kitten.Compile (compile, locateImport)
import Kitten.Error
import Kitten.Fragment
import Kitten.Interpret
import Kitten.Resolved (Resolved, Value)
import Kitten.Util.Text (toText)
import Kitten.X86_64
import Kitten.Yarn (yarn)
import Repl

import qualified Kitten.Compile as Compile

data AssemblyFormat
  = X86_64
  | Yarn

data CompileMode
  = CompileMode AssemblyFormat
  | InterpretMode

data Arguments = Arguments
  { argsCompileMode :: CompileMode
  , argsDumpResolved :: Bool
  , argsDumpScoped :: Bool
  , argsEnableImplicitPrelude :: Bool
  , argsEntryPoints :: [FilePath]
  , argsLibraryDirectories :: [FilePath]
  , argsOutputPath :: Maybe FilePath
  , argsShowHelp :: Bool
  , argsShowVersion :: Bool
  }

main :: IO ()
main = do

  arguments <- parseArguments
  preludes <- locateImport
    (argsLibraryDirectories arguments)
    "Prelude"

  prelude <- if not (argsEnableImplicitPrelude arguments)
    then return mempty
    else case preludes of

    [] -> do
      hPutStrLn stderr "No module 'Prelude' found."
      exitFailure

    [filename] -> do
      source <- readFile filename
      mPrelude <- compile Compile.Config
        { Compile.dumpResolved = argsDumpResolved arguments
        , Compile.dumpScoped = argsDumpScoped arguments
        , Compile.libraryDirectories
          = argsLibraryDirectories arguments
        , Compile.name = filename
        , Compile.prelude = mempty
        , Compile.source = source
        , Compile.stack = []
        }

      Fragment{..} <- case mPrelude of
        Left compileErrors -> do
          printCompileErrors compileErrors
          exitFailure
        Right prelude -> return prelude

      unless (V.null fragmentTerms) $ do
        hPutStrLn stderr "Prelude includes executable code."
        exitFailure

      return mempty { fragmentDefs = fragmentDefs }

    _ -> do
      hPutStrLn stderr . unlines
        $ "Too many Prelude candidates:"
        : preludes
      exitFailure

  case argsEntryPoints arguments of
    [] -> runRepl prelude
    entryPoints -> interpretAll entryPoints
      (argsCompileMode arguments)
      (argsOutputPath arguments)
      prelude
      $ \ filename program -> Compile.Config
      { Compile.dumpResolved = argsDumpResolved arguments
      , Compile.dumpScoped = argsDumpScoped arguments
      , Compile.libraryDirectories = argsLibraryDirectories arguments
      , Compile.name = filename
      , Compile.prelude = prelude
      , Compile.source = program
      , Compile.stack = []
      }

interpretAll
  :: [FilePath]
  -> CompileMode
  -> Maybe FilePath
  -> Fragment Value Resolved
  -> (FilePath -> String -> Compile.Config)
  -> IO ()
interpretAll entryPoints compileMode outputPath prelude config
  = mapM_ interpretOne entryPoints

  where
  output :: Vector Text -> IO ()
  output = case outputPath of
    Nothing -> V.mapM_ (hPutStrLn stdout . T.unpack)
    Just path
      -> \ items -> bracket (openFile path WriteMode) hClose
      $ \ handle -> V.mapM_ (hPutStrLn handle . T.unpack) items

  interpretOne
    :: FilePath
    -> IO ()
  interpretOne filename = do
    program <- readFile filename
    mResult <- compile (config filename program)
    case mResult of
      Left compileErrors -> printCompileErrors compileErrors
      Right result -> case compileMode of
        CompileMode format
          -> let yarned = yarn (prelude <> result)
          in output $ case format of
            X86_64 -> V.map x86_64 yarned
            Yarn -> V.map toText yarned
        InterpretMode -> void $ interpret [] prelude result

parseArguments :: IO Arguments
parseArguments = do
  arguments <- processArgs argumentsMode

  when (argsShowVersion arguments) $ do
    putStrLn "Kitten version 1.0"
    exitSuccess

  when (argsShowHelp arguments) $ do
    print $ helpText [] HelpFormatDefault argumentsMode
    exitSuccess

  return arguments

argumentsMode :: Mode Arguments
argumentsMode = mode "kitten" defaultArguments
  "Interprets Kitten code." bareArgument options
  where

  defaultArguments :: Arguments
  defaultArguments = Arguments
    { argsCompileMode = InterpretMode
    , argsDumpResolved = False
    , argsDumpScoped = False
    , argsEnableImplicitPrelude = True
    , argsEntryPoints = []
    , argsLibraryDirectories = []
    , argsOutputPath = Nothing
    , argsShowHelp = False
    , argsShowVersion = False
    }

  bareArgument :: Arg Arguments
  bareArgument = flagArg entryPointArgument "entry-point"

  entryPointArgument
    :: FilePath -> Arguments -> Either e Arguments
  entryPointArgument path acc = Right
    $ acc { argsEntryPoints = path : argsEntryPoints acc }

  options :: [Flag Arguments]
  options =
    [ flagOpt' ["c", "compile"] "x86_64|yarn"
      "Compile to specified assembly format."
      "x86_64"
      $ \ format acc@Arguments{..} -> case format of
        "x86_64" -> Right acc { argsCompileMode = CompileMode X86_64 }
        "yarn" -> Right acc { argsCompileMode = CompileMode Yarn }
        _ -> Left $ concat
          [ "Unknown assembly format '"
          , format
          , "'."
          ]

    , flagBool' ["dump-resolved"]
      "Output result of name resolution."
      $ \ flag acc@Arguments{..} -> acc
      { argsDumpResolved = flag }

    , flagBool' ["dump-scoped"]
      "Output result of scope resolution."
      $ \ flag acc@Arguments{..} -> acc
      { argsDumpScoped = flag }

    , flagReq' ["L", "library"] "DIR"
      "Add library search directory."
      $ \ path acc@Arguments{..} -> Right acc
      { argsLibraryDirectories = path : argsLibraryDirectories }

    , flagReq' ["o", "output"] "FILE"
      "Set output path."
      $ \ path acc@Arguments{..} -> case argsOutputPath of
        Just existing -> Left $ unlines
          [ "Multiple output paths specified:"
          , existing
          , "And:"
          , path
          ]
        Nothing -> Right acc { argsOutputPath = Just path }

    , flagBool' ["no-implicit-prelude"]
      "Disable implicit inclusion of prelude."
      $ \ flag acc@Arguments{..} -> acc
      { argsEnableImplicitPrelude = not flag }

    , flagHelpSimple $ \ acc -> acc { argsShowHelp = True }
    , flagVersion $ \ acc -> acc { argsShowVersion = True }
    ]

flagBool'
  :: [Name]
  -> Help
  -> (Bool -> a -> a)
  -> Flag a
flagBool' names description option
  = flagBool names option description

flagOpt'
  :: [Name]
  -> FlagHelp
  -> Help
  -> String
  -> Update a
  -> Flag a
flagOpt' names sample description default_ option
  = flagOpt default_ names option sample description

flagReq'
  :: [Name]
  -> FlagHelp
  -> Help
  -> Update a
  -> Flag a
flagReq' names sample description option
  = flagReq names option sample description
