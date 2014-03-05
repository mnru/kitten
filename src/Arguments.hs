module Arguments
  ( parseArguments
  ) where

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

data CompileMode
  = CheckMode
  | CompileMode OutputFormat
  | InterpretMode
  | HTMLMode

data OutputFormat
  = C
  | Yarn

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
    [ flagReq' ["c", "compile"] "c|yarn"
      "Compile to specified output format."
      $ \format acc@Arguments{..} -> case format of
        "c" -> Right acc { argsCompileMode = CompileMode C }
        "yarn" -> Right acc { argsCompileMode = CompileMode Yarn }
        _ -> Left $ concat
          [ "Unknown output format '"
          , format
          , "'."
          ]

    , flagBool' ["check"]
      "Check syntax and types without compiling or running."
      $ \flag acc@Arguments{..} -> acc
      { argsCompileMode = if flag then CheckMode else argsCompileMode }

    , flagBool' ["dump-resolved"]
      "Output result of name resolution."
      $ \flag acc@Arguments{..} -> acc
      { argsDumpResolved = flag }

    , flagBool' ["dump-scoped"]
      "Output result of scope resolution."
      $ \flag acc@Arguments{..} -> acc
      { argsDumpScoped = flag }

    , flagBool' ["html"]
      "Output an HTML document for viewing the type-checked source code."
      $ \flag acc@Arguments{..} -> acc
      { argsCompileMode = if flag then HTMLMode else argsCompileMode }

    , flagReq' ["L", "library"] "DIR"
      "Add library search directory."
      $ \path acc@Arguments{..} -> Right $ acc
      { argsLibraryDirectories = path : argsLibraryDirectories }

    , flagReq' ["o", "output"] "FILE"
      "Set output path."
      $ \path acc@Arguments{..} -> case argsOutputPath of
        Just existing -> Left $ unlines
          [ "Multiple output paths specified:"
          , existing
          , "And:"
          , path
          ]
        Nothing -> Right acc { argsOutputPath = Just path }

    , flagBool' ["no-implicit-prelude"]
      "Disable implicit inclusion of prelude."
      $ \flag acc@Arguments{..} -> acc
      { argsEnableImplicitPrelude = not flag }

    , flagHelpSimple $ \acc -> acc { argsShowHelp = True }
    , flagVersion $ \acc -> acc { argsShowVersion = True }
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
