{-# LANGUAGE ScopedTypeVariables #-}

-- Code is derived from https://github.com/mpickering/hie-bios/blob/master/src/HIE/Bios/Cradle.hs
-- git commit: 6460ab40709fe5cc6209b2094d32f80d46c889fd
-- Derived code subject to hie-bios's BSD 3-Clause "New" or "Revised" License
-- Hie-bios's license is distributed with the hie-bios dependency
-- Initial differences can be found at https://github.com/mpickering/hie-bios/pull/178

module Hie.Implicit.Cradle
  ( loadImplicitHieCradle,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (handleJust)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Void
import qualified Data.Yaml as Yaml
import GHC.Fingerprint (fingerprintString)
import HIE.Bios.Config
import HIE.Bios.Cradle
import HIE.Bios.Environment (getCacheDir)
import HIE.Bios.Types hiding (ActionName (..))
import qualified HIE.Bios.Types as Types
import HIE.Bios.Wrappers
import Hie.Cabal.Parser
import Hie.Locate
import Hie.Yaml
import System.Directory hiding (findFile)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isPermissionError)
import System.IO.Temp
import System.Info.Extra (isWindows)
import System.PosixCompat.Files
import System.Process

-- | Given root\/foo\/bar.hs, load an implicit cradle
loadImplicitHieCradle :: Show a => FilePath -> IO (Cradle a)
loadImplicitHieCradle wfile = do
  let wdir = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  return $ case cfg of
    Just bc -> getCradle absurd bc
    Nothing -> defaultCradle wdir

implicitConfig :: FilePath -> MaybeT IO (CradleConfig a, FilePath)
implicitConfig fp = do
  (crdType, wdir) <- implicitConfig' fp
  return (CradleConfig [] crdType, wdir)

implicitConfig' :: FilePath -> MaybeT IO (CradleType a, FilePath)
implicitConfig' fp =
  ( \wdir ->
      (Bios (wdir </> ".hie-bios") Nothing, wdir)
  )
    <$> biosWorkDir fp
    --   <|> (Obelisk,) <$> obeliskWorkDir fp
    --   <|> (Bazel,) <$> rulesHaskellWorkDir fp
    <|> (cabalExecutable >> cabalProjectDir fp >> cabalDistDir fp >>= cabal)
    <|> (stackExecutable >> stackYamlDir fp >> stackWorkDir fp >>= stack)
    <|> (cabalExecutable >> cabalProjectDir fp >>= cabal)
    <|> (stackExecutable >> stackYamlDir fp >>= stack)
    <|> (cabalExecutable >> cabalFile fp >>= cabal)
  where
    readPkgs f gp p = do
      cfs <- gp p
      pkgs <- liftIO $ catMaybes <$> mapM (nestedPkg p) cfs
      pure $ concatMap (components f) pkgs
    build cn cc gp p = do
      c <- cn <$> readPkgs cc gp p
      pure (c, p)
    cabal :: FilePath -> MaybeT IO (CradleType a, FilePath)
    cabal = build CabalMulti cabalComponent cabalPkgs
    stack = build StackMulti stackComponent stackYamlPkgs
    components f (Package n cs) = map (f n) cs

------------------------------------------------------------------------
-- Cabal Cradle
-- Works for new-build by invoking `v2-repl` does not support components
-- yet.
cabalCradle :: FilePath -> Maybe String -> Cradle a
cabalCradle wdir mc =
  Cradle
    { cradleRootDir = wdir,
      cradleOptsProg =
        CradleAction
          { actionName = Types.Cabal,
            runCradle = cabalAction wdir mc
          }
    }

cabalCradleDependencies :: FilePath -> IO [FilePath]
cabalCradleDependencies rootDir = do
  cabalFiles <- findCabalFiles rootDir
  return $ cabalFiles ++ ["cabal.project", "cabal.project.local"]

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles wdir = do
  dirContent <- listDirectory wdir
  return $ filter ((== ".cabal") . takeExtension) dirContent

processCabalWrapperArgs :: [String] -> Maybe (FilePath, [String])
processCabalWrapperArgs args =
  case args of
    (dir : ghc_args) ->
      let final_args =
            removeVerbosityOpts
              $ removeRTS
              $ removeInteractive ghc_args
       in Just (dir, final_args)
    _ -> Nothing

-- | GHC process information.
-- Consists of the filepath to the ghc executable and
-- arguments to the executable.
type GhcProc = (FilePath, [String])

-- generate a fake GHC that can be passed to cabal
-- when run with --interactive, it will print out its
-- command-line arguments and exit
withCabalWrapperTool :: GhcProc -> FilePath -> (FilePath -> IO a) -> IO a
withCabalWrapperTool (ghcPath, ghcArgs) wdir k =
  if isWindows
    then do
      cacheDir <- getCacheDir ""
      let srcHash = show (fingerprintString cabalWrapperHs)
      let wrapper_name = "wrapper-" ++ srcHash
      let wrapper_fp = cacheDir </> wrapper_name <.> "exe"
      exists <- doesFileExist wrapper_fp
      unless exists $ withSystemTempDirectory "hie-bios" $ \tmpDir -> do
        createDirectoryIfMissing True cacheDir
        let wrapper_hs = cacheDir </> wrapper_name <.> "hs"
        writeFile wrapper_hs cabalWrapperHs
        let ghc =
              ( proc ghcPath $
                  ghcArgs ++ ["-rtsopts=ignore", "-outputdir", tmpDir, "-o", wrapper_fp, wrapper_hs]
              )
                { cwd = Just wdir
                }
        readCreateProcess ghc "" >>= putStr
      setMode wrapper_fp
      k wrapper_fp
    else
      withSystemTempFile
        "bios-wrapper"
        ( \loc h -> do
            hPutStr h cabalWrapper
            hClose h
            setMode loc
            k loc
        )
  where
    setMode wrapper_fp = setFileMode wrapper_fp accessModes

cabalAction :: FilePath -> Maybe String -> LoggingFunction -> FilePath -> IO (CradleLoadResult ComponentOptions)
cabalAction work_dir mc l fp =
  withCabalWrapperTool ("ghc", []) work_dir $ \wrapper_fp -> do
    let cab_args = ["v2-repl", "--with-compiler", wrapper_fp, fromMaybe (fixTargetPath fp) mc]
    (ex, output, stde, args) <-
      readProcessWithOutputFile l work_dir "cabal" cab_args
    deps <- cabalCradleDependencies work_dir
    case processCabalWrapperArgs args of
      Nothing ->
        pure $
          CradleFail
            ( CradleError
                ex
                [ "Failed to parse result of calling cabal",
                  unlines output,
                  unlines stde,
                  unlines args
                ]
            )
      Just (componentDir, final_args) -> pure $ makeCradleResult (ex, stde, componentDir, final_args) deps
  where
    -- Need to make relative on Windows, due to a Cabal bug with how it
    -- parses file targets with a C: drive in it
    fixTargetPath x
      | isWindows && hasDrive x = makeRelative work_dir x
      | otherwise = x

removeInteractive :: [String] -> [String]
removeInteractive = filter (/= "--interactive")

-- Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
removeRTS :: [String] -> [String]
removeRTS ("+RTS" : xs) =
  case dropWhile (/= "-RTS") xs of
    [] -> []
    (_ : ys) -> removeRTS ys
removeRTS (y : ys) = y : removeRTS ys
removeRTS [] = []

removeVerbosityOpts :: [String] -> [String]
removeVerbosityOpts = filter ((&&) <$> (/= "-v0") <*> (/= "-w"))

cabalExecutable :: MaybeT IO FilePath
cabalExecutable = MaybeT $ findExecutable "cabal"

cabalDistDir :: FilePath -> MaybeT IO FilePath
cabalDistDir = findFileUpwards isCabal
  where
    -- TODO do old style dist builds work?
    isCabal name = name == "dist-newstyle" || name == "dist"

cabalProjectDir :: FilePath -> MaybeT IO FilePath
cabalProjectDir = findFileUpwards isCabal
  where
    isCabal name = name == "cabal.project"

cabalFile :: FilePath -> MaybeT IO FilePath
cabalFile = findFileUpwards isCabal
  where
    isCabal = (".cabal" ==) . takeExtension

------------------------------------------------------------------------
-- Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script

stackCradle :: FilePath -> Maybe String -> Cradle a
stackCradle wdir mc =
  Cradle
    { cradleRootDir = wdir,
      cradleOptsProg =
        CradleAction
          { actionName = Types.Stack,
            runCradle = stackAction wdir mc
          }
    }

stackCradleDependencies :: FilePath -> IO [FilePath]
stackCradleDependencies wdir = do
  cabalFiles <- findCabalFiles wdir
  return $ cabalFiles ++ ["package.yaml", "stack.yaml"]

stackAction :: FilePath -> Maybe String -> LoggingFunction -> FilePath -> IO (CradleLoadResult ComponentOptions)
stackAction work_dir mc l _fp = do
  let ghcProcArgs = ("stack", ["exec", "ghc", "--"])
  -- Same wrapper works as with cabal
  withCabalWrapperTool ghcProcArgs work_dir $ \wrapper_fp -> do
    (ex1, _stdo, stde, args) <-
      readProcessWithOutputFile
        l
        work_dir
        "stack"
        $ ["repl", "--no-nix-pure", "--with-ghc", wrapper_fp]
          ++ catMaybes [mc]
    (ex2, pkg_args, stdr, _) <-
      readProcessWithOutputFile l work_dir "stack" ["path", "--ghc-package-path"]
    let split_pkgs = concatMap splitSearchPath pkg_args
        pkg_ghc_args = concatMap (\p -> ["-package-db", p]) split_pkgs
    deps <- stackCradleDependencies work_dir
    return $ case processCabalWrapperArgs args of
      Nothing ->
        CradleFail
          ( CradleError ex1 $
              ( "Failed to parse result of calling stack"
                  : stde
              )
                ++ args
          )
      Just (componentDir, ghc_args) ->
        makeCradleResult (combineExitCodes [ex1, ex2], stde ++ stdr, componentDir, ghc_args ++ pkg_ghc_args) deps

combineExitCodes :: [ExitCode] -> ExitCode
combineExitCodes = foldr go ExitSuccess
  where
    go ExitSuccess b = b
    go a _ = a

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

stackWorkDir :: FilePath -> MaybeT IO FilePath
stackWorkDir = findFileUpwards isStack
  where
    isStack name = name == ".stack-work"

stackYamlDir :: FilePath -> MaybeT IO FilePath
stackYamlDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"

-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
  cnts <-
    liftIO $
      handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)
  case cnts of
    []
      | dir' == dir -> fail "No cabal files"
      | otherwise -> findFileUpwards p dir'
    _ : _ -> return dir
  where
    dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)
