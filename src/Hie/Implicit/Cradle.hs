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
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Void
import HIE.Bios.Config hiding (cabalComponent, stackComponent)
import HIE.Bios.Cradle
import HIE.Bios.Types hiding (ActionName (..))
import Hie.Cabal.Parser
import Hie.Locate
import Hie.Yaml
import System.Directory hiding (findFile)
import System.FilePath
import System.IO.Error (isPermissionError)
import System.Process (readProcess)

-- | Given root\/foo\/bar.hs, load an implicit cradle
loadImplicitHieCradle :: FilePath -> IO (Cradle a)
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
      (Bios (Program $ wdir </> ".hie-bios") Nothing Nothing, wdir)
  )
    <$> biosWorkDir fp
    --   <|> (Obelisk,) <$> obeliskWorkDir fp
    --   <|> (Bazel,) <$> rulesHaskellWorkDir fp
    <|> (cabalExecutable >> cabalProjectDir fp >> cabalDistDir fp >>= cabal)
    <|> (stackExecutable >> stackYamlDir fp >> stackWorkDir fp >>= stack)
    <|> (cabalExecutable >> (cabalProjectDir fp <|> cabalDistDir fp) >>= cabal)
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
    cabal fp = ifM ((>= "3.4") <$> liftIO cabalVersion) (pure (Cabal mempty, fp)) (build (CabalMulti mempty) cabalComponent' cabalPkgs fp)
    stack :: FilePath -> MaybeT IO (CradleType a, FilePath)
    stack = build (StackMulti mempty) stackComponent' stackYamlPkgs
    components f (Package n cs) = map (f n) cs

    cabalComponent' n c = CabalType . Just <$> cabalComponent n c
    stackComponent' n c = flip StackType Nothing . Just <$> stackComponent n c

------------------------------------------------------------------------

cabalExecutable :: MaybeT IO FilePath
cabalExecutable = MaybeT $ findExecutable "cabal"

cabalDistDir :: FilePath -> MaybeT IO FilePath
cabalDistDir = findSubdirUpwards isCabal
  where
    -- TODO do old style dist builds work?
    isCabal name = name == "dist-newstyle" || name == "dist"

cabalProjectDir :: FilePath -> MaybeT IO FilePath
cabalProjectDir = findFileUpwards isCabal
  where
    isCabal "cabal.project" = True
    isCabal "cabal.project.local" = True
    isCabal _ = False

cabalFile :: FilePath -> MaybeT IO FilePath
cabalFile = findFileUpwards isCabal
  where
    isCabal = (".cabal" ==) . takeExtension

------------------------------------------------------------------------
-- Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

stackWorkDir :: FilePath -> MaybeT IO FilePath
stackWorkDir = findSubdirUpwards isStack
  where
    isStack name = name == ".stack-work"

stackYamlDir :: FilePath -> MaybeT IO FilePath
stackYamlDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"

-- | Searches upwards for the first directory containing a subdirectory
-- to match the predicate.
findSubdirUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findSubdirUpwards p dir = findContentUpwards p' dir
  where
    p' subdir = do
      exists <- doesDirectoryExist $ dir </> subdir
      return $ (p subdir) && exists

-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = findContentUpwards p' dir
  where
    p' file = do
      exists <- doesFileExist $ dir </> file
      return $ (p file) && exists

findContentUpwards :: (FilePath -> IO Bool) -> FilePath -> MaybeT IO FilePath
findContentUpwards p dir = do
  cnts <-
    liftIO $
      handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findContent p dir)
  case cnts of
    []
      | dir' == dir -> fail "No cabal files"
      | otherwise -> findContentUpwards p dir'
    _ : _ -> return dir
  where
    dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findContent :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
findContent p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles else pure []
  where
    getFiles = getDirectoryContents dir >>= filterM p

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

cabalVersion :: IO String
cabalVersion = head . words <$> readProcess "cabal" ["--numeric-version"] ""
