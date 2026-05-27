{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)
import Data.List (isSuffixOf, sort)
import GHC
  ( DynFlags(backend, ghcLink, importPaths), GhcLink(NoLink)
  , GhcMonad(getSession, setSession), LoadHowMuch(LoadAllTargets)
  , getSessionDynFlags, guessTarget, load, runGhc, setSessionDynFlags
  , setTargets
  )
import GHC.Driver.Backend (noBackend)
import GHC.Driver.Env (HscEnv(hsc_plugins))
import GHC.Driver.Plugins
  ( StaticPlugin(StaticPlugin, spInitialised, spPlugin)
  , PluginWithArgs(PluginWithArgs)
  , Plugins(staticPlugins)
  )
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath (takeDirectory, (<.>), (</>), takeBaseName, takeFileName)
import System.Process (readProcess)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import qualified OM.Plugin.Imports as Plugin

main :: IO ()
main = do
  libdir <- initGhc
  testFiles <- findTestFiles
  defaultMain
    ( testGroup
        "Tests"
        [ testGroup "Golden Tests" (mkTest libdir <$> testFiles)
        , mkInPlaceTest libdir "tests/samples/Basic.hs"
        ]
    )


initGhc :: IO FilePath
initGhc = do
  out <- readProcess "ghc" ["--print-libdir"] ""
  return (init out)


findTestFiles :: IO [FilePath]
findTestFiles = do
  files <- listDirectory "tests/samples"
  return $ sort
    [ "tests/samples" </> f
    | f <- files
    , ".hs" `isSuffixOf` f
    , f `notElem`
        [ "PatternSynonymDef.hs"
        , "AmbiguousA.hs"
        , "AmbiguousB.hs"
        ]
    ]


mkTest :: FilePath -> FilePath -> TestTree
mkTest libdir srcFile =
  goldenVsFileDiff
    (takeBaseName srcFile)
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    outFile
    (runGhcPlugin libdir srcFile)
  where
    goldenFile = "tests/golden" </> (takeBaseName srcFile <.> "full-imports")
    outFile = srcFile <.> "full-imports"


mkInPlaceTest :: FilePath -> FilePath -> TestTree
mkInPlaceTest libdir srcFile =
  goldenVsFileDiff
    (takeBaseName srcFile <> " in-place")
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    outFile
    (runInPlaceGhcPlugin libdir srcFile outFile)
  where
    goldenFile = "tests/golden" </> (takeBaseName srcFile <.> "in-place.hs")
    outDir = "tests/tmp"
    outFile = outDir </> takeFileName srcFile


runGhcPlugin :: FilePath -> FilePath -> IO ()
runGhcPlugin libdir =
  runGhcPluginWithArgs libdir []


runInPlaceGhcPlugin :: FilePath -> FilePath -> FilePath -> IO ()
runInPlaceGhcPlugin libdir srcFile outFile = do
    createDirectoryIfMissing True (takeDirectory outFile)
    copyFile srcFile outFile
    runGhcPluginWithArgs libdir ["in-place"] outFile


runGhcPluginWithArgs :: FilePath -> [String] -> FilePath -> IO ()
runGhcPluginWithArgs libdir pluginArgs srcFile = do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let
          dflags_ :: DynFlags
          dflags_ = dflags
            { importPaths = [takeDirectory srcFile, "tests/samples"]
            , backend = noBackend
            , ghcLink = NoLink
            }
        
        env <- getSession
        let
          plugins :: Plugins
          plugins = hsc_plugins env

          staticPlugin :: StaticPlugin
          staticPlugin = StaticPlugin
            { spPlugin = PluginWithArgs Plugin.plugin pluginArgs
            , spInitialised = False
            }

          env_ :: HscEnv
          env_ = env { hsc_plugins = plugins { staticPlugins = staticPlugin : staticPlugins plugins } }
        setSession env_
        
        void $ setSessionDynFlags dflags_
        
        target <- guessTarget srcFile Nothing Nothing
        setTargets [target]
        void $ load LoadAllTargets
