{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)
import GHC (guessTarget, runGhc, setSessionDynFlags, setTargets, load, getSessionDynFlags, DynFlags(backend, ghcLink, importPaths), GhcLink(NoLink), LoadHowMuch(LoadAllTargets), GhcMonad(getSession, setSession))
import GHC.Driver.Backend (noBackend)
import GHC.Driver.Env (HscEnv(hsc_plugins))
import GHC.Driver.Plugins (PluginWithArgs(PluginWithArgs), Plugins(staticPlugins), StaticPlugin(StaticPlugin, spInitialised, spPlugin))
import Prelude (($), Monad(return), Semigroup((<>)), Bool(False, True), String, Maybe(Nothing, Just), IO, init, FilePath)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((<.>), (</>), takeBaseName, takeDirectory, takeFileName)
import System.Process (readProcess)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)
import qualified OM.Plugin.Imports as Plugin

main :: IO ()
main = do
  libdir <- initGhc
  defaultMain
    ( testGroup
        "Tests"
        [ testGroup
            "Golden Tests"
            [ mkTest libdir "tests/samples/AmbiguousUser.hs"
            , mkTest libdir "tests/samples/Basic.hs"
            , mkTest libdir "tests/samples/EnumPatImport.hs"
            , mkTest libdir "tests/samples/LongImport.hs"
            , mkTest libdir "tests/samples/PatternSynonym.hs"
            , mkTest libdir "tests/samples/Qualified.hs"
            ]
        , testGroup
            "In-Place Tests"
            [ mkInPlaceTest libdir "tests/samples/Basic.hs"
            , mkInPlaceTest libdir "tests/samples/LongImport.hs"
            , mkInPlaceTest libdir "tests/samples/PatternSynonym.hs"
            , mkDoubleInPlaceTest libdir "tests/samples/PatternSynonym.hs"
            , mkInPlaceTest libdir "tests/samples/EnumPatImport.hs"
            , mkDoubleInPlaceTest libdir "tests/samples/EnumPatImport.hs"
            , mkInPlaceTest libdir "tests/samples/OrphanInstanceImport.hs"
            ]
        ]
    )


initGhc :: IO FilePath
initGhc = do
  out <- readProcess "ghc" ["--print-libdir"] ""
  return (init out)


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


mkDoubleInPlaceTest :: FilePath -> FilePath -> TestTree
mkDoubleInPlaceTest libdir srcFile =
  goldenVsFileDiff
    (takeBaseName srcFile <> " double in-place")
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    outFile
    (runDoubleInPlaceGhcPlugin libdir srcFile outFile)
  where
    goldenFile =
      "tests/golden" </> (takeBaseName srcFile <.> "double-in-place.hs")
    outDir = "tests/tmp"
    outFile = outDir </> (takeBaseName srcFile <> "-double" <.> "hs")


runDoubleInPlaceGhcPlugin :: FilePath -> FilePath -> FilePath -> IO ()
runDoubleInPlaceGhcPlugin libdir srcFile outFile = do
  createDirectoryIfMissing True (takeDirectory outFile)
  copyFile srcFile outFile
  runGhcPluginWithArgs libdir ["in-place"] outFile
  runGhcPluginWithArgs libdir ["in-place"] outFile


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
