{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified OM.Plugin.Imports as Plugin
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import System.FilePath ((</>), takeBaseName, (<.>))
import System.Directory (listDirectory)
import Data.List (isSuffixOf, sort)
import GHC
  ( runGhc, getSessionDynFlags, setSessionDynFlags, getSession, setSession
  , guessTarget, setTargets, load, LoadHowMuch(LoadAllTargets)
  , DynFlags(backend, ghcLink, importPaths), GhcLink(NoLink)
  )
import GHC.Driver.Env (HscEnv(hsc_plugins))
import GHC.Driver.Plugins
  ( StaticPlugin(StaticPlugin, spInitialised, spPlugin)
  , PluginWithArgs(PluginWithArgs)
  , Plugins(staticPlugins)
  )
import GHC.Driver.Backend (noBackend)
import Control.Monad (void)
import System.Process (readProcess)

main :: IO ()
main = do
  libdir <- initGhc
  testFiles <- findTestFiles
  defaultMain (testGroup "Golden Tests" (mkTest libdir <$> testFiles))

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

runGhcPlugin :: FilePath -> FilePath -> IO ()
runGhcPlugin libdir srcFile = do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags
                { importPaths = ["tests/samples"]
                , backend = noBackend
                , ghcLink = NoLink
                }
        
        env <- getSession
        let plugins = hsc_plugins env
            staticPlugin = StaticPlugin
                { spPlugin = PluginWithArgs Plugin.plugin []
                , spInitialised = False
                }
            env' = env { hsc_plugins = plugins { staticPlugins = staticPlugin : staticPlugins plugins } }
        setSession env'
        
        void $ setSessionDynFlags dflags'
        
        target <- guessTarget srcFile Nothing Nothing
        setTargets [target]
        void $ load LoadAllTargets
