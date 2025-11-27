{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- | Description: Plugin for helping close open imports. -}
module OM.Plugin.Imports (
  plugin,
) where


import Control.Monad (void)
import Data.IORef (readIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set, member)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC (ModSummary(ms_hspp_file), DynFlags, ModuleName, Name, moduleName)
import GHC.Types.TyThing (TyThing(AConLike))
import GHC.Core.ConLike (ConLike(PatSynCon))
import GHC.Data.Bag (bagToList)
import GHC.Plugins
  ( GlobalRdrEltX(GRE, gre_imp, gre_name, gre_par), HasDynFlags(getDynFlags)
  , ImpDeclSpec(ImpDeclSpec, is_as, is_mod, is_qual), ImportSpec(is_decl)
  , Outputable(ppr), Parent(NoParent, ParentIs)
  , Plugin(pluginRecompile, typeCheckResultAction)
  , PluginRecompile(NoForceRecompile), CommandLineOption, GlobalRdrElt
  , bestImport, defaultPlugin, liftIO, nonDetOccEnvElts, showSDoc
  )
import GHC.Tc.Utils.Env (tcLookupGlobal)
import GHC.Tc.Utils.Monad
  ( ImportAvails(imp_mods), TcGblEnv(tcg_imports, tcg_used_gres), MonadIO, TcM
  , recoverM
  )
import GHC.Unit.Module.Imported
  ( ImportedBy(ImportedByUser), ImportedModsVal(imv_all_exports)
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Eq((==)), Foldable(elem)
  , Maybe(Just, Nothing), Monoid(mempty), Num((+)), Ord((>)), Semigroup((<>))
  , Show(show), ($), (.), (<$>), (||), FilePath, Int, String, concat, otherwise
  , putStr, putStrLn, unlines, writeFile, mapM
  )
import Safe (headMay)
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set


plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = typeCheckResultActionImpl
  , pluginRecompile = \_ -> pure NoForceRecompile
  }


newtype Options = Options
  { excessive :: Bool
  }


typeCheckResultActionImpl
  :: [CommandLineOption]
  -> ModSummary
  -> TcGblEnv
  -> TcM TcGblEnv
typeCheckResultActionImpl args modSummary env = do
  startTime <- liftIO getCurrentTime
  liftIO . putStr $
    "Generating imports for file: "
    <> ms_hspp_file modSummary
    <> "..."
  let options = parseOptions args
  used <- getUsedImports env
  flags <- getDynFlags
  void $ writeToDumpFile options (ms_hspp_file modSummary) flags used
  endTime <- liftIO getCurrentTime
  liftIO (putStrLn (" in " <> show (diffUTCTime endTime startTime)))
  pure env


writeToDumpFile
  :: (MonadIO m)
  => Options
  -> FilePath
  -> DynFlags
  -> Map ModuleImport (Map WrappedName (Set WrappedName))
  -> m (Maybe FilePath)
writeToDumpFile options srcFile flags used =
  liftIO $ do
    let
      filename :: FilePath
      filename = srcFile <> ".full-imports"
    writeFile filename (renderNewImports options flags used)
    pure (Just filename)


getUsedImports
  :: TcGblEnv
  -> TcM (Map ModuleImport (Map WrappedName (Set WrappedName)))
getUsedImports env = do
  rawUsed <- (liftIO . readIORef) (tcg_used_gres env) :: TcM [GlobalRdrElt]
  let
    {-
      Sometimes, the module from which the name is imported may not
      export the Parent of the name. E.g. Data.List exports 'foldl',
      but not 'Foldable'. So we check to see if the parent is available
      from the module. If it isn't then we just omit the parent. If it
      is, we include the parent with the justification that it provides
      more explicit information to the reader.
    -}
    availableParents :: Map ModuleName (Set Name)
    availableParents =
      Map.unionsWith
        Set.union
        [ Map.singleton
            (moduleName m)
            (Set.singleton name)
        | (m, ibs)
            <- Map.toList . imp_mods . tcg_imports $ env
        , ImportedByUser imv <- ibs
        , GRE { gre_name = name } <-
            concat
            . nonDetOccEnvElts
            . imv_all_exports
            $ imv
        ]

  usedList <-
    let
      mkImport :: GlobalRdrElt -> TcM (ModuleImport, Map WrappedName (Set WrappedName))
      mkImport GRE { gre_name = name, gre_par = parent, gre_imp = imps } = do
        -- Check if the name is a pattern synonym
        isPat <- isPatternSynonym name
        let wrappedName = WrappedName name isPat

        let
          imp :: ImportSpec
          imp = bestImport (NonEmpty.fromList (bagToList imps))

          modName :: ModuleName
          modImport :: ModuleImport
          (modImport, modName) =
            let
              ImpDeclSpec
                  { is_mod = moduleName -> is_mod
                  , is_as
                  , is_qual
                  }
                = is_decl imp
            in
              ( case (is_qual, is_as == is_mod) of
                  (True, True)   -> Qualified is_mod
                  (True, False)  -> QualifiedAs is_mod is_as
                  (False, True)  -> Unqualified is_mod
                  (False, False) -> UnqualifiedAs is_mod is_as
              , is_mod
              )

          {-
            Figure out if we need to omit the parent name because
            it isn't exported by the module from which the name
            itself is imported.
          -}
          withPossibleParent :: Name -> Map WrappedName (Set WrappedName)
          withPossibleParent parentName =
            if
              Set.member parentName $
                Map.findWithDefault
                  mempty
                  modName
                  availableParents
            then
              -- For parents, we assume they are not pattern synonyms (usually types/classes).
              -- Even if they are, 'pattern' keyword logic for parent might differ.
              -- But parent names in imports usually don't take 'pattern'.
              -- (e.g. import M (T(..))). T is parent.
              Map.singleton
                (WrappedName parentName False)
                (Set.singleton wrappedName)
            else
              noParent

          noParent :: Map WrappedName (Set WrappedName)
          noParent = Map.singleton wrappedName mempty

        pure
          ( modImport
          , case parent of
              NoParent -> noParent
              ParentIs parentName -> withPossibleParent parentName
          )
    in
      mapM mkImport rawUsed

  pure $ Map.unionsWith (Map.unionWith Set.union) [Map.singleton k v | (k, v) <- usedList]


isPatternSynonym :: Name -> TcM Bool
isPatternSynonym name = do
  maybeThing <- recoverM (pure Nothing) (Just <$> tcLookupGlobal name)
  pure $ case maybeThing of
    Just (AConLike (PatSynCon _)) -> True
    _ -> False



data ModuleImport
  = Unqualified
      { name :: ModuleName
      }
  | UnqualifiedAs
      { name :: ModuleName
      , as :: ModuleName
      }
  | Qualified
      { name :: ModuleName
      }
  | QualifiedAs
      { name :: ModuleName
      ,   as :: ModuleName
      }
  deriving stock (Eq, Ord)


data WrappedName = WrappedName Name Bool
  deriving stock (Eq, Ord)


instance Outputable WrappedName where
  ppr (WrappedName n _) = ppr n


renderNewImports
  :: Options
  -> DynFlags
  -> Map ModuleImport (Map WrappedName (Set WrappedName))
  -> String
renderNewImports options flags used =
    unlines
      [ case modImport of
          Unqualified { name } ->
            "import " <> shown name <> " (" <> showParents parents <> ")"
          UnqualifiedAs { name, as } ->
            "import " <> shown name <> " as "
            <> shown as <> " (" <> showParents parents <> ")"
          Qualified { name } ->
            "import qualified " <> shown name
            <> maybeShowList name parents
          QualifiedAs { name, as } ->
            "import qualified "
            <> shown name <> " as " <> shown as
            <> maybeShowList as parents
      | (modImport, parents) <- Map.toAscList used
      ]
  where
    maybeShowList :: ModuleName -> Map WrappedName (Set WrappedName) -> String
    maybeShowList modName parents =
      if options.excessive || modName `member` ambiguousNames
        then " (" <> showParents parents <> ")"
        else ""

    ambiguousNames :: Set ModuleName
    ambiguousNames =
      Map.keysSet
      . Map.filter (> 1)
      . Map.unionsWith (+)
      $ [ case modImport of
            Unqualified { name } -> Map.singleton name (1 :: Int)
            UnqualifiedAs {as} -> Map.singleton as 1
            Qualified { name } -> Map.singleton name 1
            QualifiedAs { as } -> Map.singleton as 1
        | (modImport, _) <- Map.toAscList used
        ]

    showParents :: Map WrappedName (Set WrappedName) -> String
    showParents parents =
      intercalate ", "
        [ shownWrapped parent <> showChildren children
        | (parent, children) <- Map.toList parents
        ]

    showChildren :: Set WrappedName -> String
    showChildren children =
      if Set.null children then
        ""
      else
        "(" <> intercalate ", " (shownWrapped <$> Set.toAscList children) <> ")"

    shownWrapped :: WrappedName -> String
    shownWrapped (WrappedName name isPat) =
      let s = shown name in
      if isPat then "pattern " <> s else s

    shown :: Outputable o => o -> String
    shown = fixInlineName . showSDoc flags . ppr

    fixInlineName :: String -> String
    fixInlineName name =
      case headMay name of
        Nothing -> name
        Just c
          | Char.isAlphaNum c || c == '_' -> name
          | otherwise -> "(" <> name <> ")"


parseOptions :: [CommandLineOption] -> Options
parseOptions args =
  Options
    { excessive = "excessive" `elem` args
    }


