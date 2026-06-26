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

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.IORef (readIORef)
import Data.List ()
import Data.Map (Map)
import Data.Set (Set, member)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC (DynFlags, ModSummary(ms_hspp_file), ModuleName, Name, moduleName)
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
import GHC.Types.TyThing (TyThing(AConLike))
import GHC.Unit.Module.Imported
  ( ImportedBy(ImportedByUser), ImportedModsVal(imv_all_exports)
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Eq((==)), Foldable(elem)
  , Maybe(Just, Nothing), Monoid(mempty), Num((+), (-)), Ord((>), (<=))
  , Semigroup((<>)), Show(show), ($), (.), (<$>), (&&), (||), FilePath
  , Int, String, break, concat, dropWhile, filter, length, lines, mapM, not
  , otherwise, putStr, putStrLn, readFile, reverse, span, unlines, words
  , writeFile
  )
import OM.Plugin.Imports.Format
  ( GroupKeyType
  , ImportList (OpenImport)
  , ImportStmt (ImportStmt)
  , ImportStmtHead (ImportStmtHead)
  , buildPartialImportList
  , formatImportBlock
  , parentImportEntry
  )
import Safe (headMay)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set


plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = typeCheckResultActionImpl
  , pluginRecompile = \_ -> pure NoForceRecompile
  }


data Options = Options
  { excessive :: Bool
  ,   inPlace :: Bool
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
  void $ writeCanonicalImports options (ms_hspp_file modSummary) flags used
  endTime <- liftIO getCurrentTime
  liftIO (putStrLn (" in " <> show (diffUTCTime endTime startTime)))
  pure env


writeCanonicalImports
  :: (MonadIO m)
  => Options
  -> FilePath
  -> DynFlags
  -> Map ModuleImport (Map WrappedName (Set WrappedName))
  -> m (Maybe FilePath)
writeCanonicalImports options srcFile flags used =
  liftIO $ do
    let
      imports :: String
      imports = renderNewImports options flags used

      filename :: FilePath
      filename = srcFile <> ".full-imports"
    if options.inPlace then do
      src <- readFile srcFile
      let
        updatedSrc :: String
        updatedSrc = replaceImportBlock imports src
      void (evaluate (length updatedSrc))
      writeFile srcFile updatedSrc
      pure Nothing
    else do
      writeFile filename imports
      pure (Just filename)


replaceImportBlock :: String -> String -> String
replaceImportBlock imports src =
  let
    importLines :: [String]
    importLines = lines imports

    srcLines :: [String]
    srcLines = lines src

    (prefix, rest) = break isImportStart srcLines

    lines_ :: [String]
    lines_ =
      if not (List.null rest) then
        trimTrailingBlank prefix
        <> [""]
        <> importLines
        <> [""]
        <> dropLeadingBlank (dropImportBlock rest)
      else
        insertImportBlock importLines srcLines
  in
    unlines lines_


dropImportBlock :: [String] -> [String]
dropImportBlock [] = []
dropImportBlock importLineRest =
  let
    afterImport :: [String]
    afterImport = dropOneImport importLineRest

    between :: [String]
    afterBetween :: [String]
    (between, afterBetween) = span isBlankOrComment afterImport
  in
    case afterBetween of
      line : _
        | isImportStart line ->
            dropImportBlock afterBetween
      _ ->
        between <> afterBetween


dropOneImport :: [String] -> [String]
dropOneImport [] = []
dropOneImport (line : rest) =
  let
    balance :: Int
    balance = parenDelta line
  in
    if balance <= 0 && not (continuesOnNextLine rest) then
      rest
    else
      dropImportContinuation balance rest


dropImportContinuation :: Int -> [String] -> [String]
dropImportContinuation _ [] = []
dropImportContinuation balance (line : rest) =
  let
    balance_ :: Int
    balance_ = balance + parenDelta line
  in
    if balance_ <= 0 && not (continuesOnNextLine rest) then
      rest
    else
      dropImportContinuation balance_ rest


continuesOnNextLine :: [String] -> Bool
continuesOnNextLine [] = False
continuesOnNextLine (line : _) =
  case line of
    [] ->
      False
    c : _ ->
      Char.isSpace c


insertImportBlock :: [String] -> [String] -> [String]
insertImportBlock importLines srcLines =
  let
    (prefix, suffix) = break hasModuleWhere srcLines
  in
    case suffix of
      [] ->
        importLines <> [""] <> srcLines
      moduleWhereLine : rest ->
        prefix
        <> [moduleWhereLine]
        <> [""]
        <> importLines
        <> [""]
        <> dropLeadingBlank rest


isImportStart :: String -> Bool
isImportStart line =
  let
    line_ :: String
    line_ = dropWhile Char.isSpace line
  in
    "import " `List.isPrefixOf` line_
    || "import\t" `List.isPrefixOf` line_


isBlankOrComment :: String -> Bool
isBlankOrComment line =
  let
    line_ :: String
    line_ = dropWhile Char.isSpace line
  in
    List.null line_
    || "--" `List.isPrefixOf` line_
    || "{-" `List.isPrefixOf` line_


hasModuleWhere :: String -> Bool
hasModuleWhere line =
  "where" `elem` words line


parenDelta :: String -> Int
parenDelta line =
  length (filter (== '(') line)
  - length (filter (== ')') line)


trimTrailingBlank :: [String] -> [String]
trimTrailingBlank =
  reverse . dropWhile (List.null . dropWhile Char.isSpace) . reverse


dropLeadingBlank :: [String] -> [String]
dropLeadingBlank =
  dropWhile (List.null . dropWhile Char.isSpace)


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
  formatImportBlock (buildImportStmts options flags used)


buildImportStmts
  :: Options
  -> DynFlags
  -> Map ModuleImport (Map WrappedName (Set WrappedName))
  -> [ImportStmt]
buildImportStmts options flags used =
  [ ImportStmt (importHead modImport) (importList modImport parents)
  | (modImport, parents) <- Map.toList used
  ]
  where
    ambiguousNames :: Set ModuleName
    ambiguousNames =
      Map.keysSet
      . Map.filter (> 1)
      . Map.unionsWith (+)
      $ [ case modImport of
            Unqualified { name } -> Map.singleton name (1 :: Int)
            UnqualifiedAs { as } -> Map.singleton as 1
            Qualified { name } -> Map.singleton name 1
            QualifiedAs { as } -> Map.singleton as 1
        | (modImport, _) <- Map.toList used
        ]

    importHead :: ModuleImport -> ImportStmtHead
    importHead modImport =
      case modImport of
        Unqualified { name } ->
          ImportStmtHead False (shown name) Nothing
        UnqualifiedAs { name, as } ->
          ImportStmtHead False (shown name) (Just (shown as))
        Qualified { name } ->
          ImportStmtHead True (shown name) Nothing
        QualifiedAs { name, as } ->
          ImportStmtHead True (shown name) (Just (shown as))

    importList
      :: ModuleImport
      -> Map WrappedName (Set WrappedName)
      -> ImportList
    importList modImport parents =
      case modImport of
        Unqualified {} ->
          buildPartialImportList (parentEntries parents)
        UnqualifiedAs {} ->
          buildPartialImportList (parentEntries parents)
        Qualified { name } ->
          if options.excessive || name `member` ambiguousNames then
            buildPartialImportList (parentEntries parents)
          else
            OpenImport
        QualifiedAs { as } ->
          if options.excessive || as `member` ambiguousNames then
            buildPartialImportList (parentEntries parents)
          else
            OpenImport

    parentEntries
      :: Map WrappedName (Set WrappedName)
      -> Map GroupKeyType (Set String)
    parentEntries parents =
      Map.fromListWith Set.union
        [ parentImportEntry
            (shown name)
            isPat
            (Set.map shownChild children)
        | (WrappedName name isPat, children) <- Map.toList parents
        ]

    shownChild :: WrappedName -> String
    shownChild (WrappedName name _) =
      shown name

    shown :: Outputable o => o -> String
    shown =
      fixInlineName . showSDoc flags . ppr

    fixInlineName :: String -> String
    fixInlineName name =
      case headMay name of
        Nothing ->
          name
        Just c
          | Char.isAlphaNum c || c == '_' ->
              name
          | otherwise ->
              "(" <> name <> ")"


parseOptions :: [CommandLineOption] -> Options
parseOptions args =
  Options
    { excessive = "excessive" `elem` args
    ,   inPlace = "in-place" `elem` args
    }


