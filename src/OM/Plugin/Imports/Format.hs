{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Format generated import statements using simformat conventions. -}
module OM.Plugin.Imports.Format (
  formatImportBlock,
  buildPartialImportList,
  parentImportEntry,
  GroupKeyType,
  ImportStmt (ImportStmt),
  ImportStmtHead (ImportStmtHead),
  ImportList (OpenImport, PartialImport),
) where

import Data.Bool (bool)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import Data.Set (Set)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set


data ImportStmtHead = ImportStmtHead
  { importStmtHeadQualified  :: Bool
  , importStmtHeadModuleName :: String
  , importStmtHeadAlias      :: Maybe String
  }
  deriving stock (Eq, Show)


instance Ord ImportStmtHead where
  ImportStmtHead q1 m1 a1 `compare` ImportStmtHead q2 m2 a2 =
    (q1, m1, a1) `compare` (q2, m2, a2)


data ImportStmt = ImportStmt
  { importStmtHead       :: ImportStmtHead
  , importStmtImportList :: ImportList
  }
  deriving stock (Eq, Ord, Show)


data ImportList
  = OpenImport
  | PartialImport (Map GroupKeyType (Set String))
  deriving stock (Eq, Ord, Show)


data GroupKeyType
  = ConstructorKey String
  | NoGroupKey
  | NoGroupPatternKey
  deriving stock (Eq, Ord, Show)


newtype SortedImportStmts = SortedImportStmts
  { unSortedImportStmts :: Map ImportStmtHead ImportList
  }
  deriving stock (Eq, Ord, Show)


formatImportBlock :: [ImportStmt] -> String
formatImportBlock =
  unlines . map renderImportStmt . fromSortedImportStmts . toSortedImportStmts


toSortedImportStmts :: [ImportStmt] -> SortedImportStmts
toSortedImportStmts =
  SortedImportStmts
    . Map.fromListWith mergeImportList
    . map
        ( \ImportStmt
            { importStmtHead = head_
            , importStmtImportList = list_
            } ->
            (head_, list_)
        )


mergeImportList :: ImportList -> ImportList -> ImportList
mergeImportList OpenImport _ = OpenImport
mergeImportList _ OpenImport = OpenImport
mergeImportList (PartialImport left) (PartialImport right) =
  PartialImport (Map.unionWith Set.union left right)


fromSortedImportStmts :: SortedImportStmts -> [ImportStmt]
fromSortedImportStmts =
  map (uncurry ImportStmt) . Map.toList . unSortedImportStmts


renderImportStmt :: ImportStmt -> String
renderImportStmt
  ImportStmt
    { importStmtHead =
        ImportStmtHead
          { importStmtHeadQualified = qualified_
          , importStmtHeadModuleName = moduleName_
          , importStmtHeadAlias = alias_
          }
    , importStmtImportList = importList_
    } =
  let
    prefix :: String
    prefix =
      "import"
      <> bool "" " qualified" qualified_
      <> printf " %s" moduleName_
      <> maybe "" (printf " as %s") alias_
  in
    prefix <> renderImportList (length prefix) importList_


renderImportList :: Int -> ImportList -> String
renderImportList prefixLen importList =
  case importList of
    OpenImport ->
      ""
    PartialImport entries ->
      renderImportEntries prefixLen entries


renderImportEntries :: Int -> Map GroupKeyType (Set String) -> String
renderImportEntries prefixLen =
  either (' ':) (('\n' :) . intercalate "\n")
    . renderList prefixLen "  " id
    . concatMap chunkImportEntry
    . Map.toAscList


chunkImportEntry :: (GroupKeyType, Set String) -> [String]
chunkImportEntry (NoGroupKey, symbols) =
  Set.toAscList symbols
chunkImportEntry (ConstructorKey groupName, groupSymbols) =
  [groupName <> renderGroupList groupSymbols]
chunkImportEntry (NoGroupPatternKey, symbols) =
  ("pattern " <>) <$> Set.toAscList symbols


renderGroupList :: Set String -> String
renderGroupList =
  either id (('\n' :) . intercalate "\n")
    . renderList 0 "    " id
    . Set.toAscList


renderList :: Int -> String -> (a -> String) -> [a] -> Either String [String]
renderList prefixLen indent renderItem items
  | null items =
      Left "()"
  | tooLongSingleLine oneLine =
      Right multiLine
  | otherwise =
      Left oneLine
  where
    tooLongSingleLine :: String -> Bool
    tooLongSingleLine line =
      length line + prefixLen > 80

    tooLong :: String -> Bool
    tooLong line =
      length line > 80

    oneLine :: String
    oneLine =
      printf "(%s)" (intercalate ", " (renderItem <$> items))

    multiLine :: [String]
    multiLine =
      go (indent <> "( ") True items
      where
        go _ True [] =
          [indent <> ")"]
        go acc False [] =
          [acc, indent <> ")"]
        go acc True (item : rest) =
          go (acc <> renderItem item) False rest
        go acc False (item : rest) =
          let
            renderedItem :: String
            renderedItem = renderItem item

            acc_ :: String
            acc_ = acc <> ", " <> renderedItem
          in
            if tooLong acc_ then
              acc : go (indent <> ", " <> renderedItem) False rest
            else
              go acc_ False rest


buildPartialImportList :: Map GroupKeyType (Set String) -> ImportList
buildPartialImportList entries =
  PartialImport (removeDuplicateGroupKeys entries)


removeDuplicateGroupKeys :: Map GroupKeyType (Set String) -> Map GroupKeyType (Set String)
removeDuplicateGroupKeys entries =
  Map.adjust (Set.\\ groupKeys) NoGroupKey entries
  where
    groupKeys :: Set String
    groupKeys =
      Set.fromList
        [ key
        | ConstructorKey key <- Map.keys entries
        ]


parentImportEntry
  :: String
  -> Bool
  -> Set String
  -> (GroupKeyType, Set String)
parentImportEntry symbol isPattern children =
  if Set.null children then
    if isPattern then
      (NoGroupPatternKey, Set.singleton symbol)
    else
      (NoGroupKey, Set.singleton symbol)
  else
    ( ConstructorKey symbol
    , Set.fromList (sortOn id (Set.toList children))
    )


{-# INLINE formatImportBlock #-}
