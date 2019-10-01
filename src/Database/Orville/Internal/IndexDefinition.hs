{-|
Module    : Database.Orville.Internal.IndexDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.Internal.IndexDefinition
  ( IndexName, uniqueIndex, simpleIndex, indexNameToString, stringToIndexName, safeStringToIndexName
  ) where

import            Data.List (intercalate)

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

uniqueIndex :: IndexName -> TableDefinition entity -> [FieldDefinition] -> Bool -> IndexDefinition
uniqueIndex name tableDef fields concurrently =
  IndexDefinition {
    indexName = indexNameToString name
  , indexUnique = True
  , indexConcurrently = concurrently
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

simpleIndex :: IndexName -> TableDefinition entity -> [FieldDefinition] -> Bool -> IndexDefinition
simpleIndex name tableDef fields concurrently =
  IndexDefinition {
    indexName = indexNameToString name
  , indexUnique = False
  , indexConcurrently = concurrently
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

indexFieldsBody :: [FieldDefinition] -> String
indexFieldsBody fields = "(" ++ intercalate "," (map escapedFieldName fields) ++ ")"

newtype IndexName = IndexName String deriving (Eq, Ord)

indexNameToString :: IndexName -> String
indexNameToString (IndexName i) = i

stringToIndexName :: String -> IndexName
stringToIndexName name =
  case safeStringToIndexName name of
    Left err -> error err
    Right iName -> iName

safeStringToIndexName :: String -> Either String IndexName
safeStringToIndexName name =
  if length name > 63 then
    Left "Index greater than 63 characters"
  else
    Right $ IndexName name
