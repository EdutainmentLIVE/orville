{-|
Module    : Database.Orville.Internal.IndexDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.Internal.IndexDefinition
  ( uniqueIndex, simpleIndex
  ) where

import            Data.List (intercalate)

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

uniqueIndex :: String -> TableDefinition entity -> [FieldDefinition] -> Bool -> IndexDefinition
uniqueIndex name tableDef fields concurrently =
  IndexDefinition {
    indexName = name
  , indexUnique = True
  , indexConcurrently = concurrently
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

simpleIndex :: String -> TableDefinition entity -> [FieldDefinition] -> Bool -> IndexDefinition
simpleIndex name tableDef fields concurrently =
  IndexDefinition {
    indexName = name
  , indexUnique = False
  , indexConcurrently = concurrently
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

indexFieldsBody :: [FieldDefinition] -> String
indexFieldsBody fields = "(" ++ intercalate "," (map escapedFieldName fields) ++ ")"
