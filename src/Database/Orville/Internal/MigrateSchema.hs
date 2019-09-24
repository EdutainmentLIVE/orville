{-|
Module    : Database.Orville.Internal.MigrateSchema
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Internal.MigrateSchema
  ( migrateSchema
  , MigrationError(..)
  ) where

import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Data.Data
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.MigrateConstraint
import            Database.Orville.Internal.MigrateIndex
import            Database.Orville.Internal.MigrateTable
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Types

migrateSchema :: SchemaDefinition -> Orville ()
migrateSchema schemaDef =
  withConnection $ \conn -> do
    tables <- liftIO $ getTables conn
    indexes <- liftIO $ getIndexes conn
    constraints <- liftIO $ getConstraints conn

    forM_ schemaDef $ \table ->
      case table of
      Table tableDef ->
        if tableName tableDef `elem` tables
        then migrateTable conn tableDef
        else createTable conn tableDef

      DropTable name ->
        when (name `elem` tables)
              (dropTable conn name)

      Index indexDef ->
        when (not $ indexName indexDef `elem` indexes)
              (createIndex conn indexDef)

      DropIndex name ->
        when (name `elem` indexes)
              (dropIndex conn name)

      Constraint constraintDef ->
        when (not $ constraintName constraintDef `elem` constraints)
              (createConstraint conn constraintDef)

      DropConstraint tablName name ->
        when (name `elem` constraints)
              (dropConstraint conn tablName name)

data MigrationError =
    MigrationLockExcessiveRetryError String
  deriving (Data, Typeable, Show)

instance Exception MigrationError
