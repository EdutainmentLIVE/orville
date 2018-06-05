{-|
Module    : Database.Orville.Internal.Expr.SelectExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Database.Orville.Internal.Expr.SelectExpr where

import                Data.Maybe

#if ! MIN_VERSION_base(4,11,0)
import                Data.Monoid
#endif

import                Database.Orville.Internal.Expr.Expr
import                Database.Orville.Internal.Expr.NameExpr

type SelectExpr = Expr SelectForm

data SelectForm = SelectForm
  { selectFormColumn  :: NameForm
  , selectFormTable   :: Maybe NameForm
  , selectFormAlias   :: Maybe NameForm
  }

selectColumn :: NameForm -> SelectForm
selectColumn name = SelectForm name Nothing Nothing

selectFormOutput :: SelectForm -> NameForm
selectFormOutput = fromMaybe <$> selectFormColumn <*> selectFormAlias

qualified :: SelectForm -> NameForm -> SelectForm
qualified sf name = sf { selectFormTable = Just name }

aliased :: SelectForm -> NameForm -> SelectForm
aliased sf name = sf { selectFormAlias = Just name }

instance GenerateSql SelectForm where
  generateSql (SelectForm {..}) =
       qualification selectFormTable
    <> generateSql selectFormColumn
    <> asOutput selectFormAlias

qualification :: Maybe NameForm -> RawExpr
qualification Nothing = mempty
qualification (Just name) = generateSql name <> "."

asOutput :: Maybe NameForm -> RawExpr
asOutput Nothing = mempty
asOutput (Just name) = " AS " <> generateSql name
