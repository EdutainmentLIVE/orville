module Example.Data.Student
  ( Student(..)
  , StudentId(..)
  , StudentName(..)
  ) where

import Data.Text (Text)

import Example.Data.Major (MajorId)

data Student key = Student
  { studentId :: key
  , studentName :: StudentName
  , studentMajor :: MajorId
  } deriving (Show, Eq)

newtype StudentId = StudentId
  { studentIdInt :: Int
  } deriving (Show, Eq, Ord)

newtype StudentName = StudentName
  { studentNameText :: Text
  } deriving (Show, Eq)
