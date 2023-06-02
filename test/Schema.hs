{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Schema
    ( Person (..)
    , Car (..)
    , Key (..)
    , CarId
    , entityDefs
    ) where

import           Database.Persist.Class
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Prelude
import           Test.QuickCheck

share [mkPersist sqlSettings, mkEntityDefList "entityDefs"] [persistLowerCase|
Person
    name String
    age Int
    Primary name
    deriving Show
    deriving Eq
    deriving Ord

Car
    cid Int
    color String Maybe
    owner PersonId
    deriving Show
    deriving Eq
    deriving Ord
|]

instance Arbitrary Person where
    arbitrary = Person <$> elements names
                       <*> suchThat arbitrary (> 0)

instance Arbitrary Car where
    arbitrary = Car <$> arbitrary
                    <*> genColor
                    <*> (PersonKey <$> elements names)

names :: [String]
names = ["John", "Stevan", "Kostas", "Curry", "Robert"]

colors :: [String]
colors = ["black", "blue", "red", "yellow"]

genColor :: Gen (Maybe String)
genColor = elements $ Nothing : (Just <$> colors)
