{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Deck sql=decks
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Card sql=cards
    title Text
    image Text
    strength Int
    link Text
    type Text
    deckId DeckId
    UniqueText title
    deriving Show Read Eq
|]

instance ToJSON (Entity Card) where
  toJSON (Entity bid card) = object $
    "id" .= (fromSqlKey bid) : cardPairs card

instance ToJSON Card where
  toJSON card = object (cardPairs card)

cardPairs :: Card -> [Pair]
cardPairs card =
  [ "title" .= cardTitle card
  , "image" .= cardImage card
  , "strength" .= cardStrength card
  , "link" .= cardLink card
  , "type" .= cardType card
  , "deckId" .= cardDeckId card
  ]

instance FromJSON (Entity Card) where
  parseJSON = withObject "Card Entity" $ \o -> do
    card <- parseCard o
    bid <- o .: "id"
    return $ Entity (toSqlKey bid) card


instance FromJSON Card where
  parseJSON = withObject "Card" parseCard
  

parseCard :: Object -> Parser Card
parseCard o = do
  uTitle <- o .: "title"
  uImage <- o .: "image"
  uStrength <- o .: "strength"
  uLink <- o .: "link"
  uType <- o .: "type"
  uDeckId <- o .: "deckId"
  return Card
    { cardTitle = uTitle
    , cardImage = uImage
    , cardStrength = uStrength
    , cardLink = uLink
    , cardType = uType
    , cardDeckId = uDeckId
    }


--------------------------------------------------------------
instance ToJSON (Entity Deck) where
  toJSON (Entity lid deck) = object $
    "id" .= (fromSqlKey lid) : deckPairs deck

instance ToJSON Deck where
  toJSON deck = object (deckPairs deck)

deckPairs :: Deck -> [Pair]
deckPairs deck =
  [ "name" .= deckName deck
  ]

instance FromJSON (Entity Deck) where
  parseJSON = withObject "Deck Entity" $ \o -> do
    deck <- parseDeck o
    lid <- o .: "id"
    return $ Entity (toSqlKey lid) deck

instance FromJSON Deck where
  parseJSON = withObject "Deck" parseDeck

parseDeck :: Object -> Parser Deck
parseDeck o = do
  uName <- o .: "name"
  return Deck
    { deckName = uName
    }