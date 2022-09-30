{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)

import BasicSchema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=gwentDB password=admin"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


fetchCardPG :: PGInfo -> Int64 -> IO (Maybe Card)
fetchCardPG connString bid = runAction connString (get (toSqlKey bid))


fetchAllCardsForDeckPG :: PGInfo -> Int64  -> IO [Entity Card]
fetchAllCardsForDeckPG connString lid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Card]
    fetchAction = select . from $ \cards2 -> do
      where_ (cards2 ^. CardDeckId ==. val (toSqlKey lid))
      return cards2


fetchAllCardsPG :: PGInfo -> IO [Entity Card]
fetchAllCardsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Card]
    fetchAction = select . from $ \cards2 -> do
      return cards2


fetchRecentCardsPG :: PGInfo -> IO [(Entity Deck, Entity Card)]
fetchRecentCardsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Deck, Entity Card)]
    fetchAction = select . from $ \(decks `InnerJoin` cards) -> do
      on (decks ^. DeckId  ==. cards ^. CardDeckId)
      orderBy [desc (cards ^. CardStrength)]
      limit 10
      return (decks, cards)

fetchPrekoJoinaPG :: PGInfo -> IO [Entity Card]
fetchPrekoJoinaPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Card]
    fetchAction = select . from $ \(decks `InnerJoin` cards) -> do
      on (decks ^. DeckId  ==. cards ^. CardDeckId)
      orderBy [desc (cards ^. CardStrength)]
      limit 10
      return cards

createCardPG :: PGInfo -> Card -> IO Int64 
createCardPG connString card = fromSqlKey <$> runAction connString (insert card)

createDeckPG :: PGInfo -> Deck  -> IO Int64
createDeckPG connString deck = fromSqlKey <$> runAction connString (insert deck)


deleteCardPG :: PGInfo -> Int64 -> IO () 
deleteCardPG connString bid = runAction connString (delete cardKey)
  where
    cardKey :: Key Card
    cardKey = toSqlKey bid
