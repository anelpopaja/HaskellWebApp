{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
--import           Network.Wai.Middleware.Cors  --NISAM USPJELA DA PODESIM DA RADI OVO, TAKO DA SE MORA KLIJENT OTVARATI U BROWSERU SA ISKLJUCENIM SIGURNOSNIM FLAG-OVIMA
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchCardPG, fetchAllCardsPG, fetchAllCardsForDeckPG, fetchRecentCardsPG, fetchPrekoJoinaPG, createDeckPG, createCardPG, deleteCardPG, localConnString)
import           BasicSchema


type CardsAPI = 
       "cards" :> Capture "cardid" Int64 :> Get '[JSON] Card
  :<|> "cards" :> ReqBody '[JSON] Card :> Post '[JSON] Int64
  :<|> "cards" :> "sve" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "monsters" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "neutral" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "nilfgaard" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "northernRealms" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "scoiatael" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> "joinDeck" :> Get '[JSON] [(Entity Deck , Entity Card)]
  :<|> "cards" :> "join" :> Get '[JSON] [Entity Card]
  :<|> "cards" :> Capture "cardid" Int64 :> Post '[JSON] () --Delete ZASAD
--  :<|> "decks" :> ReqBody '[JSON] Card :> Post '[JSON] Int64  --insert jezik, nesto ne radi


cardsAPI :: Proxy CardsAPI
cardsAPI = Proxy :: Proxy CardsAPI

fetchCardsHandler :: ConnectionString -> Int64 -> Handler Card
fetchCardsHandler connString bid = do
  maybeCard <- liftIO $ fetchCardPG connString bid
  case maybeCard of
    Just card -> return card
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find a card with that ID" })


fetchAllCardsMonstersHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsMonstersHandler connString = liftIO $ fetchAllCardsForDeckPG connString 1

fetchAllCardsNeutralHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsNeutralHandler connString = liftIO $ fetchAllCardsForDeckPG connString 2

fetchAllCardsNilfgaardHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsNilfgaardHandler connString = liftIO $ fetchAllCardsForDeckPG connString 3

fetchAllCardsNorthernRealmsHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsNorthernRealmsHandler connString = liftIO $ fetchAllCardsForDeckPG connString 4

fetchAllCardsScoiataelHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsScoiataelHandler connString = liftIO $ fetchAllCardsForDeckPG connString 5

fetchAllCardsHandler :: ConnectionString -> Handler [Entity Card]
fetchAllCardsHandler connString = liftIO $ fetchAllCardsPG connString

createCardHandler :: ConnectionString -> Card -> Handler Int64
createCardHandler connString card = liftIO $ createCardPG connString card

createDeckHandler :: ConnectionString -> Deck -> Handler Int64
createDeckHandler connString deck = liftIO $ createDeckPG connString deck

fetchRecentCardsHandler :: ConnectionString -> Handler [(Entity Deck , Entity Card)]
fetchRecentCardsHandler connString = liftIO $ fetchRecentCardsPG localConnString 

fetchPrekoJoinaHandler :: ConnectionString -> Handler [Entity Card]
fetchPrekoJoinaHandler connString = liftIO $ fetchPrekoJoinaPG localConnString

deleteCardHandler :: ConnectionString -> Int64 -> Handler ()
deleteCardHandler connString bid = liftIO $ deleteCardPG connString bid

cardsServer :: ConnectionString -> Server CardsAPI
cardsServer connString = 
  (fetchCardsHandler connString) :<|> 
  (createCardHandler connString) :<|>
  (fetchAllCardsHandler connString) :<|>
  (fetchAllCardsMonstersHandler connString) :<|>
  (fetchAllCardsNeutralHandler connString) :<|>
  (fetchAllCardsNilfgaardHandler connString) :<|>
  (fetchAllCardsNorthernRealmsHandler connString) :<|>
  (fetchAllCardsScoiataelHandler connString) :<|>
  (fetchRecentCardsHandler connString) :<|>
  (fetchPrekoJoinaHandler connString) :<|>
  (deleteCardHandler connString)
--  (createDeckHandler connString)

runServer :: IO ()
runServer = run 5000 (serve cardsAPI (cardsServer localConnString))

