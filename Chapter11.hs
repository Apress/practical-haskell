{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
             MultiParamTypeClasses, FlexibleContexts, GADTs, 
             OverloadedStrings, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeApplications #-}
module Chapter11 where

import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Maybe (catMaybes)
import Data.Char

data Gender = Male | Female
    deriving (Show, Read, Eq)
derivePersistField "Gender"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name String
  UniqueCountryName name
  deriving Show
Client
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int
  UniqueClient firstName lastName address country
  deriving Show
Product
  name String
  price Double
  amount Int
  inStock Int
  deriving Show
Purchase
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]

exampleConn = runNoLoggingT $
  withSqliteConn @(NoLoggingT IO) @SqlBackend "example.db" $ \conn ->
    liftIO $ flip runSqlPersistM conn $ do
      spain     <- insert $ Country "Spain"
      _client1  <- insert $ Client "Alejandro" "Serrano"
                                   "Home Town, 1" spain 25
      return ()

exampleConn2 = runSqlite @IO @SqlBackend "example.db" $ do
  spain     <- insert $ Country "Spain"
  _client1  <- insert $ Client "Alejandro" "Serrano"
                               "Home Town, 1" spain 25
  return ()

examplePool = runNoLoggingT $
  withSqlitePool @(NoLoggingT IO) @SqlBackend "example.db" 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      spain     <- insert $ Country "Spain"
      _client1  <- insert $ Client "Alejandro" "Serrano"
                                   "Home Town, 1" spain 25
      return ()

ejemplico = runSqlite @IO @SqlBackend "example.db" $ do
  spain     <- insert $ Country "Spain"
  _client1  <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain 25
  return ()

getPurchaseClient p = get (purchaseClient p)  -- returns Maybe Client
getClientById n = get $ ClientKey (SqlBackendKey $ fromIntegral n)

getClientByInfo :: MonadIO m => String -> String -> String -> String -> SqlPersistT m (Maybe Client)
getClientByInfo fName lName addr cnName = do
  cn <- getBy $ UniqueCountryName cnName
  case cn of
    Just (Entity cId _) -> 
      do cl <- getBy $ UniqueClient fName lName addr cId
         case cl of
           Just (Entity _ client) -> return $ Just client
           Nothing                -> return Nothing
    Nothing -> return Nothing

getAdultsOfSpainAndGermany :: MonadIO m => SqlPersistT m [Entity Client]
getAdultsOfSpainAndGermany = do
  es <- getBy $ UniqueCountryName "Spain"
  de <- getBy $ UniqueCountryName "Germany"
  let countries = map entityKey (catMaybes [es, de])
  selectList [ ClientCountry <-.countries, ClientAge >=. 18 ] []

countAdultsOfSpainAndGermany :: MonadIO m => SqlPersistT m Int
countAdultsOfSpainAndGermany = do
  es <- getBy $ UniqueCountryName "Spain"
  de <- getBy $ UniqueCountryName "Germany"
  let countries = map entityKey (catMaybes [es, de])
  count [ ClientCountry <-.countries, ClientAge >=. 18 ]

getAdultsOfSpainAndUS :: MonadIO m => SqlPersistT m [Entity Client]
getAdultsOfSpainAndUS = do
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity usId _) <- getBy $ UniqueCountryName "United States of America"
  selectList (   [ ClientCountry ==. spId, ClientAge >=. 18 ]
             ||. [ ClientCountry ==. usId, ClientAge >=. 21 ] )
             []

getProductsPage n = selectList [ ] [ Asc ProductPrice, LimitTo 10, OffsetBy ((n-1)*10) ]

getCountriesWithBigBuyers :: MonadIO m => SqlPersistT m [Country]
getCountriesWithBigBuyers = do  -- returns [Country]
  buyers <- selectKeysList [ ] [ ]
  buyersPurchases <- mapM (\b -> count [ PurchaseClient ==. b ] >>= \c -> return (b,c)) buyers
  let buyersPurchases' = filter (\(_,c) -> c > 3) buyersPurchases
  mapM (\(b,_) -> do Just cl <- get b
                     Just cn <- get $ clientCountry cl
                     return cn)
       buyersPurchases'

getPeopleOver25 :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25 =  -- returns [Entity Client]
  E.select $
  E.from $ \client -> do
  E.where_ (client ^. ClientAge E.>. E.val 25)
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getPeopleOver25FromSpainOrGermany :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25FromSpainOrGermany =  -- returns [Entity Client]
  E.select $
  E.from $ \(client, country) -> do
  E.where_ (   client ^. ClientAge E.>. (E.val 25)
           E.&&. country ^. CountryName `E.in_` E.valList [ "Spain", "Germany" ]
           E.&&. client ^. ClientCountry E.==. country ^. CountryId )
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getPeopleOver25FromSpainOrGermanyJoin :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25FromSpainOrGermanyJoin =  -- returns [Entity Client]
  E.select $ 
  E.from $ \(client `E.InnerJoin` country) -> do
  E.on (client ^. ClientCountry E.==. country ^. CountryId)
  E.where_ (     client ^. ClientAge E.>. (E.val 25)
           E.&&. country ^. CountryName `E.in_` E.valList [ "Spain", "Germany" ])
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getMoneyByClient :: MonadIO m => SqlPersistT m [(Entity Client, E.Value (Maybe Double))]
getMoneyByClient =  -- returns [(Entity Client, Value (Maybe Double))]
  E.select $ 
  E.from $ \(client `E.LeftOuterJoin` purchase) -> do
  E.on (client ^. ClientId E.==. purchase ^. PurchaseClient)
  E.groupBy (client ^. ClientId)
  let s = E.sum_ (purchase ^. PurchaseAmount)
  return (client, s)

capitalizeNamesSlow :: MonadIO m => SqlPersistT m ()
capitalizeNamesSlow = do
  clients <- selectList [] []
  mapM_ (\(Entity ident client) -> 
             let c:rest  = clientFirstName client 
              in replace ident $ client { clientFirstName = (toUpper c):rest })
        clients

discount :: MonadIO m => SqlPersistT m ()
discount = do
  updateWhere [ ProductPrice <=. 10000 ] [ ProductPrice *=. 0.9 ]
  updateWhere [ ProductPrice >. 10000 ] [ ProductPrice *=. 0.97 ]

betterDiscount :: MonadIO m => SqlPersistT m ()
betterDiscount = E.update $ \product -> do
  let totalAmount = E.sub_select $
                    E.from $ \purchase -> do
                    E.where_ $ product ^. ProductId E.==. purchase ^. PurchaseProduct
                    E.groupBy (purchase ^. PurchaseProduct)
                    return $ E.sum_ (purchase ^. PurchaseAmount)
  E.where_ $ E.isNothing totalAmount E.||. totalAmount E.<. E.just (E.val (10 :: Double))
  E.set product [ ProductPrice E.*=. E.val 0.9 ]

cleanProductStock :: MonadIO m => SqlPersistT m ()
cleanProductStock = deleteWhere [ ProductInStock ==. 0 ]

cleanProductStock' :: MonadIO m => SqlPersistT m ()
cleanProductStock' = E.delete $ 
  E.from $ \product -> do
  E.where_ $ product ^. ProductInStock E.==. E.val 0
           E.&&. (E.notExists $ E.from $ \purchase ->
                                E.where_ (purchase ^. PurchaseProduct E.==. product ^. ProductId))
