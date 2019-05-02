{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies,
             MultiParamTypeClasses, FlexibleContexts, GADTs, 
             OverloadedStrings, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeApplications, FlexibleInstances #-}
import Web.Spock
import Web.Spock.Config
import Network.HTTP.Types
import Database.Persist.TH
import qualified Database.Persist.Sqlite as Db
import Data.Text as T
import Control.Monad.Logger
import Control.Monad.Trans
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Text.Digestive
import Text.Digestive.Util
import Text.Digestive.Blaze.Html5
import Web.Spock.Digestive

-- USE 'shakespeare'!!!!

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country json
  name String
  canWeSend Bool default=True
  UniqueCountryName name
  deriving Show
Client json
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int
  UniqueClient firstName lastName address country
  deriving Show
Product json
  name String
  description String
  price Double
  inStock Int
  deriving Show
Purchase json
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]

main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock cfg app)

main2 :: IO ()
main2 = do
  Db.runSqlite "example.db" $ Db.runMigration migrateAll
  runNoLoggingT $
    Db.withSqlitePool "example.db" 10 $ \pool -> liftIO $ do
      cfg <- defaultSpockCfg () (PCPool pool) ()
      runSpock 3000 (spock cfg app2)

app :: SpockM () () () ()
app = do
  get "about" $ 
    html $ mconcat [ "<html><body>"
                   , "  <h1>Hello Practical Haskell!</h1>"
                   , "</body></html>"]

  hookAnyAll $ \_route -> do
    setStatus notFound404
    html "<h1>Not found :(</h1>"

app2 :: SpockM Db.SqlBackend () () ()
app2 = do
  get "products" $ do
    (products :: [Db.Entity Product]) <- runQuery $ \conn ->
      flip Db.runSqlPersistM conn $
        Db.selectList [] []
    html $ toStrict $ renderHtml [shamlet|
      <html>
        <body>
          <h1>Products
          <table>
            <tr>
              <th>Name
              <th>Description
            $forall Db.Entity _ p <- products
              <tr>
                <td>#{productName p}
                <td>#{productDescription p}
    |]

  get ("product" <//> var) $ \(productId :: Integer) -> do
    product <- runQuery $ \conn ->  -- pool taken care of
      flip Db.runSqlPersistM conn $ 
        Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just (Product { .. }) ->
        {-
        html $ mconcat [ "<html><body>"
                       , "<h1>"
                       , T.pack productName
                       , "</h1>"
                       , "<p>"
                       , T.pack productDescription
                       , "</p>"
                       , "</body></html>" ]
        -}
        {-
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $
              H.title "Time Machine Store"
            H.body $ do
              H.h1 $ H.toHtml productName
              H.p H.! A.id "descr" $ H.toHtml productDescription
        -}
        html $ toStrict $ renderHtml [shamlet|
          <html>
            <head>
               <title>Time Machine Store
            <body>
               <h1>#{productName}
               <p id=descr>#{productDescription}
        |]
      Nothing -> do setStatus notFound404
                    html "<h1>Not found :(</h1>"
  
  get ("json" <//> "product" <//> var) $ \(productId :: Integer) -> do
    product <- runQuery $ \conn ->  -- pool taken care of
      flip Db.runSqlPersistM conn $ 
        Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just p  -> json p
      Nothing -> setStatus notFound404
  
  get "new-product" $ do
    view <- getForm "product" productForm
    let view' = fmap H.toHtml view
    html $ toStrict $ renderHtml $
      H.html $ do
        H.head $ H.title "Time Machine Store"
        H.body $ productView view'

  post "new-product" $ do
    (view,product) <- runForm "product" productForm
    case product of
      Just p -> do
        ProductKey (Db.SqlBackendKey newId) <- runQuery $ \conn ->  -- pool taken care of
          flip Db.runSqlPersistM conn $ Db.insert p
        redirect $ mconcat ["/product/", T.pack $ show newId]
      Nothing -> do
        let view' = fmap H.toHtml view
        html $ toStrict $ renderHtml $
          H.html $ do
            H.head $ H.title "Time Machine Store"
            H.body $ productView view'
      
countryForm :: Monad m => Form String m Country
countryForm = Country <$> "name" .: string Nothing
                      <*> "send" .: bool (Just True)

productForm :: Monad m => Form String m Product
productForm = Product <$> "name"        .: string Nothing
                      <*> "description" .: string Nothing
                      <*> "price"       .: validate isANumber (string Nothing)
                      <*> "inStock"     .: check "Must be >= 0" (>= 0)
                                            (validate isANumber (string Nothing))

isANumber :: (Num a, Read a) => String -> Result String a
isANumber = maybe (Error "Not a number") Success . readMaybe

productView :: View H.Html -> H.Html
productView view = do
  form view "/new-product" $ do
    label     "name"    view "Name:"
    inputText "name"    view
    H.br
    inputTextArea Nothing Nothing "description" view
    H.br
    label     "price"   view "Price:"
    inputText "price"   view
    errorList "price"   view
    label     "inStock" view "# in Stock:"
    inputText "inStock" view
    errorList "inStock" view
    H.br
    inputSubmit "Submit"
