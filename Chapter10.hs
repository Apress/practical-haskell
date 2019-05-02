{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.Text
import Data.Monoid
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy as LT

main :: IO()
main = runConduitRes $
  B.sourceFile "clients.db" .| T.decode T.utf8 .|
  T.lines .| winnersFile .| L.concatMap (\x -> [x, "\n"]) .|
  T.encode T.utf8 .| B.sinkFile "clientsWinners.db"

winnersFile :: (Monad m, MonadIO m) => ConduitT Text Text m ()
winnersFile = undefined  -- same as previous chapter, but using Text

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)
                       -- Eq and Ord will be introduced in Chapter 4

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Eq, Ord)


saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients = runConduitRes $
  L.sourceList clients .| L.map clientToText 
    .| L.concatMap (\x -> [x, "\n"])  -- write '\n' between clients
    .| T.encode T.utf8 .| B.sinkFile fpath

clientToText :: Client Int -> Text
clientToText (GovOrg  i n)     =
  "client(gov," <> escapeString (show i) <> "," <> escapeString n <> ")"
clientToText (Company i n p d) = 
  "client(com," <> escapeString (show i) <> "," <> escapeString n <> ","
    <> personToText p <> "," <> escapeString d <> ")"
clientToText (Individual i p)  =
  "client(ind," <> escapeString (show i) <> "," <> personToText p <> ")"

personToText :: Person -> Text
personToText (Person f l) = "person(" <> escapeString f <> "," <> escapeString l <> ")"

escapeString :: String -> Text
escapeString = replace "\n" "\\n" . replace ","  "\\," .
               replace "("  "\\(" . replace ")"  "\\(" . pack

clientToTextB :: Client Int -> B.Builder
clientToTextB (GovOrg i n) =
  "client(gov," <> B.decimal i <> B.singleton ',' 
                <> B.fromText (escapeString n) <> B.singleton ')'
clientToTextB (Company i n p d) =
  "client(com," <> B.decimal i <> B.singleton ',' 
                <> B.fromText (escapeString n) <> B.singleton ','
                <> personToTextB p <> B.singleton ',' 
                <> B.fromText (escapeString d) <> B.singleton ')'
clientToTextB (Individual i p) = 
  "client(ind," <> B.decimal i <> B.singleton ',' 
                <> personToTextB p <> B.singleton ')'

personToTextB :: Person -> B.Builder
personToTextB (Person f l) =
  "person(" <> B.fromText (escapeString f) <> B.singleton ',' 
            <> B.fromText (escapeString l) <> B.singleton ')'

saveClientsB :: FilePath -> [Client Int] -> IO ()
saveClientsB fpath clients = runConduitRes $
  L.sourceList clients .| L.map clientToTextB .| L.map (LT.toStrict . B.toLazyText)
    .| L.concatMap (\x -> [x, "\n"])  -- write '\n' between clients
    .| T.encode T.utf8 .| B.sinkFile fpath
