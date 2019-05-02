{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
import Data.Conduit
import qualified Data.Conduit.List as L
import Control.Monad.State
import Control.Monad.Trans
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as B
import Data.Monoid
import Data.Conduit.Network
import Network.Socket
import System.Environment
import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Data.Conduit.Serialization.Binary as S
import Control.Monad.Trans
import Data.Csv (FromRecord, ToRecord)
import qualified Data.Csv as Csv
import qualified Data.Csv.Conduit as Csv
import System.IO.Error

a = runConduitPure $ L.sourceList [1 .. 5] .| L.fold (+) 0
b = runConduitPure $ L.sourceList [1 .. 20] .| L.filter odd .| L.map (\x -> x*x) .| L.fold (+) 0
c = runConduitPure $ L.unfold (\x -> Just (x,x+1)) 1 .| L.isolate 10 .| L.consume

-- data Person = Person String String deriving Show
data Client i = Company { id :: Int, person :: Person }
              | Individual { id :: Int, person :: Person }
              | GovOrg { id :: Int, name :: String }
              deriving Show

people :: Monad m => ConduitT (Client i) Person m ()
people = do client <- await
            case client of
              Nothing -> return ()
              Just c -> do case c of
                             Company { person = p }    -> yield p
                             Individual { person = p } -> yield p
                             _                         -> return ()
                           people

d = runConduitPure $ L.sourceList [ GovOrg 1 "NASA", Individual 2 (Person "A" "S")] .| people .| L.consume

countGovOrgs :: MonadState Int m => ConduitT (Client i) Void m Int
countGovOrgs = do client <- await
                  case client of
                    Nothing -> do n <- lift $ get
                                  return n
                    Just c  -> do case c of
                                    GovOrg { } -> lift $ modify (+1)
                                    _          -> return ()
                                  countGovOrgs

main = let clients = [ GovOrg 1 "Zas", Individual 2 (Person "Alejandro" "Serrano")]
           conduitGovOrgs = L.sourceList clients .| countGovOrgs
        in print $ execState (runConduit conduitGovOrgs) 0

winners :: ConduitT (Client i) (Client i, Bool, Int) IO ()
winners = do client <- await
             case client of
               Nothing -> return ()
               Just c  -> do (w :: Bool) <- lift $ randomIO
                             (y :: Int)  <- lift $ randomRIO (0, 3000)
                             yield (c, w, y)
                             winners

winnersFile :: (Monad m, MonadIO m)
            => ConduitT BS.ByteString BS.ByteString m ()
winnersFile = do
  client <- await
  case client of
    Nothing -> return ()
    Just c  -> do (w :: Bool) <- liftIO $ randomIO
                  (y :: Int ) <- liftIO $ randomRIO (0, 3000)
                  yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y)
                  winnersFile

main2 :: IO ()
main2 = runConduitRes $ B.sourceFile "clients.db" .| B.lines .| winnersFile .| B.sinkFile "clientsWinners.db"

isWinner :: ConduitT BS.ByteString BS.ByteString IO ()
isWinner = do client <- await
              case client of
                Nothing -> return ()
                Just c  -> do
                  lift $ BS.putStrLn c
                  (w :: Bool) <- liftIO $ randomIO
                  (y :: Int ) <- liftIO $ randomRIO (0, 3000)
                  yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y)
                  isWinner

serverApp :: AppData -> IO ()
serverApp d = runConduit $ appSource d .| isWinner .| appSink d

mainServer :: IO ()
mainServer = withSocketsDo $ runTCPServer (serverSettings 8900 "*") serverApp

mainClient :: IO ()
mainClient = withSocketsDo $ do
         (name:_) <- getArgs
         runTCPClient (clientSettings 8900 "127.0.0.1") (clientApp name)

clientApp :: String -> AppData -> IO ()
clientApp name d = do runConduit $ (yield $ BS.pack name) .| appSink d
                      runConduit $ appSource d .| (do Just w <- await
                                                      lift $ BS.putStrLn w)

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Read, Generic, Binary, FromRecord, ToRecord)
-- instance Binary Person

mainEnc :: IO ()
mainEnc = runConduitRes $
         L.sourceList clients.| S.conduitEncode .| B.sinkFile "people.db"
   where clients = [Person "Alejandro" "Serrano", Person " Doctor" "Who?"]

mainDec :: IO ()
mainDec = runConduitRes $
  B.sourceFile "people.db" .| S.conduitDecode 
                           .| L.mapM_ (\(p :: Person) -> lift $ putStrLn $ show p)

mainDec2 :: IO ()
mainDec2 = runConduitRes $
  B.sourceFile "people.db"
  .| Csv.fromCsvLiftError (userError . show) Csv.defaultDecodeOptions Csv.NoHeader
  .| L.mapM_ (\(p :: Person) -> lift $ putStrLn $ show p)
