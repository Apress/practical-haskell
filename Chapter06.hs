{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Chapter06 where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform
import Data.Maybe
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.RWS as RWS
import Data.Monoid
import Control.Monad

-- K-means

class Ord v => Vector v  where
  distance :: v -> v -> Double
  centroid :: [v] -> v
class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst
                     n = fromIntegral $ length lst
                  in (u / n, v / n)
instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                      in M.adjust (p:) chosenC m)
            initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v])  -- initialization function
       -> Int                  -- number of centroids
       -> [e]                  -- the information
       -> Double               -- threshold
       -> [v]                  -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v)
        => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold = 
  let assignments     = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
      then newCentroids
      else kMeans' newCentroids points threshold

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n)
                     : initializeSimple (n-1) v

-- LENSES
-- Hand-written
data Client0 i = GovOrg0     i String
               | Company0    i String Person0 String
               | Individual0 i Person0
data Person0   = Person0 String String

firstName0 :: Lens' Person0 String
firstName0 = lens (\(Person0 f _) -> f)
                  (\(Person0 _ l) newF -> Person0 newF l)

lastName0 :: Lens' Person0 String
lastName0 = lens (\(Person0 _ l) -> l)
                 (\(Person0 f _) newL -> Person0 f newL)

identifier0 :: Lens (Client0 i) (Client0 j) i j
identifier0 = lens (\case (GovOrg0 i _)      -> i
                          (Company0 i _ _ _) -> i
                          (Individual0 i _)  -> i)
                   (\client newId -> case client of
                       GovOrg0 _ n      -> GovOrg0 newId n
                       Company0 _ n p r -> Company0 newId n p r
                       Individual0 _ p  -> Individual0 newId p)

fullName0 :: Lens' Person0 String
fullName0 = lens (\(Person0 f l) -> f ++ " " ++ l)
                 (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person0 f l
                                      _     -> error "Incorrect name")


-- Auto-generated
data Client i = GovOrg     { _identifier :: i, _name :: String }
              | Company    { _identifier :: i, _name :: String
                           , _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show
data Person   = Person { _firstName :: String, _lastName :: String }
              deriving Show
makeLenses ''Client
makeLenses ''Person

fullName :: Lens' Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")

-- K-means with lenses

data KMeansState e v = KMeansState { _centroids :: [v], _points :: [e]
                                   , _err :: Double, _threshold :: Double
                                   , _steps :: Int }

makeLenses ''KMeansState

initializeStateL :: (Int -> [e] -> [v])
                 -> Int -> [e] -> Double -> KMeansState e v
initializeStateL i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

kMeansL :: (Vector v, Vectorizable e v)
        => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansL i n pts t = view centroids $ kMeansL' (initializeStateL i n pts t)

kMeansL' :: (Vector v, Vectorizable e v)
         => KMeansState e v -> KMeansState e v
kMeansL' state = 
  let assignments = clusterAssignmentPhaseL state
      state1 = state  & centroids.traversed
                      %~ (\c -> centroid
                                    $ fmap toVector
                                    $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance (state^.centroids) 
                                                     (state1^.centroids))
      state3 = state2 & steps +~ 1
   in if state3^.err < state3^.threshold then state3 else kMeansL' state3

clusterAssignmentPhaseL :: (Vector v, Vectorizable e v)
                        => KMeansState e v -> M.Map v [e]
clusterAssignmentPhaseL = undefined

-- Monads

purchasesByClientId = undefined
numberItemsByPurchaseId = undefined
productIdByPurchaseId = undefined
priceByProductId = undefined

meanPurchase :: Integer -- the client identifier
             -> Double  -- the mean purchase
meanPurchase clientId = let p = purchasesByClientId clientId
                         in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n  -> case productIdByPurchaseId purchaseId of
                 Nothing   -> Nothing
                 Just prId -> case priceByProductId prId of
                                Nothing    -> Nothing
                                Just price -> Just $ (fromInteger n) * price

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing  _ = Nothing
thenDo (Just x) f = f x

purchaseValue2 :: Integer -> Maybe Double
purchaseValue2 purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
  productIdByPurchaseId purchaseId   `thenDo` (\productId ->
  priceByProductId productId         `thenDo` (\price ->
  Just $ fromInteger n * price       )))

type State s a = s -> (a, s)

thenDoS :: State s a    -> (a -> State s b)  -> State s b
-- thenDoS :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
thenDoS f g s = let (resultOfF, stateAfterF) = f s
                 in g resultOfF stateAfterF

data KMeansState2 v = KMeansState2 { centroids2 :: [v]
                                   , threshold2 :: Double
                                   , steps2 :: Int }

newCentroids2 :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids2 = M.elems . fmap (centroid . map toVector)

clusterAssignments2 :: (Vector v, Vectorizable e v)
                    => [v] -> [e] -> M.Map v [e]
clusterAssignments2 centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
   in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centrs
                      in M.adjust (p:) chosenC m)
            initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

kMeans2' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState2 v) [v]
kMeans2' points =
  (\s -> (centroids2 s,s))                           `thenDoS` (\prevCentrs  ->
  (\s -> (clusterAssignments2 prevCentrs points, s)) `thenDoS` (\assignments ->
  (\s -> (newCentroids2 assignments, s))             `thenDoS` (\newCentrs   ->
  (\s -> ((), s { centroids2 = newCentrs }))         `thenDoS` (\_           ->
  (\s -> ((), s { steps2 = steps2 s + 1 }))           `thenDoS` (\_           ->
  (\s -> (threshold2 s, s))                          `thenDoS` (\t           ->
  (\s -> (sum $ zipWith distance prevCentrs newCentrs, s)) `thenDoS` (\err  ->
  if err < t then (\s -> (newCentrs, s)) else (kMeans2' points) )))))))

initialState2 :: (Vector v, Vectorizable e v)
              => (Int -> [e] -> [v]) -> Int -> [e] -> Double
              -> KMeansState2 v
initialState2 i k pts t = KMeansState2 (i k pts) t 0

kMeans2 :: (Vector v, Vectorizable e v)
        => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans2 i k pts t = fst $ kMeans2' pts (initialState2 i k pts t)

remain :: a -> (s -> (a,s))
remain x = \s -> (x,s)

access :: (s -> a) -> (s -> (a,s))
access f = \s -> (f s, s)

modify :: (s -> s) -> (s -> ((), s))
modify f = \s -> ((), f s)

kMeans2b' :: (Vector v, Vectorizable e v)
          => [e] -> State (KMeansState2 v) [v]
kMeans2b' points =
  access centroids2                              `thenDoS` (\prevCentrs  ->
  remain (clusterAssignments2 prevCentrs points) `thenDoS` (\assignments ->
  remain (newCentroids2 assignments)             `thenDoS` (\newCentrs   ->
  modify (\s -> s { centroids2 = newCentrs })       `thenDoS` (\_           ->
  modify (\s -> s { steps2 = steps2 s + 1 })         `thenDoS` (\_           ->
  access threshold2                                 `thenDoS` (\t           ->
  remain (sum $ zipWith distance prevCentrs newCentrs) `thenDoS` (\err     ->
  if err < t then remain newCentrs else kMeans2b' points )))))))

-- do notation

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId
  = do n         <- numberItemsByPurchaseId purchaseId
       productId <- productIdByPurchaseId purchaseId
       price     <- priceByProductId productId
       return $ fromInteger n * price

-- State + lenses

data KMeansState3 v = KMeansState3 { _centroids3 :: [v]
                                   , _threshold3 :: Double
                                   , _steps3 :: Int }
makeLenses ''KMeansState3

kMeans3' :: (Vector v, Vectorizable e v)
         => [e] -> S.State (KMeansState3 v) [v]
kMeans3' points = do prevCentrs  <- use centroids3
                     let assignments = clusterAssignments2 prevCentrs points
                         newCentrs = newCentroids2 assignments
                     centroids3 .= newCentrs
                     steps3     += 1
                     let err = sum $ zipWith distance prevCentrs newCentrs
                     t <- use threshold3
                     if err < t then return newCentrs else kMeans3' points

-- Reader monad
data Settings e v = Settings { i :: Int -> [e] -> [v], k :: Int
                             , th :: Double, user :: Person }

kMeansMain :: (Vector v, Vectorizable e v)
           => [e] -> R.Reader (Settings e v) [v]
kMeansMain points = do i' <- R.asks i
                       k' <- R.asks k
                       t' <- R.asks th
                       return $ kMeans i' k' points t'

compareClusters :: (Vector v, Vectorizable e v)
                => [e] -> R.Reader (Settings e v) ([v], [v])
compareClusters points = do c1 <- kMeansMain points
                            c2 <- R.local (\s -> s { k = k s + 1 })
                                          (kMeansMain points)
                            return (c1, c2)

kMeans4' :: (Vector v, Vectorizable e v)
         => [e] -> RWS.RWS Double (Sum Int) [v] ()
kMeans4' points = do prevCentrs  <- RWS.get
                     let assignments = clusterAssignments2 prevCentrs points
                         newCentrs = newCentroids2 assignments
                     RWS.put newCentrs
                     RWS.tell (Sum 1)
                     t <- RWS.ask
                     let err = sum $ zipWith distance prevCentrs newCentrs
                     unless (err < t) $ kMeans4' points


kMeans4 :: (Vector v, Vectorizable e v) 
       => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> ([v], Sum Int)
kMeans4 i n pts t = RWS.execRWS (kMeans4' pts) t (i n pts)
