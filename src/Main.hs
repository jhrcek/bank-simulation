{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.Random.Strict (MonadRandom, StdGen, evalRand, getRandomR, newStdGen)
import qualified Data.List as List
import Data.Ord (comparing)
import Text.Printf (printf)

-- | Number from interval [0,1]
newtype UnitInterval
  = UnitInterval {getUnitInterval :: Double}

-- | Point in time in seconds. 0 represents start of bank's working hours.
newtype PointInTime
  = PointInTime Double
  deriving (Eq, Ord)

bankOpens :: PointInTime
bankOpens = PointInTime 0

-- | Difference between two points in time in seconds
newtype Duration
  = Duration {getDuration :: Double}
  deriving (Eq, Ord)

addDuration :: PointInTime -> Duration -> PointInTime
addDuration (PointInTime p) (Duration d) = PointInTime (p + d)

timeDiff :: PointInTime -> PointInTime -> Duration
timeDiff (PointInTime p1) (PointInTime p2) = Duration (p1 - p2)

data Customer
  = Customer
      { -- | How many seconds after the arrival of previous customer (or bank's opening time) this customer arrived
        customerArrivalDelta :: Duration,
        -- | How long does it take for the clerk to solve customer's problem
        customerProcessingTime :: Duration
      }

data CustomerType
  = Yellow
  | Red
  | Blue
  deriving (Show)

-- RANDOMA DATA GENERATION

genUnitInverval :: MonadRandom m => m UnitInterval
genUnitInverval =
  UnitInterval <$> getRandomR (0, 1)

genArrivalDelta :: MonadRandom m => m Duration
genArrivalDelta =
  arrivalDelta <$> genUnitInverval

-- The inverse of CDF modelling probabilities of arrival times from the assignment
arrivalDelta :: UnitInterval -> Duration
arrivalDelta (UnitInterval x) =
  let arrivalAlpha = 100
   in Duration $ - arrivalAlpha * log (1 - x)

genProcessingTime :: MonadRandom m => CustomerType -> m Duration
genProcessingTime customerType =
  processingTime customerType <$> genUnitInverval

processingTime :: CustomerType -> UnitInterval -> Duration
processingTime customerType (UnitInterval x) =
  Duration $ processingRho * x ** (alpha - 1) * (1 - x) ** (beta - 1)
  where
    (alpha, beta) = case customerType of
      Yellow -> (2, 5)
      Red -> (2, 2)
      Blue -> (5, 1)
    processingRho = 200

genCustomer :: MonadRandom m => CustomerType -> m Customer
genCustomer customerType =
  Customer
    <$> genArrivalDelta
    <*> genProcessingTime customerType

data QueueStats
  = QueueStats
      { -- Each customer arrival / departure changes the lenght of the queue.
        -- We represent these changes as an association list of pairs
        -- (point in time when arrival/departure happened, +1/-1 change in length of queue)
        queueEvents :: [(PointInTime, Int)],
        -- For each customer we store how long (s)he had to wait
        waitingTimes :: [Duration],
        -- Fold accumulators representing points in time when last customer arrived / left
        lastArrivedAt :: PointInTime,
        lastLeftAt :: PointInTime
      }

initQueueStats :: QueueStats
initQueueStats = QueueStats [] [] bankOpens bankOpens

calculateQueueStats :: [Customer] -> QueueStats
calculateQueueStats =
  List.foldl'
    ( \(QueueStats {queueEvents, waitingTimes, lastArrivedAt, lastLeftAt})
       (Customer {customerArrivalDelta, customerProcessingTime}) ->
          let arrivedAt = lastArrivedAt `addDuration` customerArrivalDelta
              leftAt = max arrivedAt lastLeftAt `addDuration` customerProcessingTime
              waitedFor = if arrivedAt < lastLeftAt then lastLeftAt `timeDiff` arrivedAt else Duration 0
              newQueueEvents = (arrivedAt, 1) : (leftAt, -1) : queueEvents
           in QueueStats
                newQueueEvents
                (waitedFor : waitingTimes)
                arrivedAt
                leftAt
    )
    initQueueStats

calculateQueueLengths :: [(PointInTime, Int)] -> (Int, Double)
calculateQueueLengths queueEvents = (maxLength, weightedAverageLength)
  where
    timeSortedEvents = List.sortOn fst queueEvents
    (_currentLength, maxLength, weightedSum, totalTime) =
      List.foldl'
        ( \(currentLength, maxLength', weightedSum', currentTime)
           (PointInTime eventTime, lengthDelta) ->
              let newLength = currentLength + lengthDelta
               in ( newLength,
                    max maxLength' newLength,
                    weightedSum' + fromIntegral currentLength * (eventTime - currentTime),
                    eventTime
                  )
        )
        (0 :: Int, 0 :: Int, 0 :: Double, 0 :: Double)
        timeSortedEvents
    weightedAverageLength = weightedSum / totalTime

averageDuration :: [Duration] -> Double
averageDuration ds = sum (fmap getDuration ds) / fromIntegral (length ds)

runSimulation :: StdGen -> Int -> CustomerType -> QueueStats
runSimulation gen customerSampleSize customerType =
  let randomCustomers = evalRand (replicateM customerSampleSize $ genCustomer customerType) gen
   in calculateQueueStats randomCustomers

diffMaxAndAverageWaitingTimes :: StdGen -> Int -> CustomerType -> Double
diffMaxAndAverageWaitingTimes gen sampleSize custType =
  maxWaitingTime - averageWaitingTime
  where
    QueueStats {waitingTimes} = runSimulation gen sampleSize custType
    averageWaitingTime = averageDuration waitingTimes
    maxWaitingTime = getDuration $ maximum waitingTimes

main :: IO ()
main = do
  gen <- newStdGen
  let sampleSizes = [10, 100, 1000, 10000]
  putStrLn "Yellow customers only: what are the average and maximum customer waiting times?"
  forM_ sampleSizes $ \sampleSize -> do
    let QueueStats {waitingTimes} = runSimulation gen sampleSize Yellow
        averageWaitingTime = averageDuration waitingTimes
        maxWaitingTime = getDuration $ maximum waitingTimes
    printf
      "    sample size = %5d, average waiting time = %.2f s, maximum waiting time = %.2f s\n"
      sampleSize
      averageWaitingTime
      maxWaitingTime
  putStrLn "Red customers only: what are the average and maximum queue lengths in-front of the teller?"
  forM_ sampleSizes $ \sampleSize -> do
    let queueStats = runSimulation gen sampleSize Red
        (maxQuLen, avgQuLen) = calculateQueueLengths $ queueEvents queueStats
    printf
      "    sample size = %5d, average queue length = %.2f, maximum queue length = %d\n"
      sampleSize
      avgQuLen
      maxQuLen
  putStrLn "Which type of customer gives the gives the closest value between the average and maximum customer waiting times?"
  forM_ sampleSizes $ \sampleSize -> do
    let (customerType, minDiff) =
          List.minimumBy (comparing snd) $
            fmap
              (\custType -> (custType, diffMaxAndAverageWaitingTimes gen sampleSize custType))
              [Red, Blue, Yellow]
    printf
      "    sample size = %5d, The difference was minimal for %s customers: %.2f s\n"
      sampleSize
      (show customerType)
      minDiff
