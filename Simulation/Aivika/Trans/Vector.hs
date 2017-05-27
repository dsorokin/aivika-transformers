
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Vector
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- An imperative vector.
--
module Simulation.Aivika.Trans.Vector
       (Vector, 
        newVector, 
        copyVector,
        vectorCount, 
        appendVector, 
        readVector, 
        writeVector,
        vectorBinarySearch,
        vectorInsert,
        vectorDeleteAt,
        vectorDeleteRange,
        vectorDelete,
        vectorDeleteBy,
        vectorIndex,
        vectorIndexBy,
        vectorContains,
        vectorContainsBy,
        freezeVector) where 

import Data.Array

import Control.Monad

import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Ref.Base.Lazy

-- | Represents a resizable vector.
data Vector m a = Vector { vectorArrayRef :: Ref m (Array Int (Ref m a)),
                           vectorCountRef :: Ref m Int, 
                           vectorCapacityRef :: Ref m Int }

-- | Create a new vector.
newVector :: MonadRef m => Simulation m (Vector m a)
{-# INLINABLE newVector #-}
newVector =
  do xs <- forM [0 .. 4 - 1] $ \i -> newRef undefined
     let arr = array (0, 4 - 1) $ zip [0..] xs
     arrRef   <- newRef $! arr
     countRef <- newRef $! 0
     capacityRef <- newRef $! 4
     return Vector { vectorArrayRef = arrRef,
                     vectorCountRef = countRef,
                     vectorCapacityRef = capacityRef }

-- | Copy the vector.
copyVector :: MonadRef m => Vector m a -> Event m (Vector m a)
{-# INLINABLE copyVector #-}
copyVector vector =
  do arr   <- readRef (vectorArrayRef vector)
     count <- readRef (vectorCountRef vector)
     xs' <-
       forM [0 .. count - 1] $ \i ->
       do x <- readRef (arr ! i)
          liftSimulation $ newRef x
     let arr' = array (0, count - 1) $ zip [0..] xs'
     arrRef'   <- liftSimulation $ newRef $! arr'
     countRef' <- liftSimulation $ newRef $! count
     capacityRef' <- liftSimulation $ newRef $! count
     return Vector { vectorArrayRef = arrRef',
                     vectorCountRef = countRef',
                     vectorCapacityRef = capacityRef' }

-- | Ensure that the vector has the specified capacity.
vectorEnsureCapacity :: MonadRef m => Vector m a -> Int -> Event m ()
{-# INLINABLE vectorEnsureCapacity #-}
vectorEnsureCapacity vector capacity =
  do capacity' <- readRef (vectorCapacityRef vector)
     when (capacity' < capacity) $
       do arr'   <- readRef (vectorArrayRef vector)
          count' <- readRef (vectorCountRef vector)
          let capacity'' = max (2 * capacity') capacity
          xs'' <-
            forM [0 .. capacity'' - 1] $ \i ->
            liftSimulation $ newRef undefined
          let arr'' = array (0, capacity'' - 1) $ zip [0..] xs''
          forM_ [0 .. count' - 1] $ \i ->
            do x <- readRef (arr' ! i)
               writeRef (arr'' ! i) x
          writeRef (vectorArrayRef vector) $! arr''
          writeRef (vectorCapacityRef vector) $! capacity''
          
-- | Return the element count.
vectorCount :: MonadRef m => Vector m a -> Event m Int
{-# INLINABLE vectorCount #-}
vectorCount vector = readRef (vectorCountRef vector)
          
-- | Add the specified element to the end of the vector.
appendVector :: MonadRef m => Vector m a -> a -> Event m ()          
{-# INLINABLE appendVector #-}
appendVector vector item =
  do count <- readRef (vectorCountRef vector)
     vectorEnsureCapacity vector (count + 1)
     arr   <- readRef (vectorArrayRef vector)
     writeRef (arr ! count) $! item
     writeRef (vectorCountRef vector) $! (count + 1)
     
-- | Read a value from the vector, where indices are started from 0.
readVector :: MonadRef m => Vector m a -> Int -> Event m a
{-# INLINABLE readVector #-}
readVector vector index =
  do arr <- readRef (vectorArrayRef vector)
     readRef (arr ! index)
          
-- | Set an array item at the specified index which is started from 0.
writeVector :: MonadRef m => Vector m a -> Int -> a -> Event m ()
{-# INLINABLE writeVector #-}
writeVector vector index item =
  do arr <- readRef (vectorArrayRef vector)
     writeRef (arr ! index) $! item

vectorBinarySearch' :: (MonadRef m, Ord a) => Array Int (Ref m a) -> a -> Int -> Int -> Event m Int
{-# INLINABLE vectorBinarySearch' #-}
vectorBinarySearch' arr item left right =
  if left > right 
  then return $ - (right + 1) - 1
  else
    do let index = (left + right) `div` 2
       curr <- readRef (arr ! index)
       if item < curr 
         then vectorBinarySearch' arr item left (index - 1)
         else if item == curr
              then return index
              else vectorBinarySearch' arr item (index + 1) right
                   
-- | Return the index of the specified element using binary search; otherwise, 
-- a negated insertion index minus one: 0 -> -0 - 1, ..., i -> -i - 1, ....
vectorBinarySearch :: (MonadRef m, Ord a) => Vector m a -> a -> Event m Int
{-# INLINABLE vectorBinarySearch #-}
vectorBinarySearch vector item =
  do arr   <- readRef (vectorArrayRef vector)
     count <- readRef (vectorCountRef vector)
     vectorBinarySearch' arr item 0 (count - 1)

-- | Return the elements of the vector in an immutable array.
freezeVector :: MonadRef m => Vector m a -> Event m (Array Int a)
{-# INLINABLE freezeVector #-}
freezeVector vector = 
  do arr   <- readRef (vectorArrayRef vector)
     count <- readRef (vectorCountRef vector)
     xs' <-
       forM [0 .. count - 1] $ \i ->
       readRef (arr ! i)
     let arr' = array (0, count - 1) $ zip [0..] xs'
     return arr'
     
-- | Insert the element in the vector at the specified index.
vectorInsert :: MonadRef m => Vector m a -> Int -> a -> Event m ()          
{-# INLINABLE vectorInsert #-}
vectorInsert vector index item =
  do count <- readRef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: vectorInsert."
     when (index > count) $
       error $
       "Index cannot be greater " ++
       "than the count: vectorInsert."
     vectorEnsureCapacity vector (count + 1)
     arr <- readRef (vectorArrayRef vector)
     forM_ [count, count - 1 .. index + 1] $ \i ->
       do x <- readRef (arr ! (i - 1))
          writeRef (arr ! i) x
     writeRef (arr ! index) $! item
     writeRef (vectorCountRef vector) $! (count + 1)
     
-- | Delete the element at the specified index.
vectorDeleteAt :: MonadRef m => Vector m a -> Int -> Event m ()
{-# INLINABLE vectorDeleteAt #-}
vectorDeleteAt vector index =
  do count <- readRef (vectorCountRef vector)
     when (index < 0) $
       error $
       "Index cannot be " ++
       "negative: vectorDeleteAt."
     when (index >= count) $
       error $
       "Index must be less " ++
       "than the count: vectorDeleteAt."
     arr <- readRef (vectorArrayRef vector)
     forM_ [index, index + 1 .. count - 2] $ \i ->
       do x <- readRef (arr ! (i + 1))
          writeRef (arr ! i) x
     writeRef (arr ! (count - 1)) undefined
     writeRef (vectorCountRef vector) $! (count - 1)

-- | Delete the specified range of elements.
vectorDeleteRange :: MonadRef m
                     => Vector m a
                     -- ^ the vector
                     -> Int
                     -- ^ the start index
                     -> Int
                     -- ^ the count of items to be removed
                     -> Event m ()
{-# INLINABLE vectorDeleteRange #-}
vectorDeleteRange vector index len =
  do count <- readRef (vectorCountRef vector)
     when (index < 0) $
       error $
       "The first index cannot be " ++
       "negative: vectorDeleteRange."
     when (index + len - 1 >= count) $
       error $
       "The last index must be less " ++
       "than the count: vectorDeleteRange."
     when (len < 0) $
       error "Negative range length: vectorDeleteRange." 
     arr <- readRef (vectorArrayRef vector)
     forM_ [index, index + 1 .. (count - len) - 1] $ \i ->
       do x <- readRef (arr ! (i + len))
          writeRef (arr ! i) x
     forM_ [(count - len) .. count - 1] $ \i ->
       writeRef (arr ! i) undefined
     writeRef (vectorCountRef vector) $! (count - len)
     
-- | Return the index of the item or -1.     
vectorIndex :: (MonadRef m, Eq a) => Vector m a -> a -> Event m Int
{-# INLINABLE vectorIndex #-}
vectorIndex vector item =
  do count <- readRef (vectorCountRef vector)
     arr   <- readRef (vectorArrayRef vector)
     let loop index =
           if index >= count
           then return $ -1
           else do x <- readRef (arr ! index)
                   if item == x
                     then return index
                     else loop $ index + 1
     loop 0
     
-- | Return an index of the item satisfying the predicate or -1.     
vectorIndexBy :: MonadRef m => Vector m a -> (a -> Bool) -> Event m Int
{-# INLINABLE vectorIndexBy #-}
vectorIndexBy vector pred =
  do count <- readRef (vectorCountRef vector)
     arr   <- readRef (vectorArrayRef vector)
     let loop index =
           if index >= count
           then return $ -1
           else do x <- readRef (arr ! index)
                   if pred x
                     then return index
                     else loop $ index + 1
     loop 0

-- | Remove the specified element and return a flag indicating
-- whether the element was found and removed.
vectorDelete :: (MonadRef m, Eq a) => Vector m a -> a -> Event m Bool
{-# INLINABLE vectorDelete #-}
vectorDelete vector item =
  do index <- vectorIndex vector item
     if index >= 0
       then do vectorDeleteAt vector index
               return True
       else return False
            
-- | Remove an element by the specified predicate and return the element if found.
vectorDeleteBy :: MonadRef m => Vector m a -> (a -> Bool) -> Event m (Maybe a)
{-# INLINABLE vectorDeleteBy #-}
vectorDeleteBy vector pred =
  do index <- vectorIndexBy vector pred
     if index >= 0
       then do a <- readVector vector index
               vectorDeleteAt vector index
               return (Just a)
       else return Nothing

-- | Detect whether the specified element is contained in the vector.
vectorContains :: (MonadRef m, Eq a) => Vector m a -> a -> Event m Bool
{-# INLINABLE vectorContains #-}
vectorContains vector item =
  do index <- vectorIndex vector item
     return (index >= 0)
            
-- | Detect whether an element satisfying the specified predicate is contained in the vector.
vectorContainsBy :: MonadRef m => Vector m a -> (a -> Bool) -> Event m (Maybe a)
{-# INLINABLE vectorContainsBy #-}
vectorContainsBy vector pred =
  do index <- vectorIndexBy vector pred
     if index >= 0
       then do a <- readVector vector index
               return (Just a)
       else return Nothing
