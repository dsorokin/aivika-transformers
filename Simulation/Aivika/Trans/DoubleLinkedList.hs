
-- |
-- Module     : Simulation.Aivika.Trans.DoubleLinkedList
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- An imperative double-linked list.
--
module Simulation.Aivika.Trans.DoubleLinkedList 
       (DoubleLinkedList, 
        listNull, 
        listCount,
        newList, 
        listInsertFirst,
        listAddLast,
        listRemoveFirst,
        listRemoveLast,
        listRemove,
        listRemoveBy,
        listContains,
        listContainsBy,
        listFirst,
        listLast,
        clearList,
        freezeList) where 

import Data.Maybe
import Data.Functor

import Control.Monad

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event

-- | A cell of the double-linked list.
data DoubleLinkedItem m a = 
  DoubleLinkedItem { itemVal  :: a,
                     itemPrev :: Ref m (Maybe (DoubleLinkedItem m a)),
                     itemNext :: Ref m (Maybe (DoubleLinkedItem m a)) }
  
-- | The 'DoubleLinkedList' type represents an imperative double-linked list.
data DoubleLinkedList m a =  
  DoubleLinkedList { listHead :: Ref m (Maybe (DoubleLinkedItem m a)),
                     listTail :: Ref m (Maybe (DoubleLinkedItem m a)), 
                     listSize :: Ref m Int }

-- | Test whether the list is empty.
listNull :: MonadRef m => DoubleLinkedList m a -> Event m Bool
{-# INLINABLE listNull #-}
listNull x =
  do head <- readRef (listHead x) 
     case head of
       Nothing -> return True
       Just _  -> return False
    
-- | Return the number of elements in the list.
listCount :: MonadRef m => DoubleLinkedList m a -> Event m Int
{-# INLINABLE listCount #-}
listCount x = readRef (listSize x)

-- | Create a new list.
newList :: MonadRef m => Simulation m (DoubleLinkedList m a)
{-# INLINABLE newList #-}
newList =
  do head <- newRef Nothing 
     tail <- newRef Nothing
     size <- newRef 0
     return DoubleLinkedList { listHead = head,
                               listTail = tail,
                               listSize = size }

-- | Insert a new element in the beginning.
listInsertFirst :: MonadRef m => DoubleLinkedList m a -> a -> Event m ()
{-# INLINABLE listInsertFirst #-}
listInsertFirst x v =
  do size <- readRef (listSize x)
     writeRef (listSize x) (size + 1)
     head <- readRef (listHead x)
     case head of
       Nothing ->
         do prev <- liftSimulation $ newRef Nothing
            next <- liftSimulation $ newRef Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeRef (listHead x) item
            writeRef (listTail x) item
       Just h ->
         do prev <- liftSimulation $ newRef Nothing
            next <- liftSimulation $ newRef head
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeRef (itemPrev h) item
            writeRef (listHead x) item

-- | Add a new element to the end.
listAddLast :: MonadRef m => DoubleLinkedList m a -> a -> Event m ()
{-# INLINABLE listAddLast #-}
listAddLast x v =
  do size <- readRef (listSize x)
     writeRef (listSize x) (size + 1)
     tail <- readRef (listTail x)
     case tail of
       Nothing ->
         do prev <- liftSimulation $ newRef Nothing
            next <- liftSimulation $ newRef Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeRef (listHead x) item
            writeRef (listTail x) item
       Just t ->
         do prev <- liftSimulation $ newRef tail
            next <- liftSimulation $ newRef Nothing
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeRef (itemNext t) item
            writeRef (listTail x) item

-- | Remove the first element.
listRemoveFirst :: MonadRef m => DoubleLinkedList m a -> Event m ()
{-# INLINABLE listRemoveFirst #-}
listRemoveFirst x =
  do head <- readRef (listHead x) 
     case head of
       Nothing ->
         error "Empty list: listRemoveFirst"
       Just h ->
         do size <- readRef (listSize x)
            writeRef (listSize x) (size - 1)
            head' <- readRef (itemNext h)
            case head' of
              Nothing ->
                do writeRef (listHead x) Nothing
                   writeRef (listTail x) Nothing
              Just h' ->
                do writeRef (itemPrev h') Nothing
                   writeRef (listHead x) head'

-- | Remove the last element.
listRemoveLast :: MonadRef m => DoubleLinkedList m a -> Event m ()
{-# INLINABLE listRemoveLast #-}
listRemoveLast x =
  do tail <- readRef (listTail x) 
     case tail of
       Nothing ->
         error "Empty list: listRemoveLast"
       Just t ->
         do size <- readRef (listSize x)
            writeRef (listSize x) (size - 1)
            tail' <- readRef (itemPrev t)
            case tail' of
              Nothing ->
                do writeRef (listHead x) Nothing
                   writeRef (listTail x) Nothing
              Just t' ->
                do writeRef (itemNext t') Nothing
                   writeRef (listTail x) tail'

-- | Return the first element.
listFirst :: MonadRef m => DoubleLinkedList m a -> Event m a
{-# INLINABLE listFirst #-}
listFirst x =
  do head <- readRef (listHead x)
     case head of
       Nothing ->
         error "Empty list: listFirst"
       Just h ->
         return $ itemVal h

-- | Return the last element.
listLast :: MonadRef m => DoubleLinkedList m a -> Event m a
{-# INLINABLE listLast #-}
listLast x =
  do tail <- readRef (listTail x)
     case tail of
       Nothing ->
         error "Empty list: listLast"
       Just t ->
         return $ itemVal t

-- | Remove the specified element from the list and return a flag
-- indicating whether the element was found and removed.
listRemove :: (Eq a, Functor m, MonadRef m) => DoubleLinkedList m a -> a -> Event m Bool
{-# INLINABLE listRemove #-}
listRemove x v = fmap isJust $ listRemoveBy x (== v)

-- | Remove an element satisfying the specified predicate and return
-- the element if found.
listRemoveBy :: MonadRef m => DoubleLinkedList m a -> (a -> Bool) -> Event m (Maybe a)
{-# INLINABLE listRemoveBy #-}
listRemoveBy x p = readRef (listHead x) >>= loop
  where loop item =
          case item of
            Nothing   -> return Nothing
            Just item ->
              do let f = p (itemVal item)
                 if not f
                   then readRef (itemNext item) >>= loop
                   else do size <- readRef (listSize x)
                           prev <- readRef (itemPrev item)
                           next <- readRef (itemNext item)
                           writeRef (listSize x) (size - 1)
                           case (prev, next) of
                             (Nothing, Nothing) ->
                               do writeRef (listHead x) Nothing
                                  writeRef (listTail x) Nothing
                             (Nothing, head' @ (Just item')) ->
                               do writeRef (itemPrev item') Nothing
                                  writeRef (listHead x) head'
                             (tail' @ (Just item'), Nothing) ->
                               do writeRef (itemNext item') Nothing
                                  writeRef (listTail x) tail'
                             (Just prev', Just next') ->
                               do writeRef (itemNext prev') (Just next')
                                  writeRef (itemPrev next') (Just prev')
                           return (Just $ itemVal item)

-- | Detect whether the specified element is contained in the list.
listContains :: (Eq a, Functor m, MonadRef m) => DoubleLinkedList m a -> a -> Event m Bool
{-# INLINABLE listContains #-}
listContains x v = fmap isJust $ listContainsBy x (== v)

-- | Detect whether an element satisfying the specified predicate is contained in the list.
listContainsBy :: MonadRef m => DoubleLinkedList m a -> (a -> Bool) -> Event m (Maybe a)
{-# INLINABLE listContainsBy #-}
listContainsBy x p = readRef (listHead x) >>= loop
  where loop item =
          case item of
            Nothing   -> return Nothing
            Just item ->
              do let f = p (itemVal item)
                 if not f
                   then readRef (itemNext item) >>= loop
                   else return $ Just (itemVal item)

-- | Clear the contents of the list.
clearList :: MonadRef m => DoubleLinkedList m a -> Event m ()
{-# INLINABLE clearList #-}
clearList q =
  do writeRef (listHead q) Nothing
     writeRef (listTail q) Nothing
     writeRef (listSize q) 0

-- | Freeze the list and return its contents.
freezeList :: MonadRef m => DoubleLinkedList m a -> Event m [a]
{-# INLINABLE freezeList #-}
freezeList x = readRef (listTail x) >>= loop []
  where loop acc Nothing     = return acc
        loop acc (Just item) = readRef (itemPrev item) >>= loop (itemVal item : acc)
  
