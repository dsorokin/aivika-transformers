
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
        listFirst,
        listLast) where 

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
