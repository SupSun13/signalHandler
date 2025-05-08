-- BST.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module BST
  ( Tree(..)
  , empty
  , insert
  , fromSortedLists
  , searchFloor
  , TreeM(..)
  , runTreeM
  , insertM
  , searchM
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

-- | Параметрический BST с ключами k и значениями v
--   Теперь с экземпляром NFData для полного жесткого вычисления
data Tree k v
  = Empty
  | Node
      { left  :: Tree k v
      , key   :: k
      , val   :: v
      , right :: Tree k v
      }
  deriving (Eq, Show, Generic)

instance (NFData k, NFData v) => NFData (Tree k v) where
  rnf :: (NFData k, NFData v) => Tree k v -> ()
  rnf Empty = ()
  rnf (Node l k v r) = rnf l `seq` rnf k `seq` rnf v `seq` rnf r

-- | Пустое дерево
empty :: Tree k v
empty = Empty

-- | Вставка (k,v), дубликаты ключей игнорируем
insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Node Empty k v Empty
insert k v node@(Node{..})
  | k == key  = node
  | k <  key  = node { left  = insert k v left  }
  | otherwise = node { right = insert k v right }

-- | Сбалансированное построение из отсортированных списков ключей и значений
fromSortedLists :: [k] -> [v] -> Tree k v
fromSortedLists ks vs = build (zip ks vs)
  where
    build [] = Empty
    build xs =
      let n           = length xs `div` 2
          (ls,(k,v):rs) = splitAt n xs
      in Node
           { left  = build ls
           , key   = k
           , val   = v
           , right = build rs
           }

-- | «Floor»-поиск: выдаёт значение с наибольшим key ≤ q
searchFloor :: Ord k => k -> Tree k v -> Maybe v
searchFloor _ Empty = Nothing
searchFloor q Node{..}
  | q == key = Just val
  | q <  key = searchFloor q left
  | otherwise =
      case searchFloor q right of
        Just v' -> Just v'
        Nothing -> Just val

-- | Монада состояния для Tree k v
newtype TreeM k v a = TreeM { runTreeM :: Tree k v -> (a, Tree k v) }

instance Functor (TreeM k v) where
  fmap :: (a -> b) -> TreeM k v a -> TreeM k v b
  fmap f m = TreeM $ \s ->
    let (a, s') = runTreeM m s
    in (f a, s')

instance Applicative (TreeM k v) where
  pure :: a -> TreeM k v a
  pure x      = TreeM $ \s -> (x, s)
  
  (<*>) :: TreeM k v (a -> b) -> TreeM k v a -> TreeM k v b
  mf <*> ma   = TreeM $ \s ->
    let (f, s1) = runTreeM mf s
        (a, s2) = runTreeM ma s1
    in (f a, s2)

instance Monad (TreeM k v) where
  (>>=) :: TreeM k v a -> (a -> TreeM k v b) -> TreeM k v b
  m >>= k     = TreeM $ \s ->
    let (a, s1) = runTreeM m s
    in runTreeM (k a) s1

-- | Монадическая вставка
insertM :: Ord k => k -> v -> TreeM k v ()
insertM k v = TreeM $ \t -> ((), insert k v t)

-- | Монадический «floor»-поиск
searchM :: Ord k => k -> TreeM k v (Maybe v)
searchM q   = TreeM $ \t -> (searchFloor q t, t)

