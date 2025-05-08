{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.State (State, modify, execState)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData(..), deepseq)
import Prelude hiding (lookup)

-----------------------------------------
-- Реализация AVL-дерева
-----------------------------------------

-- Определение AVL-дерева
data AVLTree k v = EmptyAVL 
                 | NodeAVL 
                   { key :: k
                   , value :: v
                   , left :: AVLTree k v
                   , right :: AVLTree k v
                   , height :: Int 
                   } 
                 deriving (Eq)

-- Функция для вычисления коэффициента балансировки
balanceFactor :: AVLTree k v -> Int
balanceFactor EmptyAVL = 0  -- Пустое дерево сбалансировано (коэффициент = 0)
balanceFactor (NodeAVL _ _ l r _) = 
    heightAVL l - heightAVL r  -- Разница высот левого и правого поддеревьев

-- Высота поддерева (удобная обёртка)
heightAVL :: AVLTree k v -> Int
heightAVL EmptyAVL = 0
heightAVL (NodeAVL _ _ _ _ h) = h

-- Поворот направо
rotateRight :: AVLTree k v -> AVLTree k v
rotateRight (NodeAVL k v l r _) =
    let newRight = NodeAVL k v (right l) r (1 + max (height (right l)) (height r))
    in NodeAVL (key l) (value l) (left l) newRight (1 + max (height (left l)) (height newRight))

-- Поворот налево
rotateLeft :: AVLTree k v -> AVLTree k v
rotateLeft (NodeAVL k v l r _) =
    let newLeft = NodeAVL k v l (left r) (1 + max (height l) (height (left r)))
    in NodeAVL (key r) (value r) newLeft (right r) (1 + max (height newLeft) (height (right r)))

-- Балансировка дерева после вставки
balance :: AVLTree k v -> AVLTree k v
balance node
    | bf > 1 && (balanceFactor (left node) >= 0) = rotateRight node
    | bf < -1 && (balanceFactor (right node) <= 0) = rotateLeft node
    | bf > 1 = rotateRight $ node { left = rotateLeft (left node) }
    | bf < -1 = rotateLeft $ node { right = rotateRight (right node) }
    | otherwise = node
  where
    bf = balanceFactor node

-- Вставка элемента с автоматической балансировкой
insertAVL :: Ord k => k -> v -> AVLTree k v -> AVLTree k v
insertAVL k v EmptyAVL = NodeAVL k v EmptyAVL EmptyAVL 1
insertAVL k v node
    | k < key node = balance $ node { left = insertAVL k v (left node) }
    | k > key node = balance $ node { right = insertAVL k v (right node) }
    | otherwise = node { value = v }  -- Обновление значения

-- Поиск последнего значения ≤ запрошенному времени
lookupAVL :: Ord k => k -> AVLTree k v -> Maybe v
lookupAVL queryTime = go Nothing
  where
    go best EmptyAVL = best
    go best (NodeAVL k v l r _)
        | queryTime < k = go best l
        | otherwise     = go (Just v) r

-----------------------------------------
-- Поддержка вывода и строгой оценки
-----------------------------------------

instance (Show k, Show v) => Show (AVLTree k v) where
    show EmptyAVL = "Empty"
    show (NodeAVL k v l r _) = 
        "Node " ++ show k ++ " " ++ show v ++ 
        " (" ++ show l ++ ") (" ++ show r ++ ")"

instance (NFData k, NFData v) => NFData (AVLTree k v) where
    rnf EmptyAVL = ()
    rnf (NodeAVL k v l r _) = rnf k `seq` rnf v `seq` rnf l `seq` rnf r

-----------------------------------------
-- Монадический интерфейс
-----------------------------------------

type AVLTreeMonad k v a = State (AVLTree k v) a

addAVL :: Ord k => k -> v -> AVLTreeMonad k v ()
addAVL k v = modify (insertAVL k v)

-----------------------------------------
-- Тестирование
-----------------------------------------

assert :: String -> Bool -> IO ()
assert msg condition = putStrLn $ if condition then "✓ " ++ msg else "✗ " ++ msg

testAVLLookup :: IO ()
testAVLLookup = do
    let tree = foldl (\t (k, v) -> insertAVL k v t) EmptyAVL [(10, 100), (20, 200)]
    assert "Поиск 15 → 100" (lookupAVL 15 tree == Just 100)
    assert "Поиск 25 → 200" (lookupAVL 25 tree == Just 200)
    assert "Поиск 5 → Nothing" (lookupAVL 5 tree == Nothing)

-----------------------------------------
-- Замер производительности
-----------------------------------------

measureAVLTime :: Int -> IO ()
measureAVLTime n = do
    let elements = [(i, i) | i <- [1..n]]
    let tree = foldl (\t (k, v) -> insertAVL k v t) EmptyAVL elements
    start <- getCPUTime
    let !_ = insertAVL (n+1) (n+1) tree `deepseq` ()
    end <- getCPUTime
    putStrLn $ "AVL n=" ++ show n ++ ", time: " ++ show (fromIntegral (end - start) / 1e12) ++ "s"

-----------------------------------------
-- Главная функция
-----------------------------------------

main :: IO ()
main = do
    putStrLn "Запуск тестов:"
    testAVLLookup
    
    putStrLn "\nЗамеры времени вставки:"
    mapM_ measureAVLTime [1000, 10000, 100000]
    
    putStrLn "\nПример дерева:"
    let finalTree = execState (addAVL 10 100 >> addAVL 20 200 >> addAVL 30 300) EmptyAVL
    print finalTree