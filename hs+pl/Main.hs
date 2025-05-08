-- Main.hs
module Main where

import BST
import System.CPUTime        (getCPUTime)
import Control.DeepSeq       (NFData, deepseq)
import Control.Monad         (forM_)
import Prelude hiding        (lookup)

-- | Простейший тестовый раннер без HUnit
assert :: Bool -> String -> IO ()
assert cond msg =
  putStrLn $ msg ++ ": " ++ if cond then "OK" else "FAILED"

-- | Запуск тестов
runTests :: IO ()
runTests = do
  putStrLn "\n=== Running tests ==="

  let t1 = snd $ runTreeM (insertM 0 3) empty
  assert (searchFloor 0 t1 == Just 3)
         "Test1: insertM 0→3, searchFloor 0 == Just 3"

  let t2 = snd $ runTreeM (do insertM 1 5; insertM 1 7) empty
  assert (searchFloor 1 t2 == Just 5)
         "Test2: duplicate key keeps first value 5"

  let ops = do insertM 2 4
               insertM 0 3
               insertM 1 5
      t3 = snd $ runTreeM ops empty
  assert (searchFloor 1 t3 == Just 5)
         "Test3: sequence insertM and floor-search"

-- | Измеряем время действия (в наносекундах)
timeNano :: NFData a => a -> IO Integer
timeNano action = do
  start <- getCPUTime
  action `deepseq` return ()  -- форсируем полную оценку
  end   <- getCPUTime
  return ((end - start) `div` 1000)  -- CPUTime в пико

-- | Бенчмарк
runBench :: IO ()
runBench = do
  putStrLn "\n=== Benchmark ==="
  let sizes :: [Int]
      sizes = [10,100,1000,10000]
  forM_ sizes $ \n -> do
    putStrLn $ "\n-- n = " ++ show n ++ " --"
    let ks    = [0..n-1]
        vs    = ks
        tree0 = fromSortedLists ks vs
        q     = n `div` 3
        newK  = n
        newV  = n * 10

    tIns <- timeNano (insert newK newV tree0)
    putStrLn $ "insert     : " ++ show tIns ++ " ns"

    tSr  <- timeNano (searchFloor q tree0)
    putStrLn $ "searchFloor: " ++ show tSr  ++ " ns"

main :: IO ()
main = do
  runTests
  runBench
