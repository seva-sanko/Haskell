{-# LANGUAGE ScopedTypeVariables #-}

import Lib
import Test.QuickCheck
import Control.Exception (evaluate, try, SomeException)

prop_congruentDifference :: Int -> Int -> NonZero Int -> Bool
prop_congruentDifference a b (NonZero m) =
  isCongruent a b m == ((a - b) `mod` m == 0)

prop_congruentSymmetric :: Int -> Int -> NonZero Int -> Bool
prop_congruentSymmetric a b (NonZero m) =
  isCongruent a b m == isCongruent b a m

prop_congruentEqual :: Int -> NonZero Int -> Bool
prop_congruentEqual a (NonZero m) =
  isCongruent a a m == True
  --isCongruent a a m /= isCongruent a a m

prop_zeroModulusThrows :: Int -> Int -> Property
prop_zeroModulusThrows a b = ioProperty $ do
    result <- try (evaluate (isCongruent a b 0))
    return $ case result of
        Left (_ :: SomeException) -> True
        Right _                   -> False

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized genTree
    where
      genTree 0 = return Empty
      genTree n = do
        val <- arbitrary
        left <- genTree (n `div` 2)
        right <- genTree (n `div` 2)
        return $ Node val left right

prop_emptyTreeDepth :: Bool
prop_emptyTreeDepth = treeDepth (Empty :: Tree Int) == 0

prop_singleNodeDepth :: Int -> Bool
prop_singleNodeDepth val = treeDepth (Node val Empty Empty) == 1

prop_treeDepthMax :: Int -> Tree Int -> Tree Int -> Bool
prop_treeDepthMax val l r =
  treeDepth (Node val l r) == 1 + max (treeDepth l) (treeDepth r)

prop_addNodeNotDecrease :: Tree Int -> Int -> Bool
prop_addNodeNotDecrease t v = treeDepth (addNode v t) >= treeDepth t
  where
    addNode x Empty = Node x Empty Empty
    addNode x (Node val left right) = Node val (addNode x left) right

main :: IO ()
main = do
  putStrLn " Testing isCongruent "
  quickCheck prop_congruentDifference
  quickCheck prop_congruentSymmetric
  quickCheck prop_congruentEqual
  quickCheck prop_zeroModulusThrows
  
  putStrLn "\n Testing treeDepth "
  quickCheck prop_emptyTreeDepth
  quickCheck prop_singleNodeDepth
  quickCheck prop_treeDepthMax
  quickCheck prop_addNodeNotDecrease