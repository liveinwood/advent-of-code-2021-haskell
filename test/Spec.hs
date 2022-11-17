{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import qualified Data.Vector       as V
import           Day13
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = hspec $ do
  describe "foldUp2dVAt" $ before prepare2dVector $ do
    it "foldUp2dVAt 0 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [1, 0, 0]
                            ,V.fromList $ map Sum [0, 0, 1]
                            ,V.fromList $ map Sum [0, 1, 0]]

        foldUp2dVAt 0 v `shouldBe` v'

    it "foldUp2dVAt 1 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [1, 0, 0]
                            ,V.fromList $ map Sum [1, 0, 1]]

        foldUp2dVAt 1 v `shouldBe` v'

    it "foldUp2dVAt 2 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 1, 0]
                            ,V.fromList $ map Sum [1, 1, 0]]

        foldUp2dVAt 2 v `shouldBe` v'

    it "foldUp2dVAt 3 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 0, 0]
                            ,V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [0, 1, 1]]

        foldUp2dVAt 3 v `shouldBe` v'

    it "foldUp2dVAt 4 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 0, 0]
                            ,V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [0, 0, 1]
                            ,V.fromList $ map Sum [1, 0, 0]]

        foldUp2dVAt 4 v `shouldBe` v'

  describe "foldLeft2dVAt" $ before prepare2dVector2 $ do
    it "foldLeft2dVAt 0 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [0, 0, 0, 0]
                            ,V.fromList $ map Sum [0, 0, 0, 1]
                            ,V.fromList $ map Sum [0, 0, 1, 0]
                            ,V.fromList $ map Sum [0, 1, 0, 0]
                            ,V.fromList $ map Sum [1, 0, 0, 0]]

        foldLeft2dVAt 0 v `shouldBe` v'

    it "foldLeft2dVAt 1 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [0, 0, 1]
                            ,V.fromList $ map Sum [0, 0, 0]
                            ,V.fromList $ map Sum [0, 0, 1]
                            ,V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [1, 0, 0]]
        -- 0 0 1    0 0 0
        -- 0 0 0    0 0 0
        -- 0 0 0    0 0 1
        -- 0 0 0    0 1 0
        -- 0 0 0    1 0 0

        foldLeft2dVAt 1 v `shouldBe` v'

    it "foldLeft2dVAt 2 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 0]
                            ,V.fromList $ map Sum [0, 1]
                            ,V.fromList $ map Sum [0, 0]
                            ,V.fromList $ map Sum [0, 1]
                            ,V.fromList $ map Sum [1, 0]]
        -- 1 0    0 0
        -- 0 1    0 0
        -- 0 0    0 0
        -- 0 0    0 1
        -- 0 0    1 0

        foldLeft2dVAt 2 v `shouldBe` v'

    it "foldLeft2dVAt 3 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 0, 0]
                            ,V.fromList $ map Sum [0, 1, 0]
                            ,V.fromList $ map Sum [0, 0, 1]
                            ,V.fromList $ map Sum [0, 0, 0]
                            ,V.fromList $ map Sum [0, 0, 1]]
        -- 1 0 0   0 0 0
        -- 0 1 0   0 0 0
        -- 0 0 1   0 0 0
        -- 0 0 0   0 0 0
        -- 0 0 0   0 0 1

        foldLeft2dVAt 3 v `shouldBe` v'

    it "foldLeft2dVAt 4 v" $ \v -> do
        let
            v' = V.fromList [V.fromList $ map Sum [1, 0, 0, 0]
                            ,V.fromList $ map Sum [0, 1, 0, 0]
                            ,V.fromList $ map Sum [0, 0, 1, 0]
                            ,V.fromList $ map Sum [0, 0, 0, 1]
                            ,V.fromList $ map Sum [0, 0, 0, 0]]
        -- 1 0 0 0   0 0 0 0
        -- 0 1 0 0   0 0 0 0
        -- 0 0 1 0   0 0 0 0
        -- 0 0 0 1   0 0 0 0
        -- 0 0 0 0   0 0 0 0

        foldLeft2dVAt 4 v `shouldBe` v'

  describe "visibleDotCount" $ do
    it "visibleDotCount all mempty" $ do
        let
            v = V.fromList [V.fromList $ map Sum [0, 0]
                           ,V.fromList $ map Sum [0, 0]]

        visibleDotCount v `shouldBe` 0

    it "visibleDotCount all not mempty" $ do
        let
            v = V.fromList [V.fromList $ map Sum [1, 1]
                           ,V.fromList $ map Sum [1, 1]]

        visibleDotCount v `shouldBe` 4

    it "visibleDotCount some not mempty" $ do
        let
            v = V.fromList [V.fromList $ map Sum [1, 0]
                           ,V.fromList $ map Sum [0, 1]]

        visibleDotCount v `shouldBe` 2

  describe "update2dVector" $ do
    it "update2dVector <<0,0>, <0,0>> (0,0) 1" $ do
        let
            v = V.fromList [V.fromList $ map Sum [0, 0]
                           ,V.fromList $ map Sum [0, 0]]

            v' = V.fromList [V.fromList $ map Sum [1, 0]
                            ,V.fromList $ map Sum [0, 0]]

        update2dVector v (0, 0) (Sum 1) `shouldBe` v'

    it "update2dVector <<0,0>, <0,0>> (0,1) 1" $ do
        let
            v = V.fromList [V.fromList $ map Sum [0, 0]
                           ,V.fromList $ map Sum [0, 0]]

            v' = V.fromList [V.fromList $ map Sum [0, 1]
                            ,V.fromList $ map Sum [0, 0]]

        update2dVector v (0, 1) (Sum 1) `shouldBe` v'

    it "update2dVector <<0,0>, <0,0>> (1,0) 1" $ do
        let
            v = V.fromList [V.fromList $ map Sum [0, 0]
                           ,V.fromList $ map Sum [0, 0]]

            v' = V.fromList [V.fromList $ map Sum [0, 0]
                            ,V.fromList $ map Sum [1, 0]]

        update2dVector v (1, 0) (Sum 1) `shouldBe` v'

    it "update2dVector <<0,0>, <0,0>> (1,1) 1" $ do
        let
            v = V.fromList [V.fromList $ map Sum [0, 0]
                           ,V.fromList $ map Sum [0, 0]]

            v' = V.fromList [V.fromList $ map Sum [0, 0]
                            ,V.fromList $ map Sum [0, 1]]

        update2dVector v (1, 1) (Sum 1) `shouldBe` v'


  describe "readInput" $ do
    it "readInput [\"0,0\", \"3,1\", \"2,2\", \"0,3\"]" $ do
        readInput ["0,0", "3,1", "2,2", "0,3"] `shouldBe` ([(0,3), (2,2), (3,1), (0,0)], (3, 3))

  describe "make2dVector" $ do
    it "make2dVector \"0,0\n3,1\n2,2\n0,3\"" $ do
        let
            v = V.fromList [V.fromList $ map Sum [1, 0, 0, 1]
                           ,V.fromList $ map Sum [0, 0, 0, 0]
                           ,V.fromList $ map Sum [0, 0, 1, 0]
                           ,V.fromList $ map Sum [0, 1, 0, 0]]
        make2dVector "0,0\n3,1\n2,2\n0,3" `shouldBe` v


prepare2dVector :: IO (V.Vector (V.Vector (Sum Int)))
prepare2dVector = pure $ V.fromList [V.fromList $ map Sum [1, 0, 0]
                                 ,V.fromList $ map Sum [0, 1, 0]
                                 ,V.fromList $ map Sum [0, 0, 1]
                                 ,V.fromList $ map Sum [1, 0, 0]
                                 ,V.fromList $ map Sum [0, 1, 0]]

prepare2dVector2 :: IO (V.Vector (V.Vector (Sum Int)))
prepare2dVector2 = pure $ V.fromList [V.fromList $ map Sum [1, 0, 0, 0, 0]
                                  ,V.fromList $ map Sum [0, 1, 0, 0, 0]
                                  ,V.fromList $ map Sum [0, 0, 1, 0, 0]
                                  ,V.fromList $ map Sum [0, 0, 0, 1, 0]
                                  ,V.fromList $ map Sum [0, 0, 0, 0, 1]]