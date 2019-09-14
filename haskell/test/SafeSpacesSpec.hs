module SafeSpacesSpec where

import Test.Hspec
import SafeSpaces (convertCoordinates, findSafeSpaces, adviceForAlex, Advice(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Convert coordinates" $ do
        it "returns empty numeric coordinates for empty alphanumeric coordinates" $ do
            convertCoordinates [] `shouldBe` []
        
        it "converts a single coordinate" $ do
            convertCoordinates ["F3"] `shouldBe` [(5, 2)]
        
        it "converts a list of coordinates to an array" $ do
            convertCoordinates ["B6","C2","J7"] `shouldBe` [(1, 5), (2, 1), (9, 6)]

        it "handles alphanumeric coordinates with a double digit number" $ do
            convertCoordinates ["J10"] `shouldBe` [(9, 9)]

    describe "Find safe spaces in the city based on agent locations" $ do
        it "returns correct result for six agents at specified locations" $ do
            findSafeSpaces [(1, 1), (3, 5), (4, 8), (7, 3), (7, 8), (9, 1)] `shouldBe` [(0, 9), (0, 7), (5, 0)]
            findSafeSpaces [(0, 0), (0, 9), (1, 5), (5, 1), (9, 0), (9, 9)] `shouldBe` [(5, 7), (6, 6), (7, 5)]
            findSafeSpaces [(0, 0)] `shouldBe` [(9, 9)]

    describe "Handle edge cases and offering recommendations" $ do
        it "works for no safe locations" $ do
            adviceForAlex [ "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"
                            , "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10"
                            , "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10"
                            , "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"
                            , "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10"
                            , "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10"
                            , "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10"
                            , "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10"
                            , "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10"
                            , "J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "J9", "J10"
                            ] `shouldBe` NoSafeSpaces

        it "works for six agents at specified locations" $ do
            adviceForAlex [ "B2", "D6", "E9", "H4", "H9", "J2" ]
                `shouldBe` SafeSpaces [ "A10", "A8", "F1" ]

        it "works for seven agents at specified locations" $ do
            adviceForAlex [ "B4", "C4", "C8", "E2", "F10", "H1", "J6" ]
                `shouldBe` SafeSpaces [ "A1", "A10", "E6", "F5", "F6", "G4", "G5","G7", "H8", "I9", "J10" ]

        it "works for six agents at different specified locations" $ do
            adviceForAlex [ "A1", "A10", "B6", "F2", "J1", "J10" ]
                `shouldBe` SafeSpaces [ "F8", "G7", "H6" ]

        it "works for only one agent" $ do
            adviceForAlex [ "A1" ] `shouldBe` SafeSpaces [ "J10" ]

        it "works for agent outside the map (no agent in the city)" $ do
            adviceForAlex [ "A12" ] `shouldBe` WholeCityIsSafe
