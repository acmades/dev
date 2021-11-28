module Phoityne.Example.FuncSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Go go go" $ do
    it "Start!" $ do
      True `shouldBe` True

  {-describe "isTriangle" $ do
    it "it works" $ do
      isTriangle 1 2 2 `shouldBe` True
      isTriangle 7 2 2 `shouldBe` False
      isTriangle 1 3 2 `shouldBe` False
      isTriangle 3 1 2 `shouldBe` False
      isTriangle 1 2 3 `shouldBe` False
      isTriangle 5 1 2 `shouldBe` False
      isTriangle 1 2 5 `shouldBe` False
      isTriangle 2 5 1 `shouldBe` False
      isTriangle 4 2 3 `shouldBe` True
      isTriangle 2 2 2 `shouldBe` True
      isTriangle 5 1 5 `shouldBe` True
      isTriangle (-1) 2 3 `shouldBe` False
      isTriangle 1 (-2) 3 `shouldBe` False
      isTriangle 1 2 (-3) `shouldBe` False
      isTriangle 0 2 3 `shouldBe` False
      
  it "disemvowel" $ do
    disemvowel "hat" `shouldBe` "ht"
    disemvowel "toast" `shouldBe` "tst"
  it "disemvowel" $ do
    disemvowel "toast hat" `shouldBe` "tst ht"

  describe "highAndLow" $ do
      it "SomeTest" $ highAndLow "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" `shouldBe`  "542 -214"
      it "SortTest" $ highAndLow "10 2 -2 -10" `shouldBe`  "10 -10"
      it "PlusMinusTest" $ highAndLow "1 -1" `shouldBe`  "1 -1"
      it "PlusPlusTest" $ highAndLow "1 1" `shouldBe`  "1 1"
      it "MinusMinusTest" $ highAndLow "-1 -1" `shouldBe`  "-1 -1"
      it "PlusMinusZeroTest" $ highAndLow "1 -1 0" `shouldBe`  "1 -1"
      it "PlusPlusZeroTest" $ highAndLow "1 1 0" `shouldBe`  "1 0"
      it "MinusMinusZeroTest" $ highAndLow "-1 -1 0" `shouldBe`  "0 -1"
      it "SingleTest" $ highAndLow "42" `shouldBe`  "42 42"
      
  describe "isIsogram" $ do
    it "testing 'Dermatoglyphics'" $ shouldBe (isIsogram "Dermatoglyphics") True
    it "testing 'moose'" $ shouldBe (isIsogram "moose") False
    it "testing 'aba'" $ shouldBe (isIsogram "aba") False
    it "testing 'moOse'" $ shouldBe (isIsogram "moOse") False
    it "testing 'thumbscrewjapingly'" $ shouldBe (isIsogram "thumbscrewjapingly") True
    it "testing 'squat'" $ shouldBe (isIsogram "squat") True
    it "testing 'letterhead'" $ shouldBe (isIsogram "letterhead") False
    it "testing the empty string" $ shouldBe (isIsogram "") True

  describe "solution" $ do
    it "gives the correct result for \"a\"" $
      solution "a" `shouldBe` ["a_"]
    it "gives the correct result for \"abc\"" $
      solution "abc" `shouldBe` ["ab", "c_"]
    it "gives the correct result for \"abcdef\"" $
      solution "abcdef" `shouldBe` ["ab", "cd", "ef"]
  
  it "tribonacci" $ do
    tribonacci (1, 1, 1) 10 `shouldBe` [1,1,1,3,5,9,17,31,57,105]
    tribonacci (0, 0, 1) 10 `shouldBe` [0,0,1,1,2,4,7,13,24,44]
    tribonacci (0, 1, 1) 10 `shouldBe` [0,1,1,2,4,7,13,24,44,81]
    tribonacci (1, 0, 0) 10 `shouldBe` [1,0,0,1,1,2,4,7,13,24]
    tribonacci (0, 0, 0) 10 `shouldBe` [0,0,0,0,0,0,0,0,0,0]
    tribonacci (1, 2, 3) 10 `shouldBe` [1,2,3,6,11,20,37,68,125,230]
    tribonacci (3, 2, 1) 10 `shouldBe` [3,2,1,6,9,16,31,56,103,190]
    tribonacci (1, 1, 1) 1 `shouldBe` [1]
    tribonacci (300, 200, 100) 0 `shouldBe` []
  
  it "titleCase" $ do
    titleCase "" "" `shouldBe` ""
    titleCase "" "aBC deF Ghi" `shouldBe` "Abc Def Ghi"
    titleCase "ab" "ab" `shouldBe` "Ab"
    titleCase "bc" "a bc" `shouldBe` "A bc"
    titleCase "BC" "a bc" `shouldBe` "A bc"
    titleCase "an often into" "First a of in" `shouldBe` "First A Of In"
    titleCase "a an the OF" "a clash of KINGS" `shouldBe` "A Clash of Kings"
    titleCase "xyz fox quick the" "the QUICK bRoWn fOX" `shouldBe` "The quick Brown fox"-}

  it "songDecoder" $ do
    songDecoder "AWUBBWUBC" `shouldBe` "A B C"
    songDecoder "AWUBWUBWUBBWUBWUBWUBC" `shouldBe` "A B C"
    songDecoder "WUBAWUBBWUBCWUB" `shouldBe` "A B C"
    songDecoder "AWUBAWUBA" `shouldBe` "A A A"
    songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
      `shouldBe` "WE ARE THE CHAMPIONS MY FRIEND"
    songDecoder "NEVERWUBWUBGONNAWUBGIVEWUBWUBYOUWUBWUBUPWUBWUBNEVERWUBWUBWUBWUBGONNAWUBWUBLETWUBWUBYOUWUBWUBDOWN"
      `shouldBe` "NEVER GONNA GIVE YOU UP NEVER GONNA LET YOU DOWN"  