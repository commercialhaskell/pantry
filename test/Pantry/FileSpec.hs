module Pantry.FileSpec
  ( spec
  ) where

import           Control.Monad ( void )
import           Pantry
import           Path
import           Path.IO
import           Test.Hspec

spec :: Spec
spec = describe "loadCabalFilePath" $ do
  it "sanity" $ do
    abs' <- resolveDir' "."
    (f, name, cabalfp) <- runPantryApp $ loadCabalFilePath Nothing abs'
    suffix <- parseRelFile "pantry.cabal"
    cabalfp `shouldBe` abs' </> suffix
    name' <- parsePackageNameThrowing "pantry"
    name `shouldBe` name'
    void $ f NoPrintWarnings
