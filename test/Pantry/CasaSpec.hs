{-# LANGUAGE OverloadedStrings #-}
module Pantry.CasaSpec (spec) where

import Distribution.Types.Version (mkVersion)
import Pantry
import Pantry.SHA256
import Test.Hspec

spec :: Spec
spec = do
  loadHackagePackageSpec
  completeSpec

completeSpec :: Spec
completeSpec =
  it
    "completePackageLocation: unliftio_0_2_12"
    (shouldReturn
       (runPantryAppClean
          (completePackageLocation (argsRlpi unliftio_0_2_12)))
       ( PLIHackage
           (PackageIdentifier
              { pkgName = "unliftio"
              , pkgVersion = mkVersion [0, 2, 12]
              })
           (argsCabalKey unliftio_0_2_12)
           (argsTreeKey unliftio_0_2_12)))

loadHackagePackageSpec :: Spec
loadHackagePackageSpec = do
  it
    "loadPackageRaw Exact hackage lookup"
    (shouldReturn
       (fmap
          packageTreeKey
          (runPantryAppClean (loadPackageRaw (argsRlpi unliftio_0_2_12))))
       (argsTreeKey unliftio_0_2_12))
  it
    "loadHackagePackageRaw Exact hackage lookup"
    (shouldReturn
       (fmap
          packageTreeKey
          (runPantryAppClean
             (loadHackagePackageRaw
                (argsRlpi unliftio_0_2_12)
                (argsRevision unliftio_0_2_12)
                (Just (argsTreeKey unliftio_0_2_12)))))
       (argsTreeKey unliftio_0_2_12))
  it
    "loadHackagePackageRawViaCasa Exact hackage lookup"
    (shouldReturn
       (fmap
          (fmap packageTreeKey)
          (runPantryAppClean
             (loadHackagePackageRawViaCasa
                (argsRlpi unliftio_0_2_12)
                (argsTreeKey unliftio_0_2_12))))
       (Just (argsTreeKey unliftio_0_2_12)))

data Args =
  Args
    { argsRlpi :: !RawPackageLocationImmutable
    , argsTreeKey :: !TreeKey
    , argsRevision :: !PackageIdentifierRevision
    , argsCabalKey :: !BlobKey
    }

unliftio_0_2_12 :: Args
unliftio_0_2_12 =
  let cabalHash = (either
                     (error . show)
                     id
                     (fromHexText
                        "b089fbc2ff2628a963c2c4b12143f2020874e3e5144ffd6c62b25639a0ca1483"))
      cabalLen = FileSize 3325
      cabalFileHash =
        CFIHash
          cabalHash
          (Just cabalLen)
      casaTreeKey =
        TreeKey
          (BlobKey
             (either
                (error . show)
                id
                (fromHexText
                   "4971b43f3d473eff868eb1a0c359729b49f1779e78c462ba45ef0d1eda677699"))
             (FileSize 2229))
      pir =
        PackageIdentifierRevision
          "unliftio"
          (mkVersion [0, 2, 12])
          cabalFileHash
   in Args
        { argsRevision = pir
        , argsRlpi = RPLIHackage pir (Just casaTreeKey)
        , argsTreeKey = casaTreeKey
        , argsCabalKey = BlobKey cabalHash cabalLen
        }
