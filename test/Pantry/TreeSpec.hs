{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Pantry.TreeSpec
  ( spec
  ) where

import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Types.Version ( mkVersion )
import           Pantry
import qualified Pantry.SHA256 as SHA256
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  let tarURL = "https://github.com/snoyberg/file-embed/archive/47b499c3c58ca465c56ee0295d0a76782a66751d.tar.gz"
      zipURL = "https://github.com/snoyberg/file-embed/archive/47b499c3c58ca465c56ee0295d0a76782a66751d.zip"
      emptyPM = RawPackageMetadata
        { rpmName = Nothing
        , rpmVersion = Nothing
        , rpmTreeKey = Nothing
        }
      mkArchive url =
        RPLIArchive
          RawArchive
            { raLocation = ALUrl url
            , raHash = Nothing
            , raSize = Nothing
            , raSubdir = ""
            }
          emptyPM
      tarPL = mkArchive tarURL
      zipPL = mkArchive zipURL
      gitPL =
          RPLIRepo
            Repo
              { repoUrl = "https://github.com/snoyberg/file-embed.git"
              , repoCommit = "47b499c3c58ca465c56ee0295d0a76782a66751d"
              , repoType = RepoGit
              , repoSubdir = ""
              }
            emptyPM
      hgPL =
          RPLIRepo
            Repo
              { repoUrl = "https://bitbucket.org/snoyberg/file-embed"
              , repoCommit = "6d8126e7a4821788a0275fa7c2c4a0083e14d690"
              , repoType = RepoHg
              , repoSubdir = ""
              }
            emptyPM

  it "zip and tar.gz archives match" $ asIO $ runPantryAppClean $ do
    pair1 <- loadPackageRaw tarPL
    pair2 <- loadPackageRaw zipPL
    liftIO $ pair2 `shouldBe` pair1
  it "archive and Git repo match" $ asIO $ runPantryAppClean $ do
    pair1 <- loadPackageRaw tarPL
    pair2 <- loadPackageRaw gitPL
    liftIO $ pair2 `shouldBe` pair1
  -- https://github.com/commercialhaskell/pantry/issues/26
  xit "archive and Hg repo match" $ asIO $ runPantryAppClean $ do
    pair1 <- loadPackageRaw tarPL
    pair2 <- loadPackageRaw hgPL
    liftIO $ pair2 `shouldBe` pair1

  it "5045 no cabal file" $ asIO $ runPantryAppClean $ do
    let rpli = RPLIArchive archive rpm
        packageName = mkPackageName "yaml"
        version = mkVersion [0, 11, 1, 2]
        archive =
            RawArchive
              { raLocation = ALUrl "https://github.com/snoyberg/yaml/archive/yaml-0.11.1.2.tar.gz"
              , raHash = either impureThrow Just
                         $ SHA256.fromHexBytes "b8564e99c555e670ee487bbf92d03800d955f0e6e16333610ef46534548e0a3d"
              , raSize = Just $ FileSize 94198
              , raSubdir = "yaml"
              }
        rpm =
            RawPackageMetadata
              { rpmName = Just packageName
              , rpmVersion = Just version
              , rpmTreeKey = Nothing
              }
    void $ loadCabalFileRawImmutable rpli
