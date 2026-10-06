{-# LANGUAGE OverloadedStrings #-}
module TDF.ContractStorageSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import Data.Either (isLeft, isRight)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus)
import Data.Bits ((.&.))
import Test.Hspec
import TDF.Contracts.Storage (storeContractBytes, readContractBytes)

spec :: Spec
spec = describe "private persistent contract storage" $ do
  let identifier = "550e8400-e29b-41d4-a716-446655440000"
      name = "550e8400-e29b-41d4-a716-446655440000.json"
      current root = root </> "uploads" </> "contracts" </> name
      legacy root = root </> "contracts" </> "store" </> name
      old root bytes = do
        createDirectoryIfMissing True (root </> "contracts" </> "store")
        BL.writeFile (legacy root) bytes
      new root bytes = do
        createDirectoryIfMissing True (root </> "uploads" </> "contracts")
        BL.writeFile (current root) bytes
  it "reopens complete private documents in the persistent uploads tree" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      storeContractBytes root identifier "contract bytes"
      readContractBytes root identifier `shouldReturn` Just "contract bytes"
      doesFileExist (current root) `shouldReturn` True
      BL.readFile (current root) `shouldReturn` "contract bytes"
      doesDirectoryExist (root </> "contracts") `shouldReturn` False
      info <- getFileStatus (current root)
      fileMode info .&. 0o777 `shouldBe` 0o600
      directory <- getFileStatus (root </> "uploads" </> "contracts")
      fileMode directory .&. 0o777 `shouldBe` 0o700
  it "reads legacy-only documents without migrating or deleting them" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      old root "legacy"
      readContractBytes root identifier `shouldReturn` Just "legacy"
      doesDirectoryExist (root </> "uploads") `shouldReturn` False
      BL.readFile (legacy root) `shouldReturn` "legacy"
  it "returns absence only when both documents are absent" $
    withSystemTempDirectory "tdf-contract" $ \root ->
      readContractBytes root identifier `shouldReturn` Nothing
  it "rejects differing copies including corrupt current bytes" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      old root "valid legacy"
      new root "{incomplete"
      readContractBytes root identifier `shouldThrow` anyIOException
      BL.readFile (current root) `shouldReturn` "{incomplete"
      BL.readFile (legacy root) `shouldReturn` "valid legacy"
  it "allows identical copies without changing either" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      old root "same"; new root "same"
      readContractBytes root identifier `shouldReturn` Just "same"
  it "does not overwrite an existing current document even for identical creation" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      storeContractBytes root identifier "original"
      storeContractBytes root identifier "different" `shouldThrow` anyIOException
      storeContractBytes root identifier "original" `shouldThrow` anyIOException
      readContractBytes root identifier `shouldReturn` Just "original"
  it "does not shadow an existing legacy identifier during creation" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      old root "legacy"
      storeContractBytes root identifier "replacement" `shouldThrow` anyIOException
      doesDirectoryExist (root </> "uploads") `shouldReturn` False
  it "admits one concurrent creator and preserves that complete result" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      let attempt bytes = try (storeContractBytes root identifier bytes) :: IO (Either IOException ())
      (first, second) <- concurrently (attempt "first") (attempt "second")
      length (filter isRight [first,second]) `shouldBe` 1
      length (filter isLeft [first,second]) `shouldBe` 1
      readContractBytes root identifier `shouldReturn` Just (if isRight first then "first" else "second")
  it "rejects symlink current storage rather than falling back to legacy" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      old root "legacy"
      createDirectoryIfMissing True (root </> "uploads" </> "contracts")
      createSymbolicLink (legacy root) (current root)
      readContractBytes root identifier `shouldThrow` anyIOException
  it "rejects invalid or noncanonical identifiers before any storage effect" $
    withSystemTempDirectory "tdf-contract" $ \root -> do
      mapM_ (\bad -> do
        storeContractBytes root bad "payload" `shouldThrow` anyIOException
        readContractBytes root bad `shouldThrow` anyIOException)
        ["../outside", "550E8400-E29B-41D4-A716-446655440000", "00000000-0000-0000-0000-000000000000"]
      doesDirectoryExist (root </> "uploads") `shouldReturn` False
