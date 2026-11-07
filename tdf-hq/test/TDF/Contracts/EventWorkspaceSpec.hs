{-# LANGUAGE OverloadedStrings #-}
module TDF.Contracts.EventWorkspaceSpec (spec) where
import Control.Exception (AsyncException(ThreadKilled), throwIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Servant (getResponse, getHeaders, errHTTPCode, errHeaders, errBody)
import Servant.Server.Internal.Handler (runHandler)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Test.Hspec
import TDF.Contracts.EventWorkspace (escapeDocumentText, workspaceDocumentLines, workspacePdfResponse)

spec :: Spec
spec = describe "private event document rendering" $ do
  it "renders contractual text literally instead of executing embedded TeX" $ do
    escapeDocumentText "50% & \\input{private} #_" `shouldBe`
      "50\\% \\& \\textbackslash{}input\\{private\\} \\#\\_"
  it "retains the full scope, unknown costs, deliverables and pending work" $ do
    let content = object ["title" .= ("Session" :: T.Text), "overview" .= ("Original scope" :: T.Text),
          "currency" .= ("USD" :: T.Text), "budgetCapCents" .= (Nothing :: Maybe Int),
          "budget" .= [object ["label" .= ("Audio" :: T.Text), "amountCents" .= (Nothing :: Maybe Int), "basis" .= ("To quote" :: T.Text)]],
          "activities" .= ([] :: [T.Text]), "deliverables" .= ["Audio master" :: T.Text], "pending" .= ["Confirm duration" :: T.Text]]
        rendered = T.unlines (workspaceDocumentLines content)
    mapM_ (\text -> rendered `shouldSatisfy` T.isInfixOf text)
      ["Original scope", "Por cotizar", "Audio master", "Confirm duration", "Pendiente de aprobación"]

  it "maps renderer IOException to a private generic 503 response" $ do
    result <- runHandler (workspacePdfResponse (\_ -> ioError (userError "synthetic/private/tool/path")) "plan.pdf" ["Synthetic plan"])
    case result of
      Left failure -> do
        errHTTPCode failure `shouldBe` 503
        lookup "Cache-Control" (errHeaders failure) `shouldBe` Just "no-store"
        BL.unpack (errBody failure) `shouldNotContain` "synthetic/private/tool/path"
      Right _ -> expectationFailure "renderer failure must not return a PDF"
  it "keeps explicit renderer failure private and unavailable" $ do
    result <- runHandler (workspacePdfResponse (\_ -> pure (Left "synthetic/private/stderr")) "plan.pdf" [])
    case result of
      Left failure -> do
        errHTTPCode failure `shouldBe` 503
        lookup "Cache-Control" (errHeaders failure) `shouldBe` Just "no-store"
        BL.unpack (errBody failure) `shouldNotContain` "synthetic/private/stderr"
      Right _ -> expectationFailure "renderer failure must not return a PDF"
  it "preserves successful PDF bytes and private attachment headers" $ do
    result <- runHandler (workspacePdfResponse (\_ -> pure (Right "%PDF synthetic")) "plan.pdf" ["Synthetic plan"])
    case result of
      Left failure -> expectationFailure (show failure)
      Right response -> do
        getResponse response `shouldBe` "%PDF synthetic"
        lookup "Cache-Control" (getHeaders response) `shouldBe` Just "private, no-store"
        lookup "Content-Disposition" (getHeaders response) `shouldBe` Just "attachment; filename=\"plan.pdf\""
  it "does not convert asynchronous cancellation into an unavailable response" $
    runHandler (workspacePdfResponse (\_ -> throwIO ThreadKilled) "plan.pdf" []) `shouldThrow` (== ThreadKilled)
