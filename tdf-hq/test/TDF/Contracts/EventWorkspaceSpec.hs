{-# LANGUAGE OverloadedStrings #-}
module TDF.Contracts.EventWorkspaceSpec (spec) where
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Test.Hspec
import TDF.Contracts.EventWorkspace (escapeDocumentText, workspaceDocumentLines)

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
