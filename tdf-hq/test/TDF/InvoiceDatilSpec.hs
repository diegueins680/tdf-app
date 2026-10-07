{-# LANGUAGE OverloadedStrings #-}

module TDF.InvoiceDatilSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import           Data.Either (isLeft)
import qualified Data.Text as T
import           Data.Time (fromGregorian)
import           Test.Hspec

import           TDF.Invoice.AccessKey
import           TDF.Invoice.BuyerIdentity
import           TDF.Invoice.Datil
import           TDF.Server.TicketManualPayments (validateBankRefundReference)

config :: DatilConfig
config = DatilConfig
  { dcApiKey = "key", dcCertificatePassword = "secret", dcEnvironment = 1
  , dcRuc = "1793215092001", dcLegalName = "TDF RECORDS", dcTradeName = "TDF Records"
  , dcAddress = "Quito", dcEstablishmentAddress = "Quito", dcAccountingRequired = False
  , dcSpecialTaxpayer = Nothing }

input :: InvoiceInput
input = InvoiceInput
  { iiSequential = 7, iiAccessKey = "1", iiIssuedAt = "2026-10-24T12:00:00.000-05:00"
  , iiEstablishment = "001", iiEmissionPoint = "002", iiBuyer = ConsumidorFinal
  , iiBuyerEmail = Just "buyer@example.invalid"
  , iiLines = [InvoiceLine "ENTRADA" "Entrada General - PATCH CULTURE" 2 2000 0]
  , iiTotalMinor = 4000, iiPaymentMedium = "otros", iiOrderReference = "TDF-31" }

spec :: Spec
spec = describe "Electronic invoices through Datil" $ do
  it "matches the SRI technical sheet access key check digit" $
    modulo11CheckDigit "211020110117921467390011002001000000001123456781" `shouldBe` '3'

  it "builds a 49-digit key bound to date, RUC, environment, series and sequence" $ do
    let key = either (const "") id $ accessKey AccessKeyInput
          { akiIssuedOn = fromGregorian 2026 10 24, akiDocumentType = "01"
          , akiRuc = "1793215092001", akiEnvironment = 1, akiEstablishment = "001"
          , akiEmissionPoint = "002", akiSequential = 7, akiNumericCode = "12345678" }
    T.length key `shouldBe` 49
    T.take 8 key `shouldBe` "24102026"
    accessKey AccessKeyInput
      { akiIssuedOn = fromGregorian 2026 10 24, akiDocumentType = "01"
      , akiRuc = "17932150920", akiEnvironment = 1, akiEstablishment = "001"
      , akiEmissionPoint = "002", akiSequential = 7, akiNumericCode = "12345678" }
      `shouldSatisfy` isLeft

  it "derives a stable eight-digit numeric code" $ do
    numericCodeFromSeed "doc-a" `shouldBe` numericCodeFromSeed "doc-a"
    T.length (numericCodeFromSeed "doc-a") `shouldBe` 8

  it "allows final consumer only up to USD 50" $ do
    validateBillingIdentity 4000 Nothing Nothing Nothing `shouldBe` Right BillingConsumidorFinal
    validateBillingIdentity 5000 (Just "consumidor_final") Nothing Nothing `shouldBe` Right BillingConsumidorFinal
    validateBillingIdentity 6000 Nothing Nothing Nothing `shouldSatisfy` isLeft

  it "validates Ecuadorian identification numbers" $ do
    validCedula "1716535511" `shouldBe` True
    validCedula "1716535512" `shouldBe` False
    validCedula "9916535511" `shouldBe` False
    validRuc "1793215092001" `shouldBe` True
    validRuc "1716535511001" `shouldBe` True
    validRuc "1716535511000" `shouldBe` False
    validateBillingIdentity 8000 (Just "cedula") (Just "1716535511") (Just "Ana Pérez")
      `shouldBe` Right (BillingCedula "1716535511" "Ana Pérez")
    validateBillingIdentity 8000 (Just "cedula") (Just "1716535511") Nothing `shouldSatisfy` isLeft

  it "builds an IVA 0% payload whose lines equal the paid total" $ do
    case invoicePayload config input of
      Right (A.Object payload) -> do
        KM.lookup "secuencial" payload `shouldBe` Just (A.Number 7)
        KM.lookup "ambiente" payload `shouldBe` Just (A.Number 1)
      other -> expectationFailure ("Unexpected payload: " <> show other)
    invoicePayload config input { iiTotalMinor = 4001 } `shouldSatisfy` isLeft
    invoicePayload config input { iiLines = [] } `shouldSatisfy` isLeft

  it "builds a credit note bound to the modified invoice without invoice-only fields" $ do
    let modified = ModifiedInvoice "001-900-000000002" "2026-10-06T12:00:00.000-05:00" "Devolución de entradas"
    case creditNotePayload config input modified of
      Right (A.Object payload) -> do
        KM.lookup "numero_documento_modificado" payload `shouldBe` Just (A.String "001-900-000000002")
        KM.lookup "tipo_documento_modificado" payload `shouldBe` Just (A.String "01")
        KM.member "pagos" payload `shouldBe` False
        case KM.lookup "totales" payload of
          Just (A.Object totals) -> do
            KM.member "descuento" totals `shouldBe` False
            KM.member "propina" totals `shouldBe` False
          other -> expectationFailure ("Unexpected totals: " <> show other)
        case KM.lookup "emisor" payload of
          Just (A.Object issuer) -> KM.lookup "contribuyente_especial" issuer `shouldBe` Just (A.String "")
          other -> expectationFailure ("Unexpected issuer: " <> show other)
      other -> expectationFailure ("Unexpected credit note: " <> show other)

  it "accepts only stable bank refund references" $ do
    validateBankRefundReference " DEV-001 " `shouldBe` Right "BT-DEV-001"
    validateBankRefundReference "ab" `shouldSatisfy` isLeft
    validateBankRefundReference "bad ref!" `shouldSatisfy` isLeft

  it "maps settling rails to Datil payment media" $ do
    paymentMedium "bank_transfer" `shouldBe` "transferencia"
    paymentMedium "paypal" `shouldBe` "otros"

  it "never treats an unauthorized or unreadable document as authorized" $ do
    let document status extra = A.object ([ "id" A..= ("abc123" :: String), "estado" A..= (status :: String) ] <> extra)
        authorization = [ "autorizacion" A..= A.object
          [ "numero" A..= ("2410202601179321509200110010020000000071234567813" :: String)
          , "fecha" A..= ("2026-10-24T17:00:00Z" :: String) ] ]
    interpretDatilDocument (document "AUTORIZADO" authorization)
      `shouldSatisfy` either (const False) isAuthorized
    interpretDatilDocument (document "AUTORIZADO" []) `shouldBe` Right (DatilPending "abc123")
    interpretDatilDocument (document "RECIBIDO" []) `shouldBe` Right (DatilPending "abc123")
    interpretDatilDocument (document "NO AUTORIZADO" [])
      `shouldSatisfy` either (const False) isRejected
    interpretDatilDocument (A.object []) `shouldSatisfy` isLeft
  where
    isAuthorized (DatilAuthorized {}) = True
    isAuthorized _ = False
    isRejected (DatilRejected {}) = True
    isRejected _ = False
