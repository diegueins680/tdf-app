{-# LANGUAGE OverloadedStrings #-}

module TDF.CoursePublicMetadataSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, listOf, elements)
import qualified TDF.Courses.PublicMetadata as Public

spec :: Spec
spec = describe "public course metadata projection" $ do
  it "admits only stored course identities reachable without normalization" $ do
    forM_ ["course", "course-2026", "a", "0", "-a-", T.replicate 96 "a"] $ \slug ->
      Public.isCanonicalCourseSlug slug `shouldBe` True
    forM_ ["", "---", "UPPERCASE", " padded ", "course/name", "ma\241ana",
           T.replicate 97 "a"] $ \slug ->
      Public.isCanonicalCourseSlug slug `shouldBe` False

  it "preserves accepted public URLs and trims the stored projection" $ do
    forM_ ["landingUrl", "locationMapUrl", "instructorAvatarUrl"] $ \field -> do
      Public.validateCoursePublicUrlField field (Just " https://tdf.example.com/public ")
        `shouldBe` Right (Just "https://tdf.example.com/public")
      Public.sanitizeStoredCoursePublicUrl field (Just "  ") `shouldBe` Nothing
      Public.sanitizeStoredCoursePublicUrl field Nothing `shouldBe` Nothing

  it "suppresses normalized production collisions without changing ordinary slug authority" $ do
    Public.ambiguousProductionCourseSlugs
      [ "produccion-musical-current", " Produccion-Musical-Current "
      , "produccion-musical-other", "ordinary", " ORDINARY "
      ] `shouldBe` ["produccion-musical-current"]
    Public.ambiguousProductionCourseSlugs ["produccion-musical-current"] `shouldBe` []

  prop "case and Unicode whitespace collisions cannot acquire a separate production identity" $
    forAll (listOf (elements (['a'..'z'] <> ['0'..'9'] <> "-"))) $ \suffix ->
      let slug = "produccion-musical-a" <> T.take 70 (T.pack suffix)
          legacy = "\t\x2003" <> T.toUpper slug <> "\x2003\n"
      in Public.ambiguousProductionCourseSlugs [slug, legacy] == [slug]

  it "shares the exact earliest-session lead boundary with public production resolution" $ do
    let today = fromGregorian 2026 10 22
    map (Public.hasRequiredProductionLead today)
      [Nothing, Just today, Just (addDays 27 today), Just (addDays 28 today),
       Just (addDays 29 today)] `shouldBe` [False, False, False, True, True]

  it "admits self-resolving cohorts and excludes stored production aliases" $ do
    let today = fromGregorian 2026 10 22
        future = Just (addDays 28 today)
        admitted = Public.isCurrentPublicCourseIdentity "custom-alias" today
    admitted "regular-course" Nothing `shouldBe` True
    admitted "produccion-musical-current" future `shouldBe` True
    admitted "produccion-musical-current" Nothing `shouldBe` False
    admitted "produccion-musical-current" (Just (addDays 27 today)) `shouldBe` False
    admitted "produccion-musical" future `shouldBe` False
    admitted "custom-alias" future `shouldBe` False
    admitted "PRODUCCION-MUSICAL-CURRENT" future `shouldBe` False
    Public.isCurrentPublicCourseIdentity "produccion-musical-current" today
      "produccion-musical-current" future `shouldBe` True

  it "rejects unsafe scheme, authority, path and control variants in every public URL field" $ do
    forM_ ["landingUrl", "locationMapUrl", "instructorAvatarUrl", "whatsappCtaUrl"] $ \field ->
      forM_ ["http://example.com/private", "javascript:private", "https://localhost/private",
             "https://127.0.0.1/private", "https://user:private@example.com/private",
             "https://example.com/#fragment", "https://example.com/a//b",
             "https://example.com/%2e%2e/private", "https://example.com/%2fprivate",
             "https://example.com/%0aprivate", "https://example.com/%7fprivate",
             "https://example.com/%xx", "https://example.com/" <> T.replicate 2049 "a"] $ \url -> do
        Public.validateCoursePublicUrlField field (Just url) `shouldSatisfy` isLeft
        Public.sanitizeStoredCoursePublicUrl field (Just url) `shouldBe` Nothing

  it "retains the field-specific WhatsApp host and send-path policy" $ do
    forM_ ["https://wa.me/593991234567", "https://wa.me/?text=Curso",
           "https://api.whatsapp.com/send?phone=593991234567",
           "https://web.whatsapp.com/send?text=Curso"] $ \url ->
      Public.validateCoursePublicUrlField "whatsappCtaUrl" (Just url) `shouldBe` Right (Just url)
    forM_ ["https://example.com/contact", "https://wa.me.evil.example/593991234567",
           "https://wa.me/0991234567", "https://wa.me/593991234567?redirect=private"] $ \url ->
      Public.sanitizeStoredCoursePublicUrl "whatsappCtaUrl" (Just url) `shouldBe` Nothing

  prop "stored URL sanitization is idempotent" $ \raw ->
    let sanitize = Public.sanitizeStoredCoursePublicUrl "landingUrl"
        once = sanitize (Just (T.pack raw))
    in sanitize once == once

  prop "padding a generated canonical identity cannot make the stored identity public" $
    forAll (listOf (elements (['a'..'z'] <> ['0'..'9'] <> "-"))) $ \tailChars ->
      let slug = "a" <> T.take 95 (T.pack tailChars)
      in Public.isCanonicalCourseSlug slug
           && not (Public.isCanonicalCourseSlug (" " <> slug <> " "))
