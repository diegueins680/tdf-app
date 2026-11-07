{-# LANGUAGE OverloadedStrings #-}

-- | The existing public URL parser, shared by public metadata and retrieval.
module TDF.PublicUrl (isValidHttpUrl, hasAmbiguousPublicUrlPath) where

import Data.Char (GeneralCategory(Format, LineSeparator, ParagraphSeparator),
                  generalCategory, isAsciiLower, isControl, isDigit, isHexDigit, isSpace)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

hasAmbiguousPublicUrlPath :: Text -> Bool
hasAmbiguousPublicUrlPath rawUrl =
  any isAmbiguousPathSegment pathSegments
  where
    lowerUrl = T.toLower rawUrl
    noScheme
      | "https://" `T.isPrefixOf` lowerUrl = T.drop 8 rawUrl
      | "http://" `T.isPrefixOf` lowerUrl = T.drop 7 rawUrl
      | otherwise = rawUrl
    pathWithQueryOrFragment =
      T.dropWhile (\ch -> ch /= '/' && ch /= '?' && ch /= '#') noScheme
    path =
      case T.uncons pathWithQueryOrFragment of
        Just ('/', _) ->
          T.takeWhile (\ch -> ch /= '?' && ch /= '#') pathWithQueryOrFragment
        _ ->
          ""
    trimmedPath = T.dropWhileEnd (== '/') path
    pathSegments =
      if T.null trimmedPath
        then []
        else T.splitOn "/" (T.drop 1 trimmedPath)
    isAmbiguousPathSegment segment =
      segment == ""
        || decodedSegment == "."
        || decodedSegment == ".."
        || hasEncodedPathSeparator segment
      where
        decodedSegment = decodeUrlEncodedDots (T.toLower segment)

    hasEncodedPathSeparator segment =
      "%2f" `T.isInfixOf` loweredSegment
        || "%5c" `T.isInfixOf` loweredSegment
      where
        loweredSegment = T.toLower segment

    decodeUrlEncodedDots txt =
      case T.uncons txt of
        Nothing -> ""
        Just ('%', rest)
          | "2e" `T.isPrefixOf` rest ->
              "." <> decodeUrlEncodedDots (T.drop 2 rest)
          | otherwise ->
              "%" <> decodeUrlEncodedDots rest
        Just (ch, rest) ->
          T.singleton ch <> decodeUrlEncodedDots rest

isValidHttpUrl :: Text -> Bool
isValidHttpUrl rawUrl
  | T.any invalidUrlChar trimmed = False
  | hasMalformedPercentEncoding trimmed = False
  | "http://" `T.isPrefixOf` lowerUrl = hasValidAuthority (T.drop 7 trimmed)
  | "https://" `T.isPrefixOf` lowerUrl = hasValidAuthority (T.drop 8 trimmed)
  | otherwise = False
  where
    trimmed = T.strip rawUrl
    lowerUrl = T.toLower trimmed

    invalidUrlChar ch =
      isSpace ch
        || isControl ch
        || ch == '\\'
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

    hasMalformedPercentEncoding url =
      case T.breakOn "%" url of
        (_, rest)
          | T.null rest -> False
          | otherwise ->
              case T.unpack (T.take 3 rest) of
                ['%', firstHex, secondHex]
                  | isHexDigit firstHex && isHexDigit secondHex ->
                      hasMalformedPercentEncoding (T.drop 3 rest)
                _ -> True

    hasValidAuthority remainder =
      let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') remainder
      in validateAuthority authority

    validateAuthority rawAuthority
      | T.null rawAuthority = False
      | T.any (== '@') rawAuthority = False
      -- Public lead drive links should be DNS-backed URLs, not IP literals.
      | "[" `T.isPrefixOf` rawAuthority =
          False
      | T.count ":" rawAuthority > 1 = False
      | otherwise =
          let (host, portSuffix) = T.breakOn ":" rawAuthority
          in validateHost host && validatePortSuffix portSuffix

    validateHost host =
      let normalizedHost = T.toLower host
      in not (T.null normalizedHost)
        && not (T.isPrefixOf "." normalizedHost)
        && not (T.isSuffixOf "." normalizedHost)
        && not (isAmbiguousNumericHost normalizedHost)
        && hasValidEmailFinalDomainLabel normalizedHost
        && all isValidEmailDomainLabel (T.splitOn "." normalizedHost)
        && not (looksLikeNonDecimalIpv4 normalizedHost)
        && not (looksLikeInvalidIpv4 normalizedHost)
        && not (requiresExplicitPublicHostname normalizedHost)
        && not (isPrivateHost normalizedHost)

    validatePortSuffix suffix
      | T.null suffix = True
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in not (T.null port)
               && T.all isDigit port
               && not (T.length port > 1 && T.head port == '0')
               && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
                    (readMaybe (T.unpack port))
      | otherwise = False

    isPrivateHost host =
      host == "localhost"
        || ".localhost" `T.isSuffixOf` host
        || maybe False isNonPublicIpv4 (parseIpv4Octets host)
        || maybe False isNonPublicIpv4 (trailingIpv4Octets host)
        || isPrivateIpv6 host

    parseIpv4Octets host =
      case T.splitOn "." host of
        [a, b, c, d] -> do
          oa <- parseOctet a
          ob <- parseOctet b
          oc <- parseOctet c
          od <- parseOctet d
          pure (oa, ob, oc, od)
        _ -> Nothing

    -- Reject malformed dotted-quad hosts that would otherwise slip through as DNS labels.
    looksLikeInvalidIpv4 host =
      case T.splitOn "." host of
        [a, b, c, d]
          | all isNumericLabel [a, b, c, d] -> isNothing (parseIpv4Octets host)
        _ -> False

    looksLikeNonDecimalIpv4 host =
      case T.splitOn "." host of
        [a, b, c, d] ->
          let labels = [a, b, c, d]
          in any isHexIpv4Label labels && all isIpv4LiteralLikeLabel labels
        _ -> False

    isIpv4LiteralLikeLabel label =
      isNumericLabel label || isHexIpv4Label label

    isHexIpv4Label label =
      let lowerLabel = T.toLower label
          digitsOnly = T.drop 2 lowerLabel
      in "0x" `T.isPrefixOf` lowerLabel
           && not (T.null digitsOnly)
           && T.all isHexDigit digitsOnly

    isAmbiguousNumericHost host =
      T.all (\c -> isDigit c || c == '.') host
        && isNothing (parseIpv4Octets host)

    requiresExplicitPublicHostname host =
      not (T.any (== '.') host)
        && not (T.any (== ':') host)
        && isNothing (parseIpv4Octets host)

    isNumericLabel label =
      not (T.null label) && T.all isDigit label

    parseOctet octet
      | T.null octet || T.any (not . isDigit) octet = Nothing
      | T.length octet > 1 && T.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack octet)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

    trailingIpv4Octets host =
      let suffix = T.takeWhileEnd (/= ':') host
      in if T.any (== '.') suffix
           then parseIpv4Octets suffix
           else Nothing

    isNonPublicIpv4 (a, b, c, _) =
      a == (0 :: Int)
        || a == 10
        || a == 127
        || (a == 100 && b >= 64 && b <= 127)
        || (a == 169 && b == 254)
        || (a == 172 && b >= 16 && b <= 31)
        || (a == 192 && b == 0 && c == 0)
        || (a == 192 && b == 0 && c == 2)
        || (a == 192 && b == 168)
        || (a == 198 && (b == 18 || b == 19))
        || (a == 198 && b == 51 && c == 100)
        || (a == 203 && b == 0 && c == 113)
        || (a >= 224 && a <= 255)

    isPrivateIpv6 host =
      host == "::"
        || host == "::1"
        || isUniqueLocal firstSegment
        || isLinkLocal firstSegment
      where
        firstSegment = T.takeWhile (/= ':') host

        isUniqueLocal segment =
          "fc" `T.isPrefixOf` segment || "fd" `T.isPrefixOf` segment

        isLinkLocal segment =
          any (`T.isPrefixOf` segment) ["fe8", "fe9", "fea", "feb"]

hasValidEmailFinalDomainLabel :: Text -> Bool
hasValidEmailFinalDomainLabel domain =
  case reverse (T.splitOn "." domain) of
    finalLabel : _ ->
      T.length finalLabel >= 2 && T.any isAsciiLower finalLabel
    [] -> False

isValidEmailDomainLabel :: Text -> Bool
isValidEmailDomainLabel label =
  not (T.null label)
    && T.length label <= maxPublicEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidEmailDomainChar label

maxPublicEmailDomainLabelChars :: Int
maxPublicEmailDomainLabelChars = 63

isValidEmailDomainChar :: Char -> Bool
isValidEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'
