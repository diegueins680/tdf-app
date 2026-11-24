{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared filter options (TAX-FILTER-001) and artist genre suggestions with
-- their review queue (TAX-GENRE-SUGGESTION-001).
module TDF.Server.Taxonomy
  ( taxonomyPublicServer
  , taxonomyProtectedServer
  , genreSuggestionSlug
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value(..), encode, object, (.=))
import Data.Char (isAlphaNum, toLower)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Servant

import TDF.API.Taxonomy
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(Admin, Artist, Artista))
import TDF.Taxonomy.GenreSuggestion
import TDF.UUIDInstances ()

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

jsonRowsDB :: Text -> [PersistValue] -> SqlPersistT IO [Value]
jsonRowsDB statement params = do
  rows <- rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue]
  pure [CMS.unAesonValue value | Single value <- rows]

jsonError :: ServerError -> Value -> ServerError
jsonError base body = base
  { errBody = encode body
  , errHeaders = ("Content-Type", "application/json;charset=utf-8") : errHeaders base
  }

plainError :: ServerError -> Text -> ServerError
plainError base message = jsonError base (object ["error" .= message])

taxonomyPublicServer :: ServerT TaxonomyPublicAPI AppM
taxonomyPublicServer = filterOptions

taxonomyProtectedServer :: AuthedUser -> ServerT TaxonomyProtectedAPI AppM
taxonomyProtectedServer user =
       checkSuggestion user
  :<|> submitSuggestion user
  :<|> listMySuggestions user
  :<|> listSuggestionQueue user
  :<|> approveSuggestion user
  :<|> rejectSuggestion user
  :<|> mergeSuggestion user

-- ---------------------------------------------------------------------------
-- Filter options
-- ---------------------------------------------------------------------------

-- | Countries, regions, cities and published genres with both names. Only
-- active references are listed; the client picks the name for its language.
filterOptions :: AppM Value
filterOptions = do
  rows <- runDB $ jsonRowsDB
    ( "SELECT jsonb_build_object("
   <> "'countries',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',item.id,'code',item.alpha2,'nameEs',item.name_es,'nameEn',item.name_en) ORDER BY item.sort_order,item.name_es),'[]'::jsonb) FROM country_reference item WHERE item.active AND item.deprecated_at IS NULL),"
   <> "'regions',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',item.id,'countryId',item.country_id,'code',item.code,'type',item.subdivision_type,'nameEs',item.name_es,'nameEn',item.name_en) ORDER BY item.sort_order,item.name_es),'[]'::jsonb) FROM subdivision_reference item WHERE item.active AND item.deprecated_at IS NULL),"
   <> "'cities',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',item.id,'countryId',item.country_id,'regionId',item.subdivision_id,'code',item.code,'nameEs',item.name_es,'nameEn',item.name_en,'latitude',item.latitude,'longitude',item.longitude) ORDER BY item.sort_order,item.name_es),'[]'::jsonb) FROM city_reference item WHERE item.active AND item.deprecated_at IS NULL),"
   <> "'genres',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',item.id,'parentId',item.parent_id,'code',item.code,'slug',item.current_slug,'nameEs',item.name_es,'nameEn',item.name_en) ORDER BY item.sort_order,item.name_es),'[]'::jsonb) FROM genre item JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' WHERE item.active AND item.deprecated_at IS NULL))"
    )
    []
  pure (fromMaybe (object []) (listToMaybe rows))

-- ---------------------------------------------------------------------------
-- Suggestions
-- ---------------------------------------------------------------------------

requireArtist :: AuthedUser -> AppM Int64
requireArtist user = do
  let partyId = fromSqlKey (auPartyId user)
      roles = auRoles user
  unless (any (`elem` roles) [Artist, Artista, Admin]) $
    throwError (plainError err403 "Only artists can suggest genres")
  when (partyId <= 0 || length roles /= length (nub roles)) $
    throwError (plainError err403 "Artist access requires coherent role grants")
  pure partyId

requireStrictAdmin :: AuthedUser -> AppM Int64
requireStrictAdmin user = do
  unless (hasStrictAdminAccess user) $
    throwError (plainError err403 "Administrator access required")
  pure (fromSqlKey (auPartyId user))

loadExistingGenres :: SqlPersistT IO [ExistingGenre]
loadExistingGenres = do
  rows <- rawSql
    "SELECT item.id,item.name_es,item.name_en,item.parent_id FROM genre item WHERE item.active ORDER BY item.sort_order,item.id"
    [] :: SqlPersistT IO [(Single UUID, Single Text, Single Text, Single (Maybe UUID))]
  pure [ExistingGenre genreId nameEs nameEn parentId | (Single genreId, Single nameEs, Single nameEn, Single parentId) <- rows]

validateParent :: Maybe UUID -> SqlPersistT IO (Either Text ())
validateParent Nothing = pure (Right ())
validateParent (Just parentId) = do
  rows <- rawSql
    "SELECT count(*) FROM genre item JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' WHERE item.id=?::uuid AND item.active"
    [toPersistValue parentId] :: SqlPersistT IO [Single Int64]
  pure $ case rows of
    [Single 1] -> Right ()
    _ -> Left "parentGenreId must reference a published genre"

genreJson :: ExistingGenre -> Value
genreJson genre = object
  [ "id" .= egId genre
  , "nameEs" .= egNameEs genre
  , "nameEn" .= egNameEn genre
  , "parentId" .= egParentId genre
  ]

verdictCode :: SuggestionVerdict -> Text
verdictCode verdict = case verdict of
  Duplicate _ -> "duplicate"
  ProbableTypo _ -> "similar"
  NeedsReview _ -> "review"
  AutoApprove -> "approve"

checkJson :: SuggestionCheck -> Value
checkJson SuggestionCheck{..} = object
  [ "verdict" .= verdictCode scVerdict
  , "nameEs" .= ncName scSpanish
  , "nameEn" .= ncName scEnglish
  , "normalizedKey" .= ncKey scSpanish
  , "match" .= case scVerdict of
      Duplicate genre -> genreJson genre
      ProbableTypo genre -> genreJson genre
      _ -> Null
  , "corrections" .=
      [ object ["word" .= wcWord correction, "suggestion" .= wcSuggestion correction]
      | correction <- nubCorrections (ncCorrections scSpanish <> ncCorrections scEnglish)
      ]
  , "reviewReasons" .= reviewReasonCodes scVerdict
  ]
  where
    nubCorrections = foldr (\c acc -> if any ((== wcWord c) . wcWord) acc then acc else c : acc) []

type GenreTransaction = ExceptT ServerError (SqlPersistT IO)

checkNames :: Maybe UUID -> Bool -> Text -> Text -> GenreTransaction SuggestionCheck
checkNames parent distinct spanish english = do
  existing <- lift loadExistingGenres
  parentResult <- lift (validateParent parent)
  either (throwError . plainError err400) pure parentResult
  either (throwError . plainError err400) pure (checkGenreSuggestion existing distinct spanish english)

-- Both languages participate in duplicate detection and spelling corrections
-- depend on the live catalog. Serialize the short publication transactions on
-- the genres catalog row, then read the catalog again under READ COMMITTED.
withGenrePublication :: GenreTransaction a -> AppM a
withGenrePublication action = do
  outcome <- runDB $ runExceptT $ do
    locks <- lift (rawSql "SELECT 1::bigint FROM catalog_definition WHERE code='genres' FOR NO KEY UPDATE" []
                   :: SqlPersistT IO [Single Int64])
    when (null locks) (throwError (plainError err500 "Genre catalog is not configured"))
    action
  either throwError pure outcome

checkSuggestion :: AuthedUser -> GenreSuggestionRequest -> AppM Value
checkSuggestion user GenreSuggestionRequest{..} = do
  _ <- requireArtist user
  result <- runDB (runExceptT (checkNames parentGenreId (fromMaybe False confirmDistinct) nameEs nameEn))
  checkJson <$> either throwError pure result

-- | Shared projection of a suggestion row aliased @suggestion@.
suggestionProjection :: Text
suggestionProjection =
  "jsonb_build_object('id',suggestion.id,'status',suggestion.status,"
    <> "'submittedName',suggestion.submitted_name,'nameEs',suggestion.name_es,'nameEn',suggestion.name_en,"
    <> "'parentGenreId',suggestion.parent_genre_id,"
    <> "'parentGenre',(SELECT jsonb_build_object('id',parent.id,'nameEs',parent.name_es,'nameEn',parent.name_en) FROM genre parent WHERE parent.id=suggestion.parent_genre_id),"
    <> "'reviewReasons',to_jsonb(suggestion.review_reasons),"
    <> "'resolvedGenre',(SELECT jsonb_build_object('id',resolved.id,'nameEs',resolved.name_es,'nameEn',resolved.name_en) FROM genre resolved WHERE resolved.id=suggestion.resolved_genre_id),"
    <> "'suggestedBy',jsonb_build_object('partyId',suggestion.suggested_by_party_id,'displayName',(SELECT party.display_name FROM party WHERE party.id=suggestion.suggested_by_party_id)),"
    <> "'reviewNote',suggestion.review_note,'reviewedAt',suggestion.reviewed_at,"
    <> "'automatic',suggestion.status='approved' AND suggestion.reviewed_by_party_id IS NULL,"
    <> "'createdAt',suggestion.created_at)"

loadSuggestion :: UUID -> SqlPersistT IO (Maybe Value)
loadSuggestion suggestionId =
  listToMaybe <$> jsonRowsDB
    ("SELECT " <> suggestionProjection <> " FROM genre_suggestion suggestion WHERE suggestion.id=?::uuid")
    [toPersistValue suggestionId]

-- | Lowercase ASCII slug for a new genre code ("Cumbia Villera" -> cumbia-villera).
genreSuggestionSlug :: Text -> Text
genreSuggestionSlug name =
  let folded = T.map toLower (foldAccents name)
      dashed = T.map (\c -> if isAlphaNum c && c < '\128' then c else '-') folded
      parts = filter (not . T.null) (T.splitOn "-" dashed)
  in if null parts then "genre" else T.take 60 (T.intercalate "-" parts)

-- | Insert a published genre and return its id. The code is the English slug,
-- suffixed with the suggestion id when it is already taken.
createGenre :: UUID -> Text -> Text -> Maybe UUID -> Int64 -> Maybe Int64 -> SqlPersistT IO UUID
createGenre suggestionId nameEsValue nameEnValue parentId authorPartyId approverPartyId = do
  let baseCode = genreSuggestionSlug nameEnValue
  rows <- rawSql
    ( "WITH chosen AS (SELECT CASE WHEN EXISTS (SELECT 1 FROM genre taken WHERE taken.code=?::text OR taken.current_slug=?::text) "
   <> "THEN ?::text||'-'||left(?::text,8) ELSE ?::text END code) "
   <> "INSERT INTO genre (catalog_id,code,parent_id,name_es,name_en,current_slug,sort_order,active,workflow_state_id,published_revision,version,created_by,approved_by,source_version) "
   <> "SELECT catalog.id,chosen.code,?::uuid,?,?,chosen.code,1000,TRUE,state.id,1,1,?,?,'artist genre suggestion' "
   <> "FROM catalog_definition catalog JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published' CROSS JOIN chosen "
   <> "WHERE catalog.code='genres' RETURNING id"
    )
    [ PersistText baseCode, PersistText baseCode, PersistText baseCode, toPersistValue suggestionId, PersistText baseCode
    , maybe PersistNull toPersistValue parentId, PersistText nameEsValue, PersistText nameEnValue
    , PersistInt64 authorPartyId, maybe PersistNull PersistInt64 approverPartyId
    ] :: SqlPersistT IO [Single UUID]
  case rows of
    [Single genreId] -> pure genreId
    _ -> liftIO (ioError (userError "genre catalog is not configured"))

-- | Add the genre to the suggesting artist's own profile, after their current ones.
addToArtistProfile :: Int64 -> UUID -> SqlPersistT IO ()
addToArtistProfile partyId genreId = do
  _ <- rawSql
    ( "INSERT INTO artist_profile_genre_membership (artist_party_id,genre_id,sort_order,created_at) "
   <> "SELECT profile.artist_party_id,?::uuid,coalesce((SELECT max(member.sort_order)+1 FROM artist_profile_genre_membership member WHERE member.artist_party_id=profile.artist_party_id),0),now() "
   <> "FROM artist_profile profile WHERE profile.artist_party_id=? "
   <> "ON CONFLICT (artist_party_id,genre_id) DO NOTHING RETURNING 1"
    )
    [toPersistValue genreId, PersistInt64 partyId] :: SqlPersistT IO [Single Int64]
  pure ()

suggestionLimits :: Int64 -> SqlPersistT IO (Int64, Int64)
suggestionLimits partyId = do
  rows <- rawSql
    "SELECT count(*) FILTER (WHERE status='pending'),count(*) FILTER (WHERE created_at>now()-interval '1 day') FROM genre_suggestion WHERE suggested_by_party_id=?"
    [PersistInt64 partyId] :: SqlPersistT IO [(Single Int64, Single Int64)]
  pure $ case rows of
    [(Single pending, Single today)] -> (pending, today)
    _ -> (0, 0)

submitSuggestion :: AuthedUser -> GenreSuggestionRequest -> AppM Value
submitSuggestion user GenreSuggestionRequest{..} = do
  partyId <- requireArtist user
  withGenrePublication $ do
    check@SuggestionCheck{..} <- checkNames parentGenreId (fromMaybe False confirmDistinct) nameEs nameEn
    case scVerdict of
      Duplicate _ -> throwError (jsonError err409 (checkJson check))
      ProbableTypo _ -> throwError (jsonError err409 (checkJson check))
      _ -> pure ()
    (pending, today) <- lift (suggestionLimits partyId)
    when (pending >= 10 || today >= 20) $
      throwError (plainError err429 "Too many genre suggestions; wait for the pending ones to be reviewed")
    let key = ncKey scSpanish
        submitted = T.unwords (T.words nameEs)
        reasons = reviewReasonCodes scVerdict
    result <- lift $ do
      -- An identical suggestion already waiting for review is shared, not duplicated.
      existingPending <- rawSql
        "SELECT id FROM genre_suggestion WHERE normalized_key=? AND status='pending'"
        [PersistText key] :: SqlPersistT IO [Single UUID]
      case existingPending of
        Single suggestionId : _ -> (,) True <$> loadSuggestion suggestionId
        [] -> do
          inserted <- rawSql
            ( "INSERT INTO genre_suggestion (suggested_by_party_id,parent_genre_id,submitted_name,name_es,name_en,normalized_key,review_reasons) "
           <> "VALUES (?,?::uuid,?,?,?,?,?::text[]) ON CONFLICT (normalized_key) WHERE status='pending' DO NOTHING RETURNING id"
            )
            [ PersistInt64 partyId, maybe PersistNull toPersistValue parentGenreId, PersistText submitted
            , PersistText (ncName scSpanish), PersistText (ncName scEnglish), PersistText key
            , PersistText ("{" <> T.intercalate "," (map quoteArrayItem reasons) <> "}")
            ] :: SqlPersistT IO [Single UUID]
          case inserted of
            [] -> pure (True, Nothing)
            Single suggestionId : _ -> do
              when (scVerdict == AutoApprove) $ do
                genreId <- createGenre suggestionId (ncName scSpanish) (ncName scEnglish) parentGenreId partyId Nothing
                _ <- rawSql
                  "UPDATE genre_suggestion SET status='approved',resolved_genre_id=?::uuid,reviewed_at=now() WHERE id=?::uuid RETURNING 1"
                  [toPersistValue genreId, toPersistValue suggestionId] :: SqlPersistT IO [Single Int64]
                addToArtistProfile partyId genreId
              (,) False <$> loadSuggestion suggestionId
    case result of
      (alreadySuggested, Just suggestion) -> pure (object ["suggestion" .= suggestion, "alreadySuggested" .= alreadySuggested, "check" .= checkJson check])
      (_, Nothing) -> throwError (plainError err409 "This genre was suggested at the same time by someone else; try again")
  where
    quoteArrayItem item = "\"" <> T.replace "\"" "" (T.replace "\\" "" item) <> "\""

listMySuggestions :: AuthedUser -> AppM [Value]
listMySuggestions user = do
  partyId <- requireArtist user
  runDB $ jsonRowsDB
    ("SELECT " <> suggestionProjection <> " FROM genre_suggestion suggestion WHERE suggestion.suggested_by_party_id=? ORDER BY suggestion.created_at DESC,suggestion.id LIMIT 50")
    [PersistInt64 partyId]

listSuggestionQueue :: AuthedUser -> Maybe Text -> AppM [Value]
listSuggestionQueue user mStatus = do
  _ <- requireStrictAdmin user
  let status = fromMaybe "pending" (T.strip <$> mStatus)
  unless (status `elem` ["pending", "approved", "rejected", "merged", "all"]) $
    throwError (plainError err400 "status must be pending, approved, rejected, merged or all")
  runDB $ jsonRowsDB
    ( "SELECT " <> suggestionProjection <> " FROM genre_suggestion suggestion "
   <> "WHERE (?::text='all' OR suggestion.status=?::text) ORDER BY suggestion.created_at"
   <> (if status == "pending" then "" else " DESC") <> ",suggestion.id LIMIT 200"
    )
    [PersistText status, PersistText status]

-- | Lock a pending suggestion; returns its author.
lockPending :: UUID -> SqlPersistT IO (Either ServerError Int64)
lockPending suggestionId = do
  rows <- rawSql
    "SELECT suggested_by_party_id,status FROM genre_suggestion WHERE id=?::uuid FOR UPDATE"
    [toPersistValue suggestionId] :: SqlPersistT IO [(Single Int64, Single Text)]
  pure $ case rows of
    [] -> Left (plainError err404 "Unknown genre suggestion")
    (Single author, Single "pending") : _ -> Right author
    (_, Single status) : _ -> Left (plainError err409 ("This suggestion is already " <> status))

decided :: UUID -> AppM Value
decided suggestionId = runDB (loadSuggestion suggestionId) >>= maybe (throwError err404) pure

approveSuggestion :: AuthedUser -> UUID -> GenreSuggestionApproval -> AppM Value
approveSuggestion user suggestionId GenreSuggestionApproval{..} = do
  adminPartyId <- requireStrictAdmin user
  withGenrePublication $ do
    author <- ExceptT (lockPending suggestionId)
    check <- checkNames approvedParentGenreId True approvedNameEs approvedNameEn
    case scVerdict check of
      Duplicate _ -> throwError (jsonError err409 (checkJson check))
      _ -> pure ()
    lift $ do
      genreId <- createGenre suggestionId (ncName (scSpanish check)) (ncName (scEnglish check)) approvedParentGenreId author (Just adminPartyId)
      _ <- rawSql
        ( "UPDATE genre_suggestion SET status='approved',name_es=?,name_en=?,parent_genre_id=?::uuid,resolved_genre_id=?::uuid,"
       <> "reviewed_by_party_id=?,reviewed_at=now() WHERE id=?::uuid RETURNING 1"
        )
        [ PersistText (ncName (scSpanish check)), PersistText (ncName (scEnglish check))
        , maybe PersistNull toPersistValue approvedParentGenreId, toPersistValue genreId
        , PersistInt64 adminPartyId, toPersistValue suggestionId
        ] :: SqlPersistT IO [Single Int64]
      addToArtistProfile author genreId
  decided suggestionId

rejectSuggestion :: AuthedUser -> UUID -> GenreSuggestionRejection -> AppM Value
rejectSuggestion user suggestionId GenreSuggestionRejection{..} = do
  adminPartyId <- requireStrictAdmin user
  let note = T.strip rejectionNote
  when (T.null note || T.length note > 500) $
    throwError (plainError err400 "A rejection note of 1 to 500 characters is required")
  outcome <- runDB $ do
    locked <- lockPending suggestionId
    forM_ locked $ \_ -> rawSql
      "UPDATE genre_suggestion SET status='rejected',review_note=?,reviewed_by_party_id=?,reviewed_at=now() WHERE id=?::uuid RETURNING 1"
      [PersistText note, PersistInt64 adminPartyId, toPersistValue suggestionId] :: SqlPersistT IO [Single Int64]
    pure locked
  either throwError (const (pure ())) outcome
  decided suggestionId

mergeSuggestion :: AuthedUser -> UUID -> GenreSuggestionMerge -> AppM Value
mergeSuggestion user suggestionId GenreSuggestionMerge{..} = do
  adminPartyId <- requireStrictAdmin user
  outcome <- runDB $ do
    target <- validateParent (Just mergeIntoGenreId)
    case target of
      Left _ -> pure (Left (plainError err400 "mergeIntoGenreId must reference a published genre"))
      Right () -> do
        locked <- lockPending suggestionId
        case locked of
          Left failure -> pure (Left failure)
          Right author -> do
            _ <- rawSql
              "UPDATE genre_suggestion SET status='merged',resolved_genre_id=?::uuid,reviewed_by_party_id=?,reviewed_at=now() WHERE id=?::uuid RETURNING 1"
              [toPersistValue mergeIntoGenreId, PersistInt64 adminPartyId, toPersistValue suggestionId] :: SqlPersistT IO [Single Int64]
            addToArtistProfile author mergeIntoGenreId
            pure (Right ())
  either throwError pure outcome
  decided suggestionId
