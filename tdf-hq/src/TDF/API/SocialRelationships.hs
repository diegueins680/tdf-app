{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.SocialRelationships
  (FollowersAPI, FollowingAPI, FriendsAPI, SuggestionsAPI, RelationshipReadsAPI, AddFriendAPI, RemoveFriendAPI, VCardAPI, RelationshipWritesAPI) where

import Data.Int (Int64)
import Servant
import TDF.DTO (PartyFollowDTO, SuggestedFriendDTO, VCardExchangeRequest)

type FollowersAPI = "followers" :> Get '[JSON] [PartyFollowDTO]
type FollowingAPI = "following" :> Get '[JSON] [PartyFollowDTO]
type FriendsAPI = "friends" :> Get '[JSON] [PartyFollowDTO]
type SuggestionsAPI = "suggestions" :> Get '[JSON] [SuggestedFriendDTO]
type RelationshipReadsAPI = FollowersAPI :<|> FollowingAPI :<|> FriendsAPI :<|> SuggestionsAPI

type AddFriendAPI = "friends" :> Capture "partyId" Int64 :> Post '[JSON] [PartyFollowDTO]
type RemoveFriendAPI = "friends" :> Capture "partyId" Int64 :> Delete '[JSON] NoContent
type VCardAPI = "vcard-exchange" :> ReqBody '[JSON] VCardExchangeRequest :> Post '[JSON] [PartyFollowDTO]
type RelationshipWritesAPI = AddFriendAPI :<|> RemoveFriendAPI :<|> VCardAPI
