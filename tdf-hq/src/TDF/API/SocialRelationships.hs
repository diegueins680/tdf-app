{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.SocialRelationships
  (FollowersAPI, FollowingAPI, FriendsAPI, SuggestionsAPI, RelationshipReadsAPI) where

import Servant
import TDF.DTO (PartyFollowDTO, SuggestedFriendDTO)

type FollowersAPI = "followers" :> Get '[JSON] [PartyFollowDTO]
type FollowingAPI = "following" :> Get '[JSON] [PartyFollowDTO]
type FriendsAPI = "friends" :> Get '[JSON] [PartyFollowDTO]
type SuggestionsAPI = "suggestions" :> Get '[JSON] [SuggestedFriendDTO]
type RelationshipReadsAPI = FollowersAPI :<|> FollowingAPI :<|> FriendsAPI :<|> SuggestionsAPI
