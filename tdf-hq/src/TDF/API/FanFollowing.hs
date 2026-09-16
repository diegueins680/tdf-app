{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.FanFollowing (FollowArtistAPI, UnfollowArtistAPI) where
import Data.Int (Int64)
import Servant
import TDF.DTO (FanFollowDTO)
type FollowArtistAPI = Capture "artistId" Int64 :> Post '[JSON] FanFollowDTO
type UnfollowArtistAPI = Capture "artistId" Int64 :> Delete '[JSON] NoContent
