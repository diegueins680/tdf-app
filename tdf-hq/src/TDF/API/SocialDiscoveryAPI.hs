{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.SocialDiscoveryAPI where

import           Data.Text (Text)
import           Servant

import           TDF.DTO.SocialDiscoveryDTO

type SocialDiscoveryAPI =
       "social-discovery" :>
         ( "posts"
             :> QueryParam "status" Text
             :> QueryParam "limit" Int
             :> Get '[JSON] [SocialDiscoveryPostDTO]
      :<|> "posts" :> Capture "postId" Text :> "review"
             :> ReqBody '[JSON] SocialDiscoveryReviewIn
             :> Put '[JSON] SocialDiscoveryPostDTO
         )
