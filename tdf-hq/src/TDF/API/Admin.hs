{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Admin where

import           Data.Text     (Text)
import           Servant

import           TDF.API.Types ( DropdownOptionCreate
                                , DropdownOptionDTO
                                , DropdownOptionUpdate
                                , RoleDetailDTO
                                , UserAccountCreate
                                , UserAccountDTO
                                , UserAccountUpdate
                                )

import           Data.Int      (Int64)

type DropdownCategoryAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [DropdownOptionDTO]
  :<|> ReqBody '[JSON] DropdownOptionCreate :> PostCreated '[JSON] DropdownOptionDTO
  :<|> Capture "optionId" Text :> ReqBody '[JSON] DropdownOptionUpdate :> Patch '[JSON] DropdownOptionDTO

type UsersAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [UserAccountDTO]
  :<|> ReqBody '[JSON] UserAccountCreate :> PostCreated '[JSON] UserAccountDTO
  :<|> Capture "userId" Int64 :>
        ( Get '[JSON] UserAccountDTO
     :<|> ReqBody '[JSON] UserAccountUpdate :> Patch '[JSON] UserAccountDTO
        )

type RolesAPI = Get '[JSON] [RoleDetailDTO]

type AdminAPI =
       "seed" :> Post '[JSON] NoContent
  :<|> "dropdowns" :> Capture "category" Text :> DropdownCategoryAPI
  :<|> "users" :> UsersAPI
  :<|> "roles" :> RolesAPI
