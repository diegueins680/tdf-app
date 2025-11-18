module TDF.Services
  ( Services(..)
  , buildServices
  ) where

import TDF.Config (AppConfig)
import qualified TDF.Email.Service as EmailSvc
import TDF.Email.Service (EmailService)

data Services = Services
  { emailService :: EmailService
  }

buildServices :: AppConfig -> Services
buildServices cfg = Services
  { emailService = EmailSvc.mkEmailService cfg
  }
