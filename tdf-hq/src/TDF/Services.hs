module TDF.Services
  ( Services(..)
  , buildServices
  , servicesWhatsApp
  ) where

import TDF.Config (AppConfig)
import qualified TDF.Email.Service as EmailSvc
import TDF.Email.Service (EmailService)
import qualified TDF.WhatsApp.Service as WASvc
import TDF.WhatsApp.Service (WhatsAppService)

data Services = Services
  { emailService :: EmailService
  , whatsappService :: WhatsAppService
  }

buildServices :: AppConfig -> IO Services
buildServices cfg = do
  whatsappSvc <- WASvc.mkWhatsAppService
  pure Services
    { emailService = EmailSvc.mkEmailService cfg
    , whatsappService = whatsappSvc
    }

servicesWhatsApp :: Services -> WhatsAppService
servicesWhatsApp = whatsappService
