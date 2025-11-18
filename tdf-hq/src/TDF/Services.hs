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
import System.IO.Unsafe (unsafePerformIO)

data Services = Services
  { emailService :: EmailService
  , whatsappService :: WhatsAppService
  }

buildServices :: AppConfig -> Services
buildServices cfg = Services
  { emailService = EmailSvc.mkEmailService cfg
  , whatsappService = unsafePerformIO WASvc.mkWhatsAppService
  }

servicesWhatsApp :: Services -> WhatsAppService
servicesWhatsApp = whatsappService
