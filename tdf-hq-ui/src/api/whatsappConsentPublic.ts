import { get, post } from './client';
import type {
  WhatsAppConsentRequest,
  WhatsAppConsentResponse,
  WhatsAppConsentStatus,
  WhatsAppOptOutRequest,
} from './whatsappConsent';

export const WhatsAppConsentPublicAPI = {
  createConsent: (payload: WhatsAppConsentRequest) =>
    post<WhatsAppConsentResponse>('/public/whatsapp/consent', payload),
  optOut: (payload: WhatsAppOptOutRequest) =>
    post<WhatsAppConsentResponse>('/public/whatsapp/opt-out', payload),
  fetchStatus: (phone: string) =>
    get<WhatsAppConsentStatus>(`/public/whatsapp/consent?phone=${encodeURIComponent(phone)}`),
};
