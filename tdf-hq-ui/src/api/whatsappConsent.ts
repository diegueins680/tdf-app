import { get, post } from './client';

export interface WhatsAppConsentStatus {
  phone: string;
  consent: boolean;
  consentedAt?: string | null;
  revokedAt?: string | null;
  displayName?: string | null;
}

export interface WhatsAppConsentResponse {
  status: WhatsAppConsentStatus;
  messageSent: boolean;
  message?: string | null;
}

export interface WhatsAppConsentRequest {
  phone: string;
  name?: string | null;
  consent: boolean;
  source?: string | null;
  sendMessage?: boolean | null;
}

export interface WhatsAppOptOutRequest {
  phone: string;
  reason?: string | null;
  sendMessage?: boolean | null;
}

export const WhatsAppConsentAPI = {
  createConsent: (payload: WhatsAppConsentRequest) =>
    post<WhatsAppConsentResponse>('/whatsapp/consent', payload),
  optOut: (payload: WhatsAppOptOutRequest) =>
    post<WhatsAppConsentResponse>('/whatsapp/opt-out', payload),
  fetchStatus: (phone: string) =>
    get<WhatsAppConsentStatus>(`/whatsapp/consent?phone=${encodeURIComponent(phone)}`),
};
