import { get, post } from './client';

export interface SocialMessage {
  externalId: string;
  senderId: string;
  senderName?: string | null;
  text?: string | null;
  direction: string;
  repliedAt?: string | null;
  replyText?: string | null;
  replyError?: string | null;
  createdAt: string;
}

export type SocialChannel = 'instagram' | 'facebook' | 'whatsapp';

interface ReplyStatusResponse {
  status?: 'ok' | 'error' | string;
  message?: string;
  response?: unknown;
}

interface ReplyRequestBase {
  senderId: string;
  message: string;
  externalId?: string;
}

interface AdsAssistResponse {
  aasReply: string;
}

interface SocialInboxFilters {
  limit?: number;
  direction?: 'incoming' | 'outgoing' | 'all';
  repliedOnly?: boolean;
}

const buildQuery = (params: Record<string, string | number | boolean | undefined | null>) => {
  const qs = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    if (value === undefined || value === null) return;
    qs.set(key, String(value));
  });
  const raw = qs.toString();
  return raw ? `?${raw}` : '';
};

export const SocialInboxAPI = {
  listInstagramMessages: (filters: SocialInboxFilters = {}) =>
    get<SocialMessage[]>(
      `/instagram/messages${buildQuery({
        limit: filters.limit,
        direction: filters.direction,
        repliedOnly: filters.repliedOnly,
      })}`,
    ),
  listFacebookMessages: (filters: SocialInboxFilters = {}) =>
    get<SocialMessage[]>(
      `/facebook/messages${buildQuery({
        limit: filters.limit,
        direction: filters.direction,
        repliedOnly: filters.repliedOnly,
      })}`,
    ),
  listWhatsAppMessages: (filters: SocialInboxFilters = {}) =>
    get<SocialMessage[]>(
      `/whatsapp/messages${buildQuery({
        limit: filters.limit,
        direction: filters.direction,
        repliedOnly: filters.repliedOnly,
      })}`,
    ),

  sendReply: async (channel: SocialChannel, payload: ReplyRequestBase) => {
    const senderId = payload.senderId.trim();
    const message = payload.message.trim();
    if (!senderId) throw new Error('Remitente inválido.');
    if (!message) throw new Error('Escribe una respuesta antes de enviar.');

    const externalId = payload.externalId?.trim();
    const safeExternalId = externalId ? externalId : undefined;

    let result: ReplyStatusResponse;
    switch (channel) {
      case 'instagram':
        result = await post<ReplyStatusResponse>('/instagram/reply', {
          irSenderId: senderId,
          irMessage: message,
          ...(safeExternalId ? { irExternalId: safeExternalId } : {}),
        });
        break;
      case 'facebook':
        result = await post<ReplyStatusResponse>('/facebook/reply', {
          frSenderId: senderId,
          frMessage: message,
          ...(safeExternalId ? { frExternalId: safeExternalId } : {}),
        });
        break;
      case 'whatsapp':
        result = await post<ReplyStatusResponse>('/whatsapp/reply', {
          wrSenderId: senderId,
          wrMessage: message,
          ...(safeExternalId ? { wrExternalId: safeExternalId } : {}),
        });
        break;
      default:
        throw new Error('Canal no soportado.');
    }

    if (result?.status === 'error') {
      throw new Error(result?.message || 'No se pudo enviar el mensaje.');
    }
    return result;
  },

  suggestReply: async (channel: SocialChannel, message: string, hint?: string) => {
    const trimmed = message.trim();
    if (!trimmed) throw new Error('El mensaje está vacío.');
    const hintClean = hint?.trim();
    const prompt = hintClean ? `${trimmed}\n\nInstrucciones adicionales:\n${hintClean}` : trimmed;

    const res = await post<AdsAssistResponse>('/ads/assist', {
      aarMessage: prompt,
      aarChannel: channel,
    });
    const reply = res?.aasReply;
    if (!reply || typeof reply !== 'string') throw new Error('Respuesta IA vacía o inválida.');
    return reply.trim();
  },
};
