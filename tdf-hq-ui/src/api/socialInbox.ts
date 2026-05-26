import { get, post } from './client';
import { assertNever } from '../utils/assertNever';

export interface SocialMessage {
  externalId: string;
  senderId: string;
  senderName?: string | null;
  text?: string | null;
  metadata?: string | null;
  direction: string;
  repliedAt?: string | null;
  replyText?: string | null;
  replyError?: string | null;
  createdAt: string;
}

export type SocialChannel = 'instagram' | 'facebook' | 'whatsapp';

interface ReplyStatusResponse {
  status?: string;
  message?: string;
  response?: unknown;
}

interface ReplyRequestBase {
  senderId: string;
  message: string;
  externalId?: string;
}

export type ReplySuggestion =
  | { kind: 'send'; text: string }
  | { kind: 'hold'; reason: string; neededInfo: string; raw: string };

interface OperatorQuestionRequest {
  channel: SocialChannel;
  senderId: string;
  externalId?: string;
  inboundMessage: string;
  holdReason: string;
  neededInfo: string;
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

const stripFormatPrefix = (value: string, prefix: 'SEND' | 'HOLD' | 'NEED') => {
  const pattern = new RegExp(`^${prefix}:\\s*`, 'i');
  return value.replace(pattern, '').trim();
};

export const parseReplySuggestion = (rawReply: string): ReplySuggestion => {
  const raw = rawReply.trim();
  const lower = raw.toLowerCase();
  if (lower.startsWith('send:')) {
    return { kind: 'send', text: stripFormatPrefix(raw, 'SEND') };
  }
  if (lower.startsWith('hold:')) {
    const withoutHold = stripFormatPrefix(raw, 'HOLD');
    const needMatch = withoutHold.match(/\n\s*NEED:\s*/i);
    if (!needMatch || needMatch.index === undefined) {
      const reason = withoutHold.trim();
      return { kind: 'hold', reason, neededInfo: reason, raw };
    }
    const reason = withoutHold.slice(0, needMatch.index).trim();
    const neededInfo = withoutHold.slice(needMatch.index + needMatch[0].length).trim();
    return { kind: 'hold', reason, neededInfo, raw };
  }
  return { kind: 'send', text: raw };
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

    const safeExternalId = payload.externalId?.trim();

    const result = await (async (): Promise<ReplyStatusResponse> => {
      switch (channel) {
        case 'instagram':
          return post<ReplyStatusResponse>('/instagram/reply', {
            irSenderId: senderId,
            irMessage: message,
            ...(safeExternalId ? { irExternalId: safeExternalId } : {}),
          });
        case 'facebook':
          return post<ReplyStatusResponse>('/facebook/reply', {
            frSenderId: senderId,
            frMessage: message,
            ...(safeExternalId ? { frExternalId: safeExternalId } : {}),
          });
        case 'whatsapp':
          return post<ReplyStatusResponse>('/whatsapp/reply', {
            wrSenderId: senderId,
            wrMessage: message,
            ...(safeExternalId ? { wrExternalId: safeExternalId } : {}),
          });
      }

      return assertNever(channel, 'social channel');
    })();

    if (result?.status === 'error') {
      throw new Error(result?.message ?? 'No se pudo enviar el mensaje.');
    }
    return result;
  },

  askOperatorQuestion: async (payload: OperatorQuestionRequest) => {
    const senderId = payload.senderId.trim();
    const inboundMessage = payload.inboundMessage.trim();
    const holdReason = payload.holdReason.trim();
    const neededInfo = payload.neededInfo.trim();
    if (!senderId) throw new Error('Remitente inválido.');
    if (!inboundMessage) throw new Error('Mensaje entrante vacío.');
    if (!holdReason) throw new Error('Motivo de consulta vacío.');
    if (!neededInfo) throw new Error('Pregunta para Diego vacía.');

    const result = await post<ReplyStatusResponse>('/whatsapp/operator-question', {
      channel: payload.channel,
      senderId,
      ...(payload.externalId?.trim() ? { externalId: payload.externalId.trim() } : {}),
      inboundMessage,
      holdReason,
      neededInfo,
    });

    if (result?.status === 'error') {
      throw new Error(result?.message ?? 'No se pudo enviar la pregunta por WhatsApp.');
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
    return parseReplySuggestion(reply);
  },
};
