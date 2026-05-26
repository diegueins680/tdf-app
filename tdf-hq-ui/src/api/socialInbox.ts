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
  const queryString = qs.toString();
  return queryString ? `?${queryString}` : '';
};

const stripFormatPrefix = (value: string, prefix: 'SEND' | 'HOLD' | 'NEED') => {
  const pattern = new RegExp(`^${prefix}:\\s*`, 'i');
  return value.replace(pattern, '').trim();
};

export const parseReplySuggestion = (rawReply: string): ReplySuggestion => {
  const trimmedReply = rawReply.trim();
  const replyLowercase = trimmedReply.toLowerCase();
  if (replyLowercase.startsWith('send:')) {
    return { kind: 'send', text: stripFormatPrefix(trimmedReply, 'SEND') };
  }
  if (replyLowercase.startsWith('hold:')) {
    const holdBody = stripFormatPrefix(trimmedReply, 'HOLD');
    const needMatch = /\n\s*NEED:\s*/i.exec(holdBody);
    if (!needMatch) {
      const fallbackHoldReason = holdBody.trim();
      return {
        kind: 'hold',
        reason: fallbackHoldReason,
        neededInfo: fallbackHoldReason,
        raw: trimmedReply,
      };
    }
    const holdReason = holdBody.slice(0, needMatch.index).trim();
    const holdNeededInfo = holdBody.slice(needMatch.index + needMatch[0].length).trim();
    return { kind: 'hold', reason: holdReason, neededInfo: holdNeededInfo, raw: trimmedReply };
  }
  return { kind: 'send', text: trimmedReply };
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
    const replySenderId = payload.senderId.trim();
    const replyMessage = payload.message.trim();
    if (!replySenderId) throw new Error('Remitente inválido.');
    if (!replyMessage) throw new Error('Escribe una respuesta antes de enviar.');

    const safeExternalId = payload.externalId?.trim();

    const sendReplyResult = await (async (): Promise<ReplyStatusResponse> => {
      switch (channel) {
        case 'instagram':
          return post<ReplyStatusResponse>('/instagram/reply', {
            irSenderId: replySenderId,
            irMessage: replyMessage,
            ...(safeExternalId ? { irExternalId: safeExternalId } : {}),
          });
        case 'facebook':
          return post<ReplyStatusResponse>('/facebook/reply', {
            frSenderId: replySenderId,
            frMessage: replyMessage,
            ...(safeExternalId ? { frExternalId: safeExternalId } : {}),
          });
        case 'whatsapp':
          return post<ReplyStatusResponse>('/whatsapp/reply', {
            wrSenderId: replySenderId,
            wrMessage: replyMessage,
            ...(safeExternalId ? { wrExternalId: safeExternalId } : {}),
          });
      }

      return assertNever(channel, 'social channel');
    })();

    if (sendReplyResult?.status === 'error') {
      throw new Error(sendReplyResult?.message ?? 'No se pudo enviar el mensaje.');
    }
    return sendReplyResult;
  },

  askOperatorQuestion: async (payload: OperatorQuestionRequest) => {
    const operatorSenderId = payload.senderId.trim();
    const inboundMessage = payload.inboundMessage.trim();
    const operatorHoldReason = payload.holdReason.trim();
    const operatorNeededInfo = payload.neededInfo.trim();
    if (!operatorSenderId) throw new Error('Remitente inválido.');
    if (!inboundMessage) throw new Error('Mensaje entrante vacío.');
    if (!operatorHoldReason) throw new Error('Motivo de consulta vacío.');
    if (!operatorNeededInfo) throw new Error('Pregunta para Diego vacía.');

    const operatorQuestionResult = await post<ReplyStatusResponse>('/whatsapp/operator-question', {
      channel: payload.channel,
      senderId: operatorSenderId,
      ...(payload.externalId?.trim() ? { externalId: payload.externalId.trim() } : {}),
      inboundMessage,
      holdReason: operatorHoldReason,
      neededInfo: operatorNeededInfo,
    });

    if (operatorQuestionResult?.status === 'error') {
      throw new Error(operatorQuestionResult?.message ?? 'No se pudo enviar la pregunta por WhatsApp.');
    }
    return operatorQuestionResult;
  },

  suggestReply: async (channel: SocialChannel, message: string, hint?: string) => {
    const suggestionMessage = message.trim();
    if (!suggestionMessage) throw new Error('El mensaje está vacío.');
    const hintClean = hint?.trim();
    const prompt = hintClean ? `${suggestionMessage}\n\nInstrucciones adicionales:\n${hintClean}` : suggestionMessage;

    const res = await post<AdsAssistResponse>('/ads/assist', {
      aarMessage: prompt,
      aarChannel: channel,
    });
    const reply = res?.aasReply;
    if (!reply || typeof reply !== 'string') throw new Error('Respuesta IA vacía o inválida.');
    return parseReplySuggestion(reply);
  },
};
