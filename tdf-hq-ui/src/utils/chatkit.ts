import { post } from '../api/client';

export interface ChatKitSessionResponse {
  client_secret: string;
  expires_after?: unknown;
}

export const createChatKitClientSecretFetcher = (workflowId: string) =>
  async (currentSecret: string | null) => {
    if (currentSecret) return currentSecret;
    if (!workflowId.trim()) {
      throw new Error('Falta configurar VITE_CHATKIT_WORKFLOW_ID.');
    }
    const payload = await post<ChatKitSessionResponse>('/chatkit/sessions', {
      workflow: { id: workflowId },
    });
    if (!payload.client_secret) {
      throw new Error('Respuesta inválida del backend (sin client_secret).');
    }
    return payload.client_secret;
  };
