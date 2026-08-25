import { post } from '../api/client';

export const CHATKIT_SCRIPT_URL = 'https://cdn.platform.openai.com/deployments/chatkit/chatkit.js';
export const CHATKIT_SCRIPT_LOAD_TIMEOUT_MS = 10_000;

const CHATKIT_SCRIPT_ID = 'openai-chatkit-web-component';

let chatKitScriptPromise: Promise<void> | null = null;

export interface ChatKitSessionResponse {
  client_secret: string;
  expires_after?: unknown;
}

export interface LoadChatKitScriptOptions {
  /**
   * Precondition: positive finite timeout in milliseconds.
   * Defaults to CHATKIT_SCRIPT_LOAD_TIMEOUT_MS.
   */
  timeoutMs?: number;
}

export function isChatKitScriptReady(): boolean {
  return (
    typeof window !== 'undefined'
    && typeof window.customElements !== 'undefined'
    && Boolean(window.customElements.get('openai-chatkit'))
  );
}

const getExistingChatKitScript = (): HTMLScriptElement | null => {
  if (typeof document === 'undefined') return null;
  const byId = document.getElementById(CHATKIT_SCRIPT_ID);
  if (byId instanceof HTMLScriptElement) return byId;
  return Array.from(document.scripts).find((script) => script.src === CHATKIT_SCRIPT_URL) ?? null;
};

const resolveChatKitScriptLoadTimeoutMs = (options: LoadChatKitScriptOptions): number => {
  const timeoutMs = options.timeoutMs ?? CHATKIT_SCRIPT_LOAD_TIMEOUT_MS;
  if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
    throw new Error('El timeout de carga de ChatKit debe ser un numero positivo en milisegundos.');
  }
  return timeoutMs;
};

export function loadChatKitScript(options: LoadChatKitScriptOptions = {}): Promise<void> {
  if (isChatKitScriptReady()) return Promise.resolve();
  if (typeof window === 'undefined' || typeof document === 'undefined') {
    return Promise.reject(new Error('ChatKit solo esta disponible en el navegador.'));
  }
  if (typeof window.customElements === 'undefined') {
    return Promise.reject(new Error('Este navegador no soporta componentes web.'));
  }
  if (chatKitScriptPromise) return chatKitScriptPromise;

  let timeoutMs: number;
  try {
    timeoutMs = resolveChatKitScriptLoadTimeoutMs(options);
  } catch (error) {
    return Promise.reject(error instanceof Error ? error : new Error('Timeout de ChatKit invalido.'));
  }

  chatKitScriptPromise = new Promise<void>((resolve, reject) => {
    const existingScript = getExistingChatKitScript();
    const script = existingScript ?? document.createElement('script');
    let settled = false;
    let timeoutId: number | null = null;

    const cleanup = () => {
      if (timeoutId) {
        window.clearTimeout(timeoutId);
        timeoutId = null;
      }
      script.removeEventListener('load', waitForDefinition);
      script.removeEventListener('error', handleError);
    };

    const fail = (error: Error) => {
      if (settled) return;
      settled = true;
      cleanup();
      chatKitScriptPromise = null;
      if (script.dataset['chatkitManaged'] === 'true') {
        script.remove();
      }
      reject(error);
    };

    const finish = () => {
      if (settled) return;
      settled = true;
      cleanup();
      resolve();
    };

    function waitForDefinition() {
      if (isChatKitScriptReady()) {
        finish();
        return;
      }
      window.customElements.whenDefined('openai-chatkit').then(finish).catch((error: unknown) => {
        fail(error instanceof Error ? error : new Error('No se pudo inicializar ChatKit.'));
      });
    }

    function handleError() {
      fail(new Error('No se pudo cargar ChatKit. Revisa la conexion e intenta de nuevo.'));
    }

    script.addEventListener('load', waitForDefinition, { once: true });
    script.addEventListener('error', handleError, { once: true });

    timeoutId = window.setTimeout(() => {
      fail(new Error('ChatKit tardo demasiado en cargar.'));
    }, timeoutMs);

    if (!existingScript) {
      script.id = CHATKIT_SCRIPT_ID;
      script.src = CHATKIT_SCRIPT_URL;
      script.async = true;
      script.crossOrigin = 'anonymous';
      script.dataset['chatkitManaged'] = 'true';
      document.head.appendChild(script);
    } else {
      waitForDefinition();
    }
  });

  return chatKitScriptPromise;
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
