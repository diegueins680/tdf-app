import { useEffect, useState } from 'react';

import { isChatKitScriptReady, loadChatKitScript } from '../utils/chatkit';

export type ChatKitScriptStatus = 'idle' | 'loading' | 'ready' | 'error';

export interface ChatKitScriptState {
  status: ChatKitScriptStatus;
  errorMessage: string | null;
}

const readInitialStatus = (): ChatKitScriptStatus => (
  isChatKitScriptReady() ? 'ready' : 'idle'
);

export function useChatKitScript(enabled: boolean): ChatKitScriptState {
  const [state, setState] = useState<ChatKitScriptState>(() => ({
    status: readInitialStatus(),
    errorMessage: null,
  }));

  useEffect(() => {
    if (!enabled) return undefined;
    if (isChatKitScriptReady()) {
      setState({ status: 'ready', errorMessage: null });
      return undefined;
    }

    let cancelled = false;
    setState({ status: 'loading', errorMessage: null });

    loadChatKitScript()
      .then(() => {
        if (!cancelled) {
          setState({ status: 'ready', errorMessage: null });
        }
      })
      .catch((error: unknown) => {
        if (!cancelled) {
          setState({
            status: 'error',
            errorMessage: error instanceof Error ? error.message : 'No se pudo cargar ChatKit.',
          });
        }
      });

    return () => {
      cancelled = true;
    };
  }, [enabled]);

  return state;
}
