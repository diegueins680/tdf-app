/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_BASE?: string;
  readonly VITE_API_DEMO_TOKEN?: string;
  readonly VITE_LIVE_SESSIONS_PUBLIC_TOKEN?: string;
  readonly VITE_CHATKIT_WORKFLOW_ID?: string;
  readonly VITE_META_APP_ID?: string;
  readonly VITE_FACEBOOK_APP_ID?: string;
  readonly VITE_INSTAGRAM_CLIENT_ID?: string;
  readonly VITE_INSTAGRAM_APP_ID?: string;
  readonly VITE_INSTAGRAM_REDIRECT_URI?: string;
  readonly VITE_INSTAGRAM_OAUTH_PROVIDER?: string;
  readonly VITE_INSTAGRAM_SCOPES?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

declare const __APP_COMMIT__: string;
declare const __APP_VERSION__: string;
