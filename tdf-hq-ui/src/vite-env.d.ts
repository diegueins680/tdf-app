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
  readonly VITE_GOOGLE_MAPS_BROWSER_API_KEY?: string;
  readonly VITE_DEFAULT_LOCALE?: string;
  readonly VITE_SUPPORTED_LOCALES?: string;
  readonly VITE_DEFAULT_CURRENCY?: string;
  readonly VITE_SUPPORTED_CURRENCIES?: string;
  readonly VITE_DEFAULT_TIMEZONE?: string;
  readonly VITE_EXCHANGE_RATE_API_BASE?: string;
  readonly VITE_DOMO_TIMEZONE?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

declare const __APP_COMMIT__: string;
declare const __APP_VERSION__: string;
