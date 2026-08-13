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
  readonly VITE_ADA_USD_RATE?: string;
  readonly VITE_CALENDAR_ICS_BASE?: string;
  readonly VITE_CARDANO_ADDRESS?: string;
  readonly VITE_COURSE_COHORTS?: string;
  readonly VITE_COURSE_INSTRUCTOR_AVATAR?: string;
  readonly VITE_COURSE_MAP_URL?: string;
  readonly VITE_COURSE_SLUG?: string;
  readonly VITE_COURSE_WHATSAPP_URL?: string;
  readonly VITE_DEFAULT_DEMO_TOKEN?: string;
  readonly VITE_DEMO_TOKEN_HOSTS?: string;
  readonly VITE_GOOGLE_CLIENT_ID?: string;
  readonly VITE_GOOGLE_DRIVE_CLIENT_ID?: string;
  readonly VITE_GOOGLE_DRIVE_FOLDER_ID?: string;
  readonly VITE_GOOGLE_DRIVE_REDIRECT_URI?: string;
  readonly VITE_INVENTORY_SCAN_BASE?: string;
  readonly VITE_PAYPAL_CLIENT_ID?: string;
  readonly VITE_POSTHOG_HOST?: string;
  readonly VITE_POSTHOG_KEY?: string;
  readonly VITE_PUBLIC_BASE?: string;
  readonly VITE_PUBLIC_BOOKING_TOKEN?: string;
  readonly VITE_PUBLIC_COURSE_BASE?: string;
  readonly VITE_SED_USD_RATE?: string;
  readonly VITE_STRIPE_PUBLISHABLE_KEY?: string;
  readonly VITE_STUDIO_MAP_URL?: string;
  readonly VITE_STUDIO_WHATSAPP_URL?: string;
  readonly VITE_TIDAL_AGENT_MODEL?: string;
  readonly VITE_TRIALS_WHATSAPP_URL?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

declare const __APP_COMMIT__: string;
declare const __APP_VERSION__: string;
