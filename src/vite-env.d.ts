/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_BASE?: string;
  readonly VITE_TZ?: string;
  readonly VITE_PIPELINES_API_ENABLED?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
