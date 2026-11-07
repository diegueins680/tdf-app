import type { ErrorInfo, ReactNode } from 'react';
import { Component } from 'react';
import { reportClientError } from './analytics/errorReporting';
import { reloadOnceForChunkError } from './utils/lazyWithReload';

/**
 * Outermost safety net. AppErrorBoundary lives inside the theme, session,
 * query, toast, locale and currency providers, so a failure in any of those
 * (or in that boundary's own MUI fallback) used to unmount the whole tree and
 * leave a blank page. This boundary depends on nothing but React and renders
 * plain, inline-styled markup with recovery actions.
 */
export class BootErrorBoundary extends Component<{ children: ReactNode }, { failed: boolean }> {
  override state = { failed: false };

  static getDerivedStateFromError(): { failed: boolean } {
    return { failed: true };
  }

  override componentDidCatch(error: Error, info: ErrorInfo) {
    reportClientError('boot_render', error, {
      component_stack: (info.componentStack ?? '').split('\n').slice(0, 6).join(' | ').slice(0, 300),
    });
  }

  override render() {
    if (!this.state.failed) return this.props.children;
    const buttonStyle = {
      minHeight: 44,
      padding: '10px 16px',
      borderRadius: 10,
      font: '600 15px system-ui, sans-serif',
      cursor: 'pointer',
    } as const;
    return (
      <div
        role="alert"
        style={{
          minHeight: '100vh',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          padding: '24px 16px',
          background: '#f8fafc',
          color: '#111827',
          fontFamily: 'system-ui, sans-serif',
        }}
      >
        <div style={{ maxWidth: 420, width: '100%' }}>
          <h1 style={{ fontSize: 20, margin: '0 0 8px' }}>No pudimos abrir TDF Records</h1>
          <p style={{ margin: '0 0 16px', lineHeight: 1.5 }}>
            Algo falló al iniciar la aplicación. Recarga la página; si continúa, vuelve a iniciar sesión.
          </p>
          <div style={{ display: 'flex', flexWrap: 'wrap', gap: 8 }}>
            <button
              type="button"
              style={{ ...buttonStyle, background: '#6d28d9', color: '#fff', border: 0 }}
              onClick={() => window.location.reload()}
            >
              Recargar
            </button>
            <button
              type="button"
              style={{ ...buttonStyle, background: '#fff', color: '#4c1d95', border: '1px solid #6d28d9' }}
              onClick={() => window.location.assign('/login')}
            >
              Iniciar sesión
            </button>
          </div>
        </div>
      </div>
    );
  }
}

let installed = false;

/**
 * Report failures that never reach a React boundary (event handlers, async
 * effects, third-party scripts) and recover from stale chunk preloads after a
 * deploy. Handlers only observe; they never swallow application errors.
 */
export function installGlobalErrorHandlers(): void {
  if (installed || typeof window === 'undefined') return;
  installed = true;

  window.addEventListener('error', (event: ErrorEvent) => {
    // Resource load failures (img/script tags) have no `error` object and are
    // handled where they are requested.
    if (!event.error && !event.message) return;
    reportClientError('window_error', event.error ?? event.message, {
      source: (event.filename ?? '').replace(/^https?:\/\/[^/]+/, '').slice(0, 120),
    });
  });

  window.addEventListener('unhandledrejection', (event: PromiseRejectionEvent) => {
    reportClientError('unhandled_rejection', event.reason);
  });

  // Vite fires this when a lazily preloaded chunk/CSS from an older deploy no
  // longer exists. Reload once into the current asset graph instead of
  // leaving the route half-rendered.
  window.addEventListener('vite:preloadError', (event: Event) => {
    const payload = (event as Event & { payload?: unknown }).payload;
    reportClientError('chunk_preload', payload ?? 'vite:preloadError');
    if (reloadOnceForChunkError()) event.preventDefault();
  });

  const reportBlankScreen = () => {
    const marker = (window as Window & { __tdfBlankScreen?: { state?: string; lastRoute?: string | null } }).__tdfBlankScreen;
    reportClientError('blank_screen', `root ${marker?.state ?? 'blank'}`, { watchdog_route: marker?.lastRoute ?? null });
  };
  window.addEventListener('tdf:blank-screen', reportBlankScreen);
}
