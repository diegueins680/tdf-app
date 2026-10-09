import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot } from 'react-dom/client';

const reportMock = jest.fn();
const reloadOnceMock = jest.fn(() => true);
jest.unstable_mockModule('../analytics/errorReporting', () => ({ reportClientError: reportMock }));
jest.unstable_mockModule('../utils/lazyWithReload', () => ({ reloadOnceForChunkError: reloadOnceMock }));

const { BootErrorBoundary, installGlobalErrorHandlers } = await import('../bootSafety');
const { default: AppErrorBoundary } = await import('../routes/AppErrorBoundary');

function Thrower({ fail }: { fail: boolean }) {
  if (fail) throw new Error('provider exploded');
  return <p>Comunidad lista</p>;
}

describe('boot safety', () => {
  let consoleError: jest.SpiedFunction<typeof console.error>;
  beforeEach(() => {
    reportMock.mockClear();
    consoleError = jest.spyOn(console, 'error').mockImplementation(() => undefined);
  });
  afterEach(() => consoleError.mockRestore());

  it('renders a dependency-free recovery screen instead of a blank root when a provider throws', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    await act(async () => {
      root.render(<BootErrorBoundary><Thrower fail /></BootErrorBoundary>);
    });
    expect(container.querySelector('[role="alert"]')?.textContent).toContain('No pudimos abrir TDF Records');
    const labels = Array.from(container.querySelectorAll('button')).map((button) => button.textContent);
    expect(labels).toEqual(['Recargar', 'Iniciar sesión']);
    expect(container.textContent).not.toContain('provider exploded');
    expect(reportMock).toHaveBeenCalledWith('boot_render', expect.any(Error), expect.any(Object));
    await act(async () => root.unmount());
    container.remove();
  });

  it('clears an app-level failure when the route changes', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    await act(async () => {
      root.render(<AppErrorBoundary resetKey="/fans"><Thrower fail /></AppErrorBoundary>);
    });
    expect(container.textContent).toContain('No pudimos cargar esta vista.');
    expect(reportMock).toHaveBeenCalledWith('app_render', expect.any(Error), expect.any(Object));
    await act(async () => {
      root.render(<AppErrorBoundary resetKey="/marketplace"><Thrower fail={false} /></AppErrorBoundary>);
    });
    expect(container.textContent).toContain('Comunidad lista');
    await act(async () => root.unmount());
    container.remove();
  });

  it('reports unhandled rejections, blank-screen detections and recovers stale chunk preloads', () => {
    installGlobalErrorHandlers();
    const rejection = new Event('unhandledrejection') as Event & { reason?: unknown };
    rejection.reason = new Error('session fetch failed');
    window.dispatchEvent(rejection);
    expect(reportMock).toHaveBeenCalledWith('unhandled_rejection', rejection.reason);

    const preload = new Event('vite:preloadError', { cancelable: true }) as Event & { payload?: unknown };
    preload.payload = new Error('Failed to fetch dynamically imported module');
    window.dispatchEvent(preload);
    expect(reloadOnceMock).toHaveBeenCalled();
    expect(preload.defaultPrevented).toBe(true);

    (window as Window & { __tdfBlankScreen?: unknown }).__tdfBlankScreen = { state: 'blank', lastRoute: '/fans' };
    window.dispatchEvent(new Event('tdf:blank-screen'));
    expect(reportMock).toHaveBeenCalledWith('blank_screen', 'root blank', { watchdog_route: '/fans' });
  });
});
