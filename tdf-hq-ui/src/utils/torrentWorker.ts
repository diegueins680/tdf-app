const TORRENT_WORKER_URL = '/webtorrent-sw.min.js';
const TORRENT_WORKER_SCOPE = '/';
const WORKER_TIMEOUT_MS = 15_000;

function assertOwnedWorker(worker: ServiceWorker | null, scriptUrl: string): void {
  if (worker && worker.scriptURL !== scriptUrl) {
    throw new Error('El reproductor no puede iniciar con el servicio actual de esta página.');
  }
}

function assertOwnedRegistration(registration: ServiceWorkerRegistration, scriptUrl: string): void {
  if (registration.scope !== new URL(TORRENT_WORKER_SCOPE, window.location.href).href) {
    throw new Error('No se pudo iniciar el reproductor en esta página.');
  }
  for (const worker of [registration.active, registration.installing, registration.waiting]) {
    assertOwnedWorker(worker, scriptUrl);
  }
}

function waitForWorkerControl(registration: ServiceWorkerRegistration, scriptUrl: string): Promise<void> {
  const container = navigator.serviceWorker;
  return new Promise((resolve, reject) => {
    const observed = new Set<ServiceWorker>();
    const claimRequested = new Set<ServiceWorker>();
    const cleanup = () => {
      window.clearTimeout(timeout);
      container.removeEventListener('controllerchange', check);
      registration.removeEventListener('updatefound', check);
      for (const worker of observed) worker.removeEventListener('statechange', check);
    };
    const check = () => {
      try {
        assertOwnedRegistration(registration, scriptUrl);
        assertOwnedWorker(container.controller, scriptUrl);
      } catch (error) {
        cleanup(); reject(error instanceof Error ? error : new Error(String(error))); return;
      }
      const candidates = [registration.active, registration.installing, registration.waiting];
      for (const worker of [...candidates, container.controller]) {
        if (worker && !observed.has(worker)) {
          observed.add(worker);
          worker.addEventListener('statechange', check);
        }
      }
      if (registration.active?.state === 'activated' && container.controller === registration.active) {
        cleanup(); resolve();
      } else if (registration.active?.state === 'activated' && !claimRequested.has(registration.active)) {
        const worker = registration.active;
        claimRequested.add(worker);
        try { worker.postMessage({ type: 'tdf-torrent-claim' }); }
        catch (error) { cleanup(); reject(error instanceof Error ? error : new Error(String(error))); }
      } else if (candidates.every(worker => !worker || worker.state === 'redundant')) {
        cleanup(); reject(new Error('No se pudo activar el servicio de torrents.'));
      }
    };
    const timeout = window.setTimeout(() => {
      cleanup();
      reject(new Error('El reproductor tardó demasiado en activarse en esta página.'));
    }, WORKER_TIMEOUT_MS);
    container.addEventListener('controllerchange', check);
    registration.addEventListener('updatefound', check);
    check();
  });
}

export async function ensureTorrentWorker(): Promise<ServiceWorkerRegistration> {
  if (!('serviceWorker' in navigator)) {
    throw new Error('Este navegador no permite reproducir torrents.');
  }
  const container = navigator.serviceWorker;
  const scriptUrl = new URL(TORRENT_WORKER_URL, window.location.href).href;
  assertOwnedWorker(container.controller, scriptUrl);
  const existing = await container.getRegistration(TORRENT_WORKER_SCOPE);
  assertOwnedWorker(container.controller, scriptUrl);
  if (existing) assertOwnedRegistration(existing, scriptUrl);

  // The browser's registration is authoritative; do not cache a fulfilled promise
  // that could outlive its registration or the current page's controller.
  const registration = existing ?? await container.register(TORRENT_WORKER_URL, { scope: TORRENT_WORKER_SCOPE });
  assertOwnedRegistration(registration, scriptUrl);
  await waitForWorkerControl(registration, scriptUrl);
  return registration;
}
