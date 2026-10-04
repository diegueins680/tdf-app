import { expect, test } from '@playwright/test';

for (const operation of ['getter', 'getItem']) {
  for (const kind of ['tracking', 'return']) {
    test(`Private service ${kind} remains recoverable without optional storage ${operation} @critical`, async ({ page, baseURL }) => {
      const origin = new URL(baseURL).origin;
      const serviceRequests = [];
      await page.addInitScript(denial => {
        if (window.top !== window) return;
        const deny = () => { throw new DOMException('Synthetic cache denial', 'SecurityError'); };
        if (denial === 'getter') Object.defineProperty(window, 'sessionStorage', { configurable: true, get: deny });
        else Storage.prototype.getItem = deny;
      }, operation);
      await page.route('**/*', async route => {
        const request = route.request(), url = new URL(request.url());
        if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
        const path = url.pathname.replace(/^\/api(?=\/)/, '');
        if (path === '/session') return route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
        if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
        if (path.includes('service') || path.includes('datafast')) serviceRequests.push({ method: request.method(), path });
        return route.fulfill({ json: { items: [] } });
      });
      const path = kind === 'tracking' ? '/mezcla-mastering/pedido/synthetic-order' : '/mezcla-mastering/pago-datafast?orderId=synthetic-order&resourcePath=synthetic-path';
      await page.goto(path);
      await expect(page.getByRole('alert').filter({ hasText: kind === 'tracking' ? 'Usa el enlace original o solicita ayuda a TDF.' : 'Faltan los datos privados necesarios' })).toBeVisible();
      await expect(page.getByText('No pudimos cargar esta vista.', { exact: true })).toHaveCount(0);
      await expect(page.getByText('El pago está confirmado.', { exact: false })).toHaveCount(0);
      expect(serviceRequests).toEqual([]);
    });
  }
}
