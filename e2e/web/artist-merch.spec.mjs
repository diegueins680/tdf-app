import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const storeId = '10000000-0000-4000-8000-000000000001';
const productId = '20000000-0000-4000-8000-000000000001';
const variantId = '30000000-0000-4000-8000-000000000001';

const enabledCapabilities = {
  environment: 'staging',
  market: { countryCode: 'EC', currency: 'USD' },
  features: {
    storefronts: true,
    sellerApplications: true,
    publicCatalog: true,
    checkout: false,
    reviews: false,
    notifications: false,
    experimental: false,
  },
  paymentMethods: { datafast: false, paypal: false, bankTransfer: false },
  automaticPayouts: false,
  message: 'Catálogo sintético; checkout deshabilitado.',
};

const product = {
  id: productId,
  storeId,
  storeSlug: 'cementerio-sintetico',
  storeName: 'Cementerio Sintético',
  slug: 'camiseta-camino',
  name: 'Camiseta Abriendo Camino',
  description: 'Producto inequívocamente ficticio para pruebas aisladas.',
  category: 'apparel',
  status: 'published',
  visibility: 'public',
  availabilityMode: 'in_stock',
  buyerLimit: 2,
  priceFromMinor: 2500,
  currency: 'USD',
  available: true,
  imageUrl: null,
  canonicalUrl: '/tienda/cementerio-sintetico/producto/camiseta-camino',
  variants: [{
    id: variantId,
    sku: 'SYN-CAM-M',
    name: 'Talla M',
    optionValues: { talla: 'M' },
    priceMinor: 2500,
    currency: 'USD',
    weightGrams: 220,
    stockMode: 'finite',
    stockOnHand: 8,
    stockReserved: 1,
    stockSold: 0,
    availableQuantity: 7,
    available: true,
    version: 1,
    reorderThreshold: 2,
    active: true,
  }],
  images: [],
  related: [],
  reviewsEnabled: false,
  reviews: [],
};

const storefront = {
  id: storeId,
  profileId: '40000000-0000-4000-8000-000000000001',
  slug: 'cementerio-sintetico',
  displayName: 'Cementerio Sintético',
  description: 'Banda inequívocamente ficticia para pruebas aisladas.',
  coverImageUrl: null,
  logoImageUrl: null,
  countryCode: 'EC',
  currency: 'USD',
  applicationStatus: 'approved',
  operationalStatus: 'active',
  products: [product],
  policies: {
    shipping: 'Envío nacional de prueba, sin operaciones reales.',
    returns: 'Política sintética de devolución.',
  },
  shippingZones: [],
  profile: { url: '/directorio/cementerio-sintetico', name: 'Cementerio Sintético' },
};

async function mockMerchApi(page, capabilities = enabledCapabilities) {
  await page.route('**/session', (route) => route.fulfill({ status: 401, json: { error: 'unauthenticated' } }));
  await page.route('**/fans/artists', (route) => route.fulfill({ json: [] }));
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: {} }));
  await page.route('**/merch/**', (route) => {
    const path = new URL(route.request().url()).pathname;
    if (path === '/merch/capabilities') return route.fulfill({ json: capabilities });
    if (path === '/merch/storefronts') return route.fulfill({ json: [storefront] });
    if (path === '/merch/storefronts/cementerio-sintetico') return route.fulfill({ json: storefront });
    if (path === '/merch/storefronts/cementerio-sintetico/products/camiseta-camino') {
      return route.fulfill({ json: product });
    }
    return route.fulfill({ status: 404, json: { error: 'Synthetic route not configured' } });
  });
}

async function expectNoSeriousAxeViolations(page, testInfo) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => {
    const result = await globalThis.axe.run(document, { resultTypes: ['violations'] });
    return result.violations
      .filter((violation) => ['critical', 'serious'].includes(violation.impact))
      .map(({ id, impact, help, nodes }) => ({
        id,
        impact,
        help,
        nodes: nodes.map((node) => node.target),
      }));
  });
  await testInfo.attach('artist-merch-axe.json', {
    body: JSON.stringify(violations, null, 2),
    contentType: 'application/json',
  });
  expect(violations).toEqual([]);
}

test('PW-MERCH-01 keeps the closed pilot honest and actionable', async ({ page }, testInfo) => {
  await mockMerchApi(page, {
    ...enabledCapabilities,
    features: { ...enabledCapabilities.features, storefronts: false, sellerApplications: false, publicCatalog: false },
  });

  await page.goto('/tiendas', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('alert')).toContainText('piloto cerrado');
  await expect(page.getByText(/comprar|vender/i)).toHaveCount(0);
  await expectNoSeriousAxeViolations(page, testInfo);
});

test('PW-MERCH-02 discovers a store and product responsively without promising checkout', async ({ page }, testInfo) => {
  await mockMerchApi(page);

  await page.goto('/tiendas', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('heading', { name: 'Merch de artistas' })).toBeVisible();
  await expect(page.getByRole('link', { name: /Cementerio Sintético/ })).toBeVisible();
  await page.getByLabel('Buscar tiendas o productos').fill('Cementerio');
  await expect(page.getByText('1 producto')).toBeVisible();
  await page.getByRole('link', { name: /Cementerio Sintético/ }).click();

  await expect(page).toHaveURL(/\/tienda\/cementerio-sintetico$/);
  await expect(page.getByRole('heading', { name: 'Cementerio Sintético' })).toBeVisible();
  await expect(page.getByText('Envío nacional de prueba')).toBeVisible();
  await page.getByRole('link', { name: /Camiseta Abriendo Camino/ }).click();

  await expect(page.getByRole('heading', { name: 'Camiseta Abriendo Camino' })).toBeVisible();
  await expect(page.getByText('Las compras siguen deshabilitadas durante el piloto. Puedes explorar el catálogo.')).toBeVisible();
  await expect(page.getByRole('button', { name: 'Agregar al carrito' })).toBeDisabled();
  await expect(page.getByLabel('Variante')).toContainText('Talla M');
  await expectNoSeriousAxeViolations(page, testInfo);

  await testInfo.attach(`artist-merch-${testInfo.project.name}.png`, {
    body: await page.screenshot({ fullPage: true }),
    contentType: 'image/png',
  });
});
