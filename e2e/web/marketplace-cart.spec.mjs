import { expect, test } from '@playwright/test';

// Synthetic API/browser integration for cart discoverability on small phones.
// Every API call is fulfilled from in-memory fixtures; no real cart, order or
// payment provider is touched.

const LISTINGS = [
  {
    miListingId: 'listing-mic',
    miAssetId: 'asset-mic',
    miPurpose: 'sale',
    miTitle: 'Micrófono Vintage SM58',
    miCategory: 'Micrófonos',
    miBrand: 'Shure',
    miModel: 'SM58',
    miPhotoUrl: null,
    miStatus: 'En stock',
    miCondition: 'used',
    miPriceUsdCents: 10000,
    miPriceDisplay: 'USD $100.00',
    miMarkupPct: 0,
    miCurrency: 'USD',
  },
  {
    miListingId: 'listing-cable',
    miAssetId: 'asset-cable',
    miPurpose: 'sale',
    miTitle: 'Cable XLR 10 m',
    miCategory: 'Cables',
    miBrand: 'Mogami',
    miModel: 'Gold',
    miPhotoUrl: null,
    miStatus: 'En stock',
    miCondition: 'new',
    miPriceUsdCents: 2500,
    miPriceDisplay: 'USD $25.00',
    miMarkupPct: 0,
    miCurrency: 'USD',
  },
];

const CART_ID = '00000000-0000-4000-8000-00000000ca27';

function buildCart(state) {
  const items = state.items.map((entry) => {
    const listing = LISTINGS.find((candidate) => candidate.miListingId === entry.listingId);
    return {
      mciListingId: listing.miListingId,
      mciTitle: listing.miTitle,
      mciCategory: listing.miCategory,
      mciBrand: listing.miBrand,
      mciModel: listing.miModel,
      mciQuantity: entry.quantity,
      mciUnitPriceUsdCents: listing.miPriceUsdCents,
      mciSubtotalCents: listing.miPriceUsdCents * entry.quantity,
      mciUnitPriceDisplay: listing.miPriceDisplay,
      mciSubtotalDisplay: listing.miPriceDisplay,
      mciPurpose: 'sale',
    };
  });
  const subtotal = items.reduce((acc, item) => acc + item.mciSubtotalCents, 0);
  return {
    mcCartId: CART_ID,
    mcItems: items,
    mcCurrency: 'USD',
    mcSubtotalCents: subtotal,
    mcSubtotalDisplay: `USD $${(subtotal / 100).toFixed(2)}`,
  };
}

async function fixture(page, baseURL) {
  const origin = new URL(baseURL).origin;
  const state = { items: [], creates: 0, upserts: [] };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    if (url.origin !== origin) return route.abort('blockedbyclient');
    const path = url.pathname;
    const method = request.method();
    if (path === '/session') return route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    if (path === '/marketplace' && method === 'GET') return route.fulfill({ json: LISTINGS });
    if (path === '/marketplace/cart' && method === 'POST') {
      state.creates++;
      return route.fulfill({ json: buildCart(state) });
    }
    if (path === `/marketplace/cart/${CART_ID}` && method === 'GET') return route.fulfill({ json: buildCart(state) });
    if (path === `/marketplace/cart/${CART_ID}/items` && method === 'POST') {
      const body = request.postDataJSON();
      state.upserts.push(body);
      // The real API *sets* the quantity (0 removes), it never increments.
      state.items = state.items.filter((entry) => entry.listingId !== body.mciuListingId);
      if (body.mciuQuantity > 0) state.items.push({ listingId: body.mciuListingId, quantity: body.mciuQuantity });
      return route.fulfill({ json: buildCart(state) });
    }
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  return state;
}

async function expectHealthyLayout(page) {
  const layout = await page.evaluate(() => ({
    scrollWidth: document.documentElement.scrollWidth,
    innerWidth: window.innerWidth,
    rootText: (document.getElementById('root')?.innerText ?? '').trim().length,
  }));
  expect(layout.scrollWidth).toBeLessThanOrEqual(layout.innerWidth);
  expect(layout.rootText).toBeGreaterThan(0);
}

test('Marketplace cart stays reachable after adding on a small phone @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL);
  await page.goto('/marketplace', { waitUntil: 'domcontentloaded' });

  const header = page.locator('header');
  const cartButton = header.getByTestId('marketplace-cart-button');
  await expect(page.getByText('Micrófono Vintage SM58').first()).toBeVisible();
  // Empty cart: the icon is visible, the count badge is not.
  await expect(cartButton).toBeVisible();
  await expect(cartButton).toHaveAccessibleName('Carrito, sin productos');
  await expect(cartButton).toBeInViewport();
  await expectHealthyLayout(page);

  const addMic = page.getByRole('button', { name: 'Agregar: Micrófono Vintage SM58' });
  await addMic.click();

  // Confirmation without navigating away, with a direct way to the cart.
  const confirmation = page.getByRole('alert').filter({ hasText: 'Agregado al carrito' });
  await expect(confirmation).toBeVisible();
  await expect(confirmation.getByRole('button', { name: 'Ver carrito' })).toBeVisible();
  await expect(page).toHaveURL(/\/marketplace(?:\?.*)?$/);
  await expect(cartButton).toHaveAccessibleName('Carrito, 1 producto');
  await expect(header.locator('.MuiBadge-badge')).toHaveText('1');
  // Without scrolling back up, a cart entry point is on screen: below the md
  // breakpoint the sticky mobile bar (the header may have scrolled away while
  // reaching the product); on wider layouts the header cart control.
  if ((page.viewportSize()?.width ?? 0) < 900) {
    await expect(page.getByRole('button', { name: /^Ver carrito \(1\)/ })).toBeInViewport();
  } else {
    await expect(cartButton).toBeInViewport();
  }
  expect(state.upserts).toEqual([{ mciuListingId: 'listing-mic', mciuQuantity: 1 }]);
  expect(state.creates).toBe(1);
  await expectHealthyLayout(page);
  await page.screenshot({ path: testInfo.outputPath('marketplace-cart-after-add.png') });

  // The header control opens the cart drawer with the cart contents.
  await cartButton.click();
  const drawer = page.getByRole('dialog', { name: 'Carrito' });
  await expect(drawer).toBeVisible();
  await expect(drawer.getByText('Micrófono Vintage SM58')).toBeVisible();
  await expect(drawer.getByRole('button', { name: 'Ir al checkout' })).toBeVisible();
  await page.screenshot({ path: testInfo.outputPath('marketplace-cart-drawer.png') });
  await drawer.getByRole('button', { name: 'Cerrar carrito' }).click();
  await expect(drawer).toBeHidden();

  // Refresh keeps the count (restored from localStorage, then confirmed by the server cart).
  await page.reload({ waitUntil: 'domcontentloaded' });
  await expect(cartButton).toHaveAccessibleName('Carrito, 1 producto');
  await expect(header.locator('.MuiBadge-badge')).toHaveText('1');
  await expectHealthyLayout(page);

  // Removing the item hides the badge but keeps the cart icon.
  await cartButton.click();
  await expect(drawer).toBeVisible();
  await drawer.getByRole('button', { name: 'Quitar Micrófono Vintage SM58 del carrito' }).click();
  await expect(drawer.getByText('Tu carrito está vacío.')).toBeVisible();
  await expect(drawer.getByRole('button', { name: 'Explorar catálogo' })).toBeVisible();
  await drawer.getByRole('button', { name: 'Cerrar carrito' }).click();
  await expect(cartButton).toHaveAccessibleName('Carrito, sin productos');
  await expect(header.locator('.MuiBadge-badge.MuiBadge-invisible')).toHaveCount(1);
  await expect(cartButton).toBeVisible();
  expect(state.upserts.at(-1)).toEqual({ mciuListingId: 'listing-mic', mciuQuantity: 0 });
  await expectHealthyLayout(page);
});
