import http from 'node:http';

const configuredPort = Number.parseInt(process.env.TDF_MERCH_FIXTURE_PORT ?? '8080', 10);
if (!Number.isInteger(configuredPort) || configuredPort < 1 || configuredPort > 65535) {
  throw new Error('TDF_MERCH_FIXTURE_PORT must be an integer between 1 and 65535');
}

const host = '127.0.0.1';
const storeId = '10000000-0000-4000-8000-000000000001';
const productId = '20000000-0000-4000-8000-000000000001';
const variantId = '30000000-0000-4000-8000-000000000001';

const capabilities = {
  environment: 'development',
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
  variants: [
    {
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
    },
  ],
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
};

const json = (response, status, value) => {
  response.writeHead(status, {
    'Content-Type': 'application/json; charset=utf-8',
    'Cache-Control': 'no-store',
  });
  response.end(JSON.stringify(value));
};

const server = http.createServer((request, response) => {
  const url = new URL(request.url ?? '/', `http://${host}:${configuredPort}`);
  console.log(`fixture_request=${request.method} ${url.pathname}${url.search}`);

  if (request.method !== 'GET') {
    return json(response, 405, { error: 'Synthetic fixture is read-only' });
  }
  if (url.pathname === '/health') {
    return json(response, 200, { status: 'ok', version: 'synthetic-mobile-merch' });
  }
  if (url.pathname === '/session') return json(response, 401, { error: 'unauthenticated' });
  if (url.pathname === '/fans/artists') return json(response, 200, []);
  if (url.pathname === '/merch/capabilities') return json(response, 200, capabilities);
  if (url.pathname === '/merch/storefronts') return json(response, 200, [storefront]);
  if (url.pathname === '/merch/storefronts/cementerio-sintetico') {
    return json(response, 200, storefront);
  }
  if (url.pathname === '/merch/storefronts/cementerio-sintetico/products/camiseta-camino') {
    return json(response, 200, product);
  }
  return json(response, 404, { error: 'Synthetic route not configured' });
});

server.listen(configuredPort, host, () => {
  console.log(`fixture_ready=http://${host}:${configuredPort}`);
});

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => server.close(() => process.exit(0)));
}
