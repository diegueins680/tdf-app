import http from 'node:http';

const configuredPort = Number.parseInt(process.env.TDF_MERCH_REPUTATION_FIXTURE_PORT ?? '8081', 10);
if (!Number.isInteger(configuredPort) || configuredPort < 1 || configuredPort > 65535) {
  throw new Error('TDF_MERCH_REPUTATION_FIXTURE_PORT must be an integer between 1 and 65535');
}

const host = '127.0.0.1';
const syntheticToken = 'synthetic-merch-reputation-buyer';
const orderId = '10000000-0000-4000-8000-000000000001';
const storeId = '20000000-0000-4000-8000-000000000001';
const storeReviewId = '30000000-0000-4000-8000-000000000001';
const lineId = '40000000-0000-4000-8000-000000000001';
const productId = '50000000-0000-4000-8000-000000000001';
const idempotentResponses = new Map();
let storeRevision = 0;

const json = (response, status, value) => {
  response.writeHead(status, {
    'Content-Type': 'application/json; charset=utf-8',
    'Cache-Control': 'no-store',
  });
  response.end(JSON.stringify(value));
};

const authorized = (request) => request.headers.authorization === `Bearer ${syntheticToken}`;
const editDeadline = () => new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString();

const eligibility = () => ({
  orderId,
  storeId,
  orderState: 'completed',
  fulfillmentState: 'delivered',
  storeReview: {
    eligible: true,
    state: storeRevision > 0 ? 'edit_available' : 'available',
    reviewId: storeRevision > 0 ? storeReviewId : null,
    currentRevision: storeRevision,
    deadline: editDeadline(),
  },
  productLines: [{
    lineId,
    productId,
    productName: 'Synthetic tour shirt',
    fulfillmentState: 'delivered',
    eligible: true,
    state: 'available',
    reviewId: null,
    currentRevision: 0,
  }],
});

const readJson = (request) => new Promise((resolve, reject) => {
  let raw = '';
  request.setEncoding('utf8');
  request.on('data', (chunk) => {
    raw += chunk;
    if (raw.length > 16_384) reject(new Error('body_too_large'));
  });
  request.on('end', () => {
    try {
      resolve(JSON.parse(raw));
    } catch {
      reject(new Error('invalid_json'));
    }
  });
  request.on('error', reject);
});

const validRating = (value) => Number.isInteger(value) && value >= 1 && value <= 5;
const validStoreReview = (body) => {
  if (!body || typeof body !== 'object' || Array.isArray(body)) return false;
  if (!validRating(body.overallRating) || body.expectedRevision !== storeRevision) return false;
  if (typeof body.issueOccurred !== 'boolean' || !body.dimensions || typeof body.dimensions !== 'object') return false;
  const required = ['preparation_dispatch', 'communication', 'packaging'];
  if (body.issueOccurred) required.push('problem_resolution');
  if (!required.every((dimension) => validRating(body.dimensions[dimension]))) return false;
  if (Object.keys(body.dimensions).some((dimension) => !required.includes(dimension))) return false;
  return body.comment === undefined
    || (typeof body.comment === 'string' && body.comment.length >= 10 && body.comment.length <= 3000);
};

const server = http.createServer(async (request, response) => {
  const url = new URL(request.url ?? '/', `http://${host}:${configuredPort}`);
  console.log(`fixture_request=${request.method} ${url.pathname}`);

  if (request.method === 'GET' && url.pathname === '/health') {
    return json(response, 200, { status: 'ok', version: 'synthetic-merch-reputation-mobile' });
  }
  if (!authorized(request)) return json(response, 401, { error: 'Synthetic bearer token required' });
  if (request.method === 'GET' && url.pathname === '/session') {
    return json(response, 200, {
      partyId: '7000001',
      username: 'synthetic-reputation-buyer',
      displayName: 'Synthetic Reputation Buyer',
      roles: [],
      modules: [],
      featureFlags: ['merch.store_reviews'],
    });
  }
  if (request.method === 'GET' && url.pathname === `/merch/orders/${orderId}/reviews/eligibility`) {
    return json(response, 200, eligibility());
  }
  if (request.method === 'PUT' && url.pathname === `/merch/orders/${orderId}/store-review`) {
    const key = request.headers['idempotency-key'];
    if (typeof key !== 'string' || key.length < 8 || key.length > 200) {
      return json(response, 400, { error: 'Valid Idempotency-Key required' });
    }
    if (idempotentResponses.has(key)) return json(response, 200, idempotentResponses.get(key));
    let body;
    try {
      body = await readJson(request);
    } catch {
      return json(response, 400, { error: 'Invalid synthetic review body' });
    }
    if (!validStoreReview(body)) return json(response, 422, { error: 'Invalid synthetic review body' });
    storeRevision += 1;
    const result = {
      reviewId: storeReviewId,
      revision: storeRevision,
      status: 'published',
      verifiedPurchase: true,
      editDeadline: editDeadline(),
    };
    idempotentResponses.set(key, result);
    return json(response, 200, result);
  }
  return json(response, 404, { error: 'Synthetic route not configured' });
});

server.listen(configuredPort, host, () => {
  console.log(`fixture_ready=http://${host}:${configuredPort}`);
  console.log(`fixture_order=${orderId}`);
});

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => server.close(() => process.exit(0)));
}
