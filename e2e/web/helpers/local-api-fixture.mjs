// Keep API/foreign-origin isolation without sending each local Vite module through
// the test runner's route callback. A URL predicate would intercept all requests;
// a RegExp is serialized to Playwright's driver for filtering there.
export function localApiFixturePattern(baseURL) {
  const base = new URL(baseURL);
  if (base.protocol !== 'http:' || !['127.0.0.1', 'localhost', '[::1]'].includes(base.hostname)
    || base.username || base.password || base.pathname !== '/' || base.search || base.hash) {
    throw new Error('Browser API fixtures require an explicit HTTP loopback origin.');
  }
  const escapedOrigin = base.origin.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  return new RegExp(`^(?!${escapedOrigin}/(?:src|node_modules)/)`);
}
