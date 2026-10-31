import { jest } from '@jest/globals';
import { ensureTorrentWorker } from './torrentWorker';

const scriptURL = new URL('/webtorrent-sw.min.js', window.location.href).href;
const rootScope = new URL('/', window.location.href).href;
class Worker extends EventTarget {
  postMessage = jest.fn();
  scriptURL = scriptURL;
  state: ServiceWorkerState = 'activated';
  listeners = new Set<EventListenerOrEventListenerObject>();
  override addEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.add(callback);
    super.addEventListener(type, callback);
  }
  override removeEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.delete(callback);
    super.removeEventListener(type, callback);
  }
}
class Registration extends EventTarget {
  active: Worker | null;
  installing: Worker | null = null;
  waiting: Worker | null = null;
  scope = rootScope;
  unregister = jest.fn();
  listeners = new Set<EventListenerOrEventListenerObject>();
  constructor(active: Worker | null) { super(); this.active = active; }
  override addEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.add(callback);
    super.addEventListener(type, callback);
  }
  override removeEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.delete(callback);
    super.removeEventListener(type, callback);
  }
}
function registration(active: Worker | null = new Worker()) { return new Registration(active); }
class Container extends EventTarget {
  controller: Worker | null = null;
  getRegistration = jest.fn<() => Promise<ReturnType<typeof registration> | undefined>>();
  register = jest.fn<() => Promise<ReturnType<typeof registration>>>();
  listeners = new Set<EventListenerOrEventListenerObject>();
  override addEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.add(callback);
    super.addEventListener(type, callback);
  }
  override removeEventListener(type: string, callback: EventListenerOrEventListenerObject | null) {
    if (callback) this.listeners.delete(callback);
    super.removeEventListener(type, callback);
  }
}
let container: Container;
beforeEach(() => {
  jest.useFakeTimers();
  container = new Container();
  container.getRegistration.mockResolvedValue(undefined);
  Object.defineProperty(navigator, 'serviceWorker', { configurable: true, value: container });
});
afterEach(() => {
  jest.useRealTimers();
  Reflect.deleteProperty(navigator, 'serviceWorker');
});

it('requires service worker support', async () => {
  Reflect.deleteProperty(navigator, 'serviceWorker');
  await expect(ensureTorrentWorker()).rejects.toThrow();
  expect(container.register).not.toHaveBeenCalled();
});

it('waits for activation and exact page control before resolving a root registration', async () => {
  const worker = new Worker(); worker.state = 'installing';
  const reg = registration(null); reg.installing = worker;
  container.register.mockResolvedValue(reg);
  let resolved = false;
  const pending = ensureTorrentWorker().then(value => { resolved = true; return value; });
  await jest.advanceTimersByTimeAsync(0);
  expect(container.getRegistration).toHaveBeenCalledWith('/');
  expect(container.register).toHaveBeenCalledWith('/webtorrent-sw.min.js', { scope: '/' });
  expect(resolved).toBe(false);
  worker.state = 'activated'; reg.active = worker; reg.installing = null;
  worker.dispatchEvent(new Event('statechange'));
  await jest.advanceTimersByTimeAsync(0);
  expect(resolved).toBe(false);
  container.controller = worker;
  container.dispatchEvent(new Event('controllerchange'));
  await expect(pending).resolves.toBe(reg);
  expect(worker.listeners.size).toBe(0);
  expect(container.listeners.size).toBe(0);
  expect(jest.getTimerCount()).toBe(0);
});

it('reuses an owned controlled root registration and rechecks control on every call', async () => {
  const reg = registration(); container.controller = reg.active;
  container.getRegistration.mockResolvedValue(reg);
  await expect(ensureTorrentWorker()).resolves.toBe(reg);
  await expect(ensureTorrentWorker()).resolves.toBe(reg);
  expect(container.register).not.toHaveBeenCalled();
  container.controller = new Worker(); container.controller.scriptURL = new URL('/foreign.js', rootScope).href;
  await expect(ensureTorrentWorker()).rejects.toThrow();
  expect(container.register).not.toHaveBeenCalled();
  expect(reg.unregister).not.toHaveBeenCalled();
});

it('does not accept a different worker merely because its script URL matches', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  container.controller = new Worker();
  let resolved = false;
  const pending = ensureTorrentWorker().then(value => { resolved = true; return value; });
  await jest.advanceTimersByTimeAsync(0);
  expect(resolved).toBe(false);
  container.controller = reg.active; container.dispatchEvent(new Event('controllerchange'));
  await expect(pending).resolves.toBe(reg);
});

it.each(['active', 'installing', 'waiting'] as const)('preserves a foreign root %s worker', async phase => {
  const reg = registration(null); const foreign = new Worker();
  foreign.scriptURL = new URL('/foreign.js', rootScope).href; reg[phase] = foreign;
  container.getRegistration.mockResolvedValue(reg);
  await expect(ensureTorrentWorker()).rejects.toThrow();
  expect(container.register).not.toHaveBeenCalled();
  expect(reg.unregister).not.toHaveBeenCalled();
});

it('does not register over a foreign current controller', async () => {
  container.controller = new Worker(); container.controller.scriptURL = new URL('/foreign.js', rootScope).href;
  await expect(ensureTorrentWorker()).rejects.toThrow();
  expect(container.register).not.toHaveBeenCalled();
});

it('rejects activation failure and removes its listener and timer', async () => {
  const worker = new Worker(); worker.state = 'installing';
  const reg = registration(null); reg.installing = worker; container.register.mockResolvedValue(reg);
  const pending = expect(ensureTorrentWorker()).rejects.toThrow();
  await jest.advanceTimersByTimeAsync(0);
  worker.state = 'redundant'; worker.dispatchEvent(new Event('statechange'));
  await pending;
  expect(worker.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
});

it('bounds activation wait and releases its listener and timer', async () => {
  const worker = new Worker(); worker.state = 'installing';
  const reg = registration(null); reg.installing = worker; container.register.mockResolvedValue(reg);
  const pending = expect(ensureTorrentWorker()).rejects.toThrow();
  await jest.advanceTimersByTimeAsync(15_000);
  await pending;
  expect(worker.listeners.size).toBe(0); expect(container.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
});

it('bounds missing control, cleans up, and permits a later valid attempt', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  const pending = expect(ensureTorrentWorker()).rejects.toThrow();
  await jest.advanceTimersByTimeAsync(15_000); await pending;
  expect(container.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
  container.controller = reg.active;
  await expect(ensureTorrentWorker()).resolves.toBe(reg);
});

it('rejects foreign takeover while waiting without unregistering anything', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  const pending = expect(ensureTorrentWorker()).rejects.toThrow();
  await jest.advanceTimersByTimeAsync(0);
  container.controller = new Worker(); container.controller.scriptURL = new URL('/foreign.js', rootScope).href;
  container.dispatchEvent(new Event('controllerchange')); await pending;
  expect(container.listeners.size).toBe(0); expect(reg.unregister).not.toHaveBeenCalled();
});


it('rechecks foreign takeover after the asynchronous registration lookup', async () => {
  container.getRegistration.mockImplementation(async () => {
    container.controller = new Worker(); container.controller.scriptURL = new URL('/foreign.js', rootScope).href;
    return undefined;
  });
  await expect(ensureTorrentWorker()).rejects.toThrow();
  expect(container.register).not.toHaveBeenCalled();
});


it('observes a replacement controller that is still activating at controllerchange', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  let resolved = false;
  const pending = ensureTorrentWorker().then(value => { resolved = true; return value; });
  await jest.advanceTimersByTimeAsync(0);
  const replacement = new Worker(); replacement.state = 'activating';
  reg.active = replacement; container.controller = replacement;
  container.dispatchEvent(new Event('controllerchange'));
  await jest.advanceTimersByTimeAsync(0); expect(resolved).toBe(false);
  replacement.state = 'activated'; replacement.dispatchEvent(new Event('statechange'));
  await jest.advanceTimersByTimeAsync(0); expect(resolved).toBe(true);
  await expect(pending).resolves.toBe(reg);
  expect(replacement.listeners.size).toBe(0); expect(container.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
});


it('observes newly found foreign workers and cleans all registration listeners', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  const pending = expect(ensureTorrentWorker()).rejects.toThrow();
  await jest.advanceTimersByTimeAsync(0);
  reg.installing = new Worker(); reg.installing.scriptURL = new URL('/foreign.js', rootScope).href;
  reg.dispatchEvent(new Event('updatefound')); await pending;
  expect(reg.listeners.size).toBe(0); expect(container.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
});


it('requests a claim once from its own activated worker while awaiting page control', async () => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  const pending = ensureTorrentWorker();
  await jest.advanceTimersByTimeAsync(0);
  expect(reg.active?.postMessage).toHaveBeenCalledWith({ type: 'tdf-torrent-claim' });
  container.dispatchEvent(new Event('controllerchange'));
  expect(reg.active?.postMessage).toHaveBeenCalledTimes(1);
  container.controller = reg.active; container.dispatchEvent(new Event('controllerchange'));
  await expect(pending).resolves.toBe(reg);
});

it.each([new Error('claim refused'), 'claim refused'])('releases observers when the owned claim message throws: %s', async failure => {
  const reg = registration(); container.getRegistration.mockResolvedValue(reg);
  reg.active?.postMessage.mockImplementation(() => {
    // eslint-disable-next-line @typescript-eslint/only-throw-error -- Deliberate non-Error fault verifies browser boundary normalization.
    throw failure;
  });
  await expect(ensureTorrentWorker()).rejects.toThrow('claim refused');
  expect(reg.listeners.size).toBe(0); expect(reg.active?.listeners.size).toBe(0);
  expect(container.listeners.size).toBe(0); expect(jest.getTimerCount()).toBe(0);
});
