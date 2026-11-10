import { getDesktopShell, isDesktopShell } from './shell';

const api = {
  version: { app: '1.0.0', electron: '45.0.0', chromium: '156.0.0.0', platform: 'linux', arch: 'x64', channel: 'production', unsigned: false },
  playback: { report: () => undefined, onCommand: () => () => undefined },
  links: { openExternal: async () => true },
};

afterEach(() => { Reflect.deleteProperty(window, 'tdfDesktop'); });

it('is absent in an ordinary browser', () => {
  expect(getDesktopShell()).toBeNull();
  expect(isDesktopShell()).toBe(false);
});

it('exposes a well-formed shell bridge', () => {
  Object.defineProperty(window, 'tdfDesktop', { configurable: true, value: api });
  expect(getDesktopShell()).toBe(api);
  expect(isDesktopShell()).toBe(true);
});

it.each([
  ['a string', 'desktop'],
  ['a partial object', { version: { app: '1', channel: 'x' } }],
  ['a non-callable report', { ...api, playback: { report: true, onCommand: () => undefined } }],
])('ignores %s injected as the global', (_label, value) => {
  Object.defineProperty(window, 'tdfDesktop', { configurable: true, value });
  expect(getDesktopShell()).toBeNull();
});
