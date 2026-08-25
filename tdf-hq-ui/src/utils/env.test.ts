import { jest } from '@jest/globals';
import { env, reportMissingEnv } from './env';
import { logger } from './logger';

type EnvTestWindow = Window & {
  __ENV__?: Record<string, string | undefined>;
  __MISSING_ENV__?: string[];
  __MISSING_ENV_KEYS__?: string[];
  __MISSING_ENV_REPORTED__?: string[];
};

describe('env utils', () => {
  const testWindow = window as EnvTestWindow;

  beforeEach(() => {
    delete testWindow.__ENV__;
    delete testWindow.__MISSING_ENV__;
    delete testWindow.__MISSING_ENV_KEYS__;
    delete testWindow.__MISSING_ENV_REPORTED__;
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it('reads and trims runtime env values from window.__ENV__', () => {
    testWindow.__ENV__ = { VITE_SAMPLE: '  enabled  ' };
    expect(env.read('VITE_SAMPLE')).toBe('enabled');
  });

  it('tracks missing vars across calls and reports only newly missing keys through the logger', () => {
    const loggerWarnSpy = jest.spyOn(logger, 'warn').mockImplementation(() => undefined);
    const consoleWarnSpy = jest.spyOn(console, 'warn').mockImplementation(() => undefined);

    reportMissingEnv(['VITE_A']);
    expect(testWindow.__MISSING_ENV__).toEqual(['VITE_A']);
    expect(loggerWarnSpy).toHaveBeenCalledTimes(1);
    expect(loggerWarnSpy).toHaveBeenLastCalledWith('Faltan variables de entorno críticas', { keys: ['VITE_A'] });
    expect(consoleWarnSpy).not.toHaveBeenCalled();

    reportMissingEnv(['VITE_A']);
    expect(loggerWarnSpy).toHaveBeenCalledTimes(1);
    expect(consoleWarnSpy).not.toHaveBeenCalled();

    testWindow.__ENV__ = { VITE_A: 'configured' };
    reportMissingEnv(['VITE_A', 'VITE_B']);
    expect(testWindow.__MISSING_ENV__).toEqual(['VITE_B']);
    expect(loggerWarnSpy).toHaveBeenCalledTimes(2);
    expect(loggerWarnSpy).toHaveBeenLastCalledWith('Faltan variables de entorno críticas', { keys: ['VITE_B'] });
    expect(consoleWarnSpy).not.toHaveBeenCalled();

    reportMissingEnv(['VITE_B']);
    expect(loggerWarnSpy).toHaveBeenCalledTimes(2);
    expect(consoleWarnSpy).not.toHaveBeenCalled();
  });
});
