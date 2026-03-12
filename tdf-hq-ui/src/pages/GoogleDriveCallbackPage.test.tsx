import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

const navigateMock = jest.fn();
const useGoogleDriveCallbackMock = jest.fn<
  () => { ok: boolean; message?: string; returnTo?: string }
>(() => ({ ok: false }));

jest.unstable_mockModule('react-router-dom', () => ({
  useNavigate: () => navigateMock,
}));

jest.unstable_mockModule('../hooks/useGoogleDriveAuth', () => ({
  useGoogleDriveCallback: () => useGoogleDriveCallbackMock(),
}));

const { default: GoogleDriveCallbackPage } = await import('./GoogleDriveCallbackPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderPage = async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(<GoogleDriveCallbackPage />);
    await flushPromises();
  });

  return {
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      document.body.removeChild(container);
    },
  };
};

describe('GoogleDriveCallbackPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    navigateMock.mockReset();
    useGoogleDriveCallbackMock.mockReset().mockReturnValue({ ok: false });
  });

  it('navigates to the callback return path without decoding it again', async () => {
    useGoogleDriveCallbackMock.mockReturnValue({ ok: true, returnTo: '/exports/100%ready' });

    const { cleanup } = await renderPage();

    expect(navigateMock).toHaveBeenCalledWith('/exports/100%ready', { replace: true });

    await cleanup();
  });
});
