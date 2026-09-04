import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

let featureFlags: string[] = [];
let locale = 'es';
const priorityFlowMock = jest.fn((props: { locale: 'es' | 'en' }) => <div>priority-flow:{props.locale}</div>);

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: { featureFlags } }),
}));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale }),
}));
jest.unstable_mockModule('../components/reputation/CategoryPriorityPrototype', () => ({
  default: (props: { locale: 'es' | 'en' }) => priorityFlowMock(props),
}));

const { default: ReputationPreferencesPage } = await import('./ReputationPreferencesPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

async function renderPage() {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(<ReputationPreferencesPage />);
  });
  return {
    container,
    async cleanup() {
      await act(async () => root?.unmount());
      root = null;
      container.remove();
    },
  };
}

describe('ReputationPreferencesPage', () => {
  beforeEach(() => {
    featureFlags = [];
    locale = 'es';
    priorityFlowMock.mockClear();
  });

  it('does not mount the preference flow while the pilot flag is disabled', async () => {
    const view = await renderPage();
    try {
      expect(view.container.textContent).toContain('La reputación contextual todavía no está habilitada para esta cuenta.');
      expect(priorityFlowMock).not.toHaveBeenCalled();
    } finally {
      await view.cleanup();
    }
  });

  it('mounts the flow with the account locale when the pilot flag is enabled', async () => {
    featureFlags = ['CONTEXTUAL_REPUTATION_ENABLED'];
    locale = 'en';
    const view = await renderPage();
    try {
      expect(view.container.textContent).toContain('priority-flow:en');
      expect(priorityFlowMock).toHaveBeenCalledWith({ locale: 'en' });
    } finally {
      await view.cleanup();
    }
  });
});
