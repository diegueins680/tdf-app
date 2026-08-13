import { jest } from '@jest/globals';
import { act } from 'react';
import { fireEvent, render } from '@testing-library/react';
import i18n from '../i18n/index';

import ReactionBar from './ReactionBar';
import type { ReactionSummaryDTO } from '../api/types';

const FIRE_ID = '50900000-0000-4000-8000-000000000001';
const MIC_DROP_ID = '50900000-0000-4000-8000-000000000004';

const summary: ReactionSummaryDTO = {
  rsItems: [
    {
      rsiReactionTypeId: FIRE_ID,
      rsiCode: 'fire',
      rsiNameEs: 'Fuego',
      rsiNameEn: 'Fire',
      rsiDisplaySymbol: '🔥',
      rsiCount: 3,
    },
    {
      rsiReactionTypeId: MIC_DROP_ID,
      rsiCode: 'mic_drop',
      rsiNameEs: 'Micrófono al suelo',
      rsiNameEn: 'Mic drop',
      rsiDisplaySymbol: '🎤',
      rsiCount: 0,
    },
  ],
  rsTotal: 3,
  rsMyReactionTypeId: FIRE_ID,
};

describe('ReactionBar canonical catalog consumption', () => {
  beforeAll(async () => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    await i18n.changeLanguage('es');
  });

  it('renders only server-provided catalog options and submits their stable UUID', async () => {
    const onReact = jest.fn<void, [string]>();
    const view = render(<ReactionBar reactions={summary} onReact={onReact} />);

    const fireButton = view.getByRole('button', { name: 'Fuego (3)' });
    const micDropButton = view.getByRole('button', { name: 'Micrófono al suelo' });
    expect(fireButton.getAttribute('aria-pressed')).toBe('true');
    expect(micDropButton.getAttribute('aria-pressed')).toBe('false');
    expect(view.queryByRole('button', { name: /calavera/i })).toBeNull();

    await act(async () => {
      fireEvent.click(micDropButton);
    });
    expect(onReact).toHaveBeenCalledWith(MIC_DROP_ID);
  });

  it('announces an empty persisted catalog without inventing emergency options', () => {
    const view = render(
      <ReactionBar
        reactions={{ rsItems: [], rsTotal: 0, rsMyReactionTypeId: null }}
        onReact={() => undefined}
      />,
    );
    expect(view.getByRole('status').textContent).toContain('Sin reacciones disponibles');
    expect(view.queryAllByRole('button')).toHaveLength(0);
  });
});
