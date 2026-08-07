import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import { evaluateFeatureAccess } from '../features/featureRegistry';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';
import ForbiddenPage from './ForbiddenPage';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('ForbiddenPage', () => {
  it('explains a safe locked state without protected data and passes serious axe rules', async () => {
    const decision = evaluateFeatureAccess('label.ddex.inbox', {
      authenticated: true,
      roles: ['Fan', 'Customer'],
      modules: ['Packages'],
    }, 'view');
    expect(decision.state).toBe('locked');
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    try {
      await act(async () => {
        root.render(<MemoryRouter><ForbiddenPage decision={decision} /></MemoryRouter>);
      });
      expect(container.textContent).toContain('No se mostró ningún dato protegido');
      expect(document.activeElement?.textContent).toContain('No tienes permiso');
      await expectNoSeriousAccessibilityViolations(container);
    } finally {
      await act(async () => root.unmount());
      container.remove();
    }
  });
});
