import { createElement } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { expectNoSeriousAccessibilityViolations } from '../../test/accessibility';
import i18n from '../../i18n';
import ContextualRankingPrototype, { type RankingPerson } from './ContextualRankingPrototype';

const people: RankingPerson[] = [
  { id: 'maya', name: 'Maya Torres', professionalName: 'Maya T', role: 'Fotógrafa', city: 'Quito', interaction: 'Sesión completada', verified: true },
  { id: 'leo', name: 'Leo Vega', role: 'Músico', interaction: 'Colaboración confirmada', verified: false },
  { id: 'noa', name: 'Noa Ruiz', role: 'Productora', city: 'Guayaquil', interaction: 'Reserva completada', verified: true },
];

describe('ContextualRankingPrototype', () => {
  beforeEach(async () => {
    await i18n.changeLanguage('es');
  });

  it('offers keyboard alternatives, announces exclusions, and has no serious accessibility violations', async () => {
    const view = render(createElement(ContextualRankingPrototype, {
      category: 'Comunicación',
      people,
    }));

    try {
      expect(screen.getByRole('list', { name: 'Ranking de Comunicación' })).toBeTruthy();
      expect(screen.getByRole('button', { name: 'Subir a Maya Torres' })).toBeTruthy();
      expect(screen.getByRole('button', { name: 'Bajar a Leo Vega' })).toBeTruthy();

      const [firstExclusion] = screen.getAllByRole('button', { name: 'No tengo suficiente información', exact: true });
      if (!firstExclusion) throw new Error('Expected an exclusion control for each comparable person');
      fireEvent.click(firstExclusion);
      expect(screen.getByRole('button', { name: 'Incluir' })).toBeTruthy();
      expect(screen.getByRole('status').textContent).toContain('se excluyó por falta de información');

      await expectNoSeriousAccessibilityViolations(view.container);
    } finally {
      view.unmount();
    }
  });

  it('renders its accessible alternatives in English', async () => {
    await i18n.changeLanguage('en');
    const view = render(createElement(ContextualRankingPrototype, {
      category: 'Communication',
      people,
    }));

    try {
      expect(screen.getByRole('list', { name: 'Ranking for Communication' })).toBeTruthy();
      expect(screen.getByRole('button', { name: 'Move Maya Torres up' })).toBeTruthy();
      expect(screen.getAllByRole('button', { name: 'I do not have enough information' })).toHaveLength(3);
    } finally {
      view.unmount();
      await i18n.changeLanguage('es');
    }
  });
});
