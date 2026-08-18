import { fireEvent, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import DistributionLandingPage from './DistributionLandingPage';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

describe('DistributionLandingPage', () => {
  it('does not invent a price or claim that delivery is enabled', () => {
    render(<MemoryRouter><DistributionLandingPage /></MemoryRouter>);

    expect(screen.getByText(/No hay una tarifa pública aprobada/)).toBeTruthy();
    expect(screen.getByText(/Total no calculado/)).toBeTruthy();
    expect(screen.getByText(/Un archivo XML o una respuesta simulada nunca significa distribución completada/)).toBeTruthy();
    expect(document.body.textContent).not.toMatch(/\$\s?\d/);
  });

  it('keeps the pricing and production gates explicit in English', () => {
    render(<MemoryRouter><DistributionLandingPage /></MemoryRouter>);
    fireEvent.click(screen.getByRole('button', { name: 'EN' }));

    expect(screen.getByText(/There is no approved public rate/)).toBeTruthy();
    expect(screen.getByText(/simulated response never means completed distribution/)).toBeTruthy();
  });

  it('has no serious automated accessibility violations', async () => {
    const { container } = render(<MemoryRouter><DistributionLandingPage /></MemoryRouter>);
    await expectNoSeriousAccessibilityViolations(container);
  });
});
