import { fireEvent, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import CommerceHubPage from './CommerceHubPage';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

describe('CommerceHubPage', () => {
  it('labels checkout, request, pilot, and unavailable offers truthfully', () => {
    render(<MemoryRouter><CommerceHubPage /></MemoryRouter>);

    expect(screen.getAllByText('Checkout disponible')).toHaveLength(2);
    expect(screen.getByText('Piloto privado')).toBeTruthy();
    expect(screen.getAllByText('No disponible').length).toBeGreaterThan(0);
    expect(screen.getByText(/Un pago verificado no significa entrega/)).toBeTruthy();
  });

  it('provides equivalent English status copy', () => {
    render(<MemoryRouter><CommerceHubPage /></MemoryRouter>);
    fireEvent.click(screen.getByRole('button', { name: 'EN' }));

    expect(screen.getByRole('heading', { name: 'TDF services and experiences' })).toBeTruthy();
    expect(screen.getByText('Private pilot')).toBeTruthy();
    expect(screen.getByText(/Verified payment never means delivered work/)).toBeTruthy();
  });

  it('has no serious automated accessibility violations', async () => {
    const { container } = render(<MemoryRouter><CommerceHubPage /></MemoryRouter>);
    await expectNoSeriousAccessibilityViolations(container);
  });
});
