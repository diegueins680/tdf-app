import { jest } from '@jest/globals';
import { render, screen } from '@testing-library/react';

const createPublicMock = jest.fn();
jest.unstable_mockModule('../api/bookings', () => ({
  Bookings: { createPublic: createPublicMock },
}));

const { default: DomoVenuePage } = await import('./DomoVenuePage');

describe('DomoVenuePage pricing truthfulness', () => {
  it('does not calculate client-authoritative prices, taxes, or deposits', () => {
    render(<DomoVenuePage />);

    expect(screen.getByRole('heading', { name: 'Resumen de solicitud' })).toBeTruthy();
    expect(screen.getByText(/El precio, impuestos, depósito y políticas vendrán en una cotización versionada/)).toBeTruthy();
    expect(document.body.textContent).not.toContain('Cotización estimada');
    expect(document.body.textContent).not.toContain('IVA 12%');
    expect(document.body.textContent).not.toMatch(/\$\s?\d/);
  });
});
