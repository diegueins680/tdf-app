import { jest } from '@jest/globals';
import { fireEvent, render, screen } from '@testing-library/react';
import MerchReviewForm from './MerchReviewForm';

describe('MerchReviewForm', () => {
  it('does not require a cross-store comparison and separates product dimensions', () => {
    render(<MerchReviewForm kind="product" expectedRevision={0} onSubmit={jest.fn()} />);

    expect(screen.getByText('Evalúa solo esta compra. No necesitas comparar tiendas.')).toBeTruthy();
    expect(screen.getByText('Producto conforme a la descripción')).toBeTruthy();
    expect(screen.getByText('Calidad del producto')).toBeTruthy();
    expect(screen.queryByText('Velocidad de preparación y despacho')).toBeNull();
  });

  it('requires ratings instead of preselecting a positive value', () => {
    render(<MerchReviewForm kind="store" expectedRevision={0} onSubmit={jest.fn()} />);
    fireEvent.click(screen.getByRole('button', { name: 'Enviar evaluación' }));
    expect(screen.getByText('Completa todas las categorías requeridas.')).toBeTruthy();
  });
});
