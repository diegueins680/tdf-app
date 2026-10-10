import '@testing-library/jest-dom';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';

import { LegalDisclosure } from './LegalDisclosure';

afterEach(cleanup);

describe('LegalDisclosure', () => {
  it('starts collapsed and links the button to its panel', () => {
    render(
      <LegalDisclosure title="Política de reembolso" summary="Versión v1">
        <p>Texto completo</p>
      </LegalDisclosure>,
    );

    const button = screen.getByRole('button', { name: /Política de reembolso/ });
    expect(button).toHaveAttribute('aria-expanded', 'false');
    expect(button).toHaveTextContent('Leer completo');
    expect(screen.getByText('Versión v1')).toBeInTheDocument();

    const panelId = button.getAttribute('aria-controls');
    expect(panelId).toBeTruthy();
    const panel = document.getElementById(panelId!);
    expect(panel).toHaveAttribute('role', 'region');
    expect(panel).toHaveAttribute('aria-labelledby', button.id);
    expect(panel).toHaveTextContent('Texto completo');
  });

  it('toggles from the whole header row and updates the label', () => {
    render(
      <LegalDisclosure title="Terms" language="en">
        <p>Full text</p>
      </LegalDisclosure>,
    );

    const button = screen.getByRole('button', { name: /Terms/ });
    fireEvent.click(button);
    expect(button).toHaveAttribute('aria-expanded', 'true');
    expect(button).toHaveTextContent('Hide');
    expect(screen.getByRole('region', { name: /Terms/ })).toHaveTextContent('Full text');

    fireEvent.click(button);
    expect(button).toHaveAttribute('aria-expanded', 'false');
    expect(button).toHaveTextContent('Read in full');
  });

  it('is a native button, so Enter and Space work from the keyboard', () => {
    render(<LegalDisclosure title="Términos">texto</LegalDisclosure>);
    const button = screen.getByRole('button', { name: /Términos/ });
    expect(button.tagName).toBe('BUTTON');
    expect(button).not.toHaveAttribute('tabindex', '-1');
  });

  it('keeps independent state for sibling documents', () => {
    render(
      <>
        <LegalDisclosure title="Términos">a</LegalDisclosure>
        <LegalDisclosure title="Privacidad">b</LegalDisclosure>
      </>,
    );
    fireEvent.click(screen.getByRole('button', { name: /Términos/ }));
    expect(screen.getByRole('button', { name: /Términos/ })).toHaveAttribute('aria-expanded', 'true');
    expect(screen.getByRole('button', { name: /Privacidad/ })).toHaveAttribute('aria-expanded', 'false');
  });

  it('wraps the control in a heading when a level is given', () => {
    render(<LegalDisclosure title="Términos" headingLevel={3} defaultExpanded>texto</LegalDisclosure>);
    expect(screen.getByRole('heading', { level: 3, name: /Términos/ })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Términos/ })).toHaveAttribute('aria-expanded', 'true');
  });

  it('opens when a deep link arrives while mounted, without collapsing a reader-opened document', () => {
    const { rerender } = render(<LegalDisclosure title="Privacidad" defaultExpanded={false}>texto</LegalDisclosure>);
    const button = () => screen.getByRole('button', { name: /Privacidad/ });
    expect(button()).toHaveAttribute('aria-expanded', 'false');

    rerender(<LegalDisclosure title="Privacidad" defaultExpanded>texto</LegalDisclosure>);
    expect(button()).toHaveAttribute('aria-expanded', 'true');

    rerender(<LegalDisclosure title="Privacidad" defaultExpanded={false}>texto</LegalDisclosure>);
    expect(button()).toHaveAttribute('aria-expanded', 'true');
  });
});
