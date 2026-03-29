import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';

const { default: EnrollmentSuccessDialog } = await import('../components/EnrollmentSuccessDialog');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

describe('EnrollmentSuccessDialog', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('renders the default message when open', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root: Root = createRoot(container);

    await act(async () => {
      root.render(<EnrollmentSuccessDialog open={true} onClose={() => {}} />);
      await flushPromises();
    });

    expect(document.body.textContent).toContain('¡Inscripción confirmada!');
    expect(document.body.textContent).toContain(
      'Felicitaciones, tu inscripción fue recibida exitosamente. Bienvenido a TDF Records!',
    );

    await act(async () => { root.unmount(); });
    document.body.removeChild(container);
  });

  it('renders a custom message when the message prop is provided', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root: Root = createRoot(container);

    await act(async () => {
      root.render(
        <EnrollmentSuccessDialog open={true} onClose={() => {}} message="Mensaje personalizado" />,
      );
      await flushPromises();
    });

    expect(document.body.textContent).toContain('Mensaje personalizado');
    expect(document.body.textContent).not.toContain('Bienvenido a TDF Records');

    await act(async () => { root.unmount(); });
    document.body.removeChild(container);
  });

  it('does not render dialog content when closed', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root: Root = createRoot(container);

    await act(async () => {
      root.render(<EnrollmentSuccessDialog open={false} onClose={() => {}} />);
      await flushPromises();
    });

    expect(document.body.textContent).not.toContain('¡Inscripción confirmada!');

    await act(async () => { root.unmount(); });
    document.body.removeChild(container);
  });

  it('calls onClose when the Entendido button is clicked', async () => {
    const onClose = jest.fn();
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root: Root = createRoot(container);

    await act(async () => {
      root.render(<EnrollmentSuccessDialog open={true} onClose={onClose} />);
      await flushPromises();
    });

    const button = Array.from(document.body.querySelectorAll('button')).find(
      (btn) => btn.textContent?.trim() === 'Entendido',
    );
    expect(button).toBeTruthy();

    await act(async () => {
      button?.click();
      await flushPromises();
    });

    expect(onClose).toHaveBeenCalledTimes(1);

    await act(async () => { root.unmount(); });
    document.body.removeChild(container);
  });
});
