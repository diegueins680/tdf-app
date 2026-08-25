import { jest } from '@jest/globals';

const buildAuthorizationHeaderMock = jest.fn<() => string | undefined>(() => undefined);

jest.unstable_mockModule('./authHeader', () => ({
  buildAuthorizationHeader: buildAuthorizationHeaderMock,
}));

jest.unstable_mockModule('../config/apiBase', () => ({
  resolveApiBase: () => '',
}));

const { submitFeedback } = await import('./feedback');

const successfulResponse = {
  ok: true,
  status: 200,
} as Response;

describe('feedback api', () => {
  const fetchMock = jest.fn<typeof fetch>();

  beforeEach(() => {
    fetchMock.mockReset();
    buildAuthorizationHeaderMock.mockReset();
    buildAuthorizationHeaderMock.mockReturnValue(undefined);
    (globalThis as unknown as { fetch: typeof fetch }).fetch = fetchMock;
  });

  it('allows anonymous submissions while including the session cookie when available', async () => {
    fetchMock.mockResolvedValueOnce(successfulResponse);

    await submitFeedback({
      title: 'No puedo enviar feedback',
      description: 'El formulario devuelve un error de autenticación.',
      categoryId: '11111111-1111-1111-1111-111111111111',
      severityId: '22222222-2222-2222-2222-222222222222',
      consent: true,
    });

    expect(fetchMock).toHaveBeenCalledTimes(1);
    const [url, request] = fetchMock.mock.calls[0] ?? [];
    expect(url).toBe('/feedback');
    expect(request).toMatchObject({
      method: 'POST',
      credentials: 'include',
      headers: undefined,
    });

    const form = request?.body as FormData;
    expect(form.get('title')).toBe('No puedo enviar feedback');
    expect(form.get('description')).toBe('El formulario devuelve un error de autenticación.');
    expect(form.get('categoryId')).toBe('11111111-1111-1111-1111-111111111111');
    expect(form.get('severityId')).toBe('22222222-2222-2222-2222-222222222222');
    expect(form.get('category')).toBeNull();
    expect(form.get('severity')).toBeNull();
    expect(form.get('consent')).toBe('true');
  });

  it('forwards bearer credentials so authenticated feedback can retain its author', async () => {
    buildAuthorizationHeaderMock.mockReturnValue('Bearer session-token');
    fetchMock.mockResolvedValueOnce(successfulResponse);

    await submitFeedback({
      title: 'Idea',
      description: 'Agregar filtros.',
      categoryId: '11111111-1111-1111-1111-111111111111',
      severityId: '22222222-2222-2222-2222-222222222222',
      consent: true,
    });

    expect(fetchMock).toHaveBeenCalledWith(
      '/feedback',
      expect.objectContaining({
        headers: { Authorization: 'Bearer session-token' },
        credentials: 'include',
      }),
    );
  });
});
