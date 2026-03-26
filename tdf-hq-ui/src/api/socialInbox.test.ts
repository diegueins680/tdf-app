import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
}));

const { SocialInboxAPI } = await import('./socialInbox');

describe('SocialInboxAPI.sendReply', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
  });

  it.each([
    [
      'instagram',
      '/instagram/reply',
      { irSenderId: 'sender-1', irMessage: 'Hola', irExternalId: 'ext-1' },
    ],
    [
      'facebook',
      '/facebook/reply',
      { frSenderId: 'sender-1', frMessage: 'Hola', frExternalId: 'ext-1' },
    ],
    [
      'whatsapp',
      '/whatsapp/reply',
      { wrSenderId: 'sender-1', wrMessage: 'Hola', wrExternalId: 'ext-1' },
    ],
  ] as const)('serializes %s replies with the correct endpoint and payload shape', async (channel, path, payload) => {
    postMock.mockResolvedValueOnce({ status: 'ok' });

    await SocialInboxAPI.sendReply(channel, {
      senderId: '  sender-1  ',
      message: '  Hola  ',
      externalId: '  ext-1  ',
    });

    expect(postMock).toHaveBeenCalledWith(path, payload);
  });

  it('rejects blank sender ids and blank messages before posting', async () => {
    await expect(SocialInboxAPI.sendReply('instagram', { senderId: '   ', message: 'Hola' })).rejects.toThrow(
      'Remitente inválido.',
    );
    await expect(SocialInboxAPI.sendReply('instagram', { senderId: 'sender-1', message: '   ' })).rejects.toThrow(
      'Escribe una respuesta antes de enviar.',
    );

    expect(postMock).not.toHaveBeenCalled();
  });

  it('surfaces API error messages from reply attempts', async () => {
    postMock.mockResolvedValueOnce({ status: 'error', message: 'No autorizado.' });

    await expect(
      SocialInboxAPI.sendReply('facebook', { senderId: 'sender-1', message: 'Hola' }),
    ).rejects.toThrow('No autorizado.');
  });
});
