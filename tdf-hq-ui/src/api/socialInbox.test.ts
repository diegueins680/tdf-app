import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
}));

const { SocialInboxAPI, parseReplySuggestion } = await import('./socialInbox');

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

  it('builds message list query strings without dropping false or zero filter values', async () => {
    getMock.mockResolvedValueOnce([]);

    await SocialInboxAPI.listInstagramMessages({ limit: 0, direction: 'all', repliedOnly: false });

    expect(getMock).toHaveBeenCalledWith('/instagram/messages?limit=0&direction=all&repliedOnly=false');
  });

  it('parses SEND/HOLD model replies before the inbox uses them', () => {
    expect(parseReplySuggestion('SEND: Hola Diego')).toEqual({
      kind: 'send',
      text: 'Hola Diego',
    });
    expect(parseReplySuggestion('HOLD: Falta contexto\nNEED: Tema del anuncio')).toEqual({
      kind: 'hold',
      reason: 'Falta contexto',
      neededInfo: 'Tema del anuncio',
      raw: 'HOLD: Falta contexto\nNEED: Tema del anuncio',
    });
    expect(parseReplySuggestion('Gracias por escribirnos.')).toEqual({
      kind: 'send',
      text: 'Gracias por escribirnos.',
    });
  });

  it('falls back to the HOLD reason when the model omits a NEED section', () => {
    expect(parseReplySuggestion('  HOLD:   Falta contexto del anuncio.  ')).toEqual({
      kind: 'hold',
      reason: 'Falta contexto del anuncio.',
      neededInfo: 'Falta contexto del anuncio.',
      raw: 'HOLD:   Falta contexto del anuncio.',
    });
  });

  it('serializes operator WhatsApp questions and surfaces provider failures', async () => {
    postMock.mockResolvedValueOnce({ status: 'ok' });

    await SocialInboxAPI.askOperatorQuestion({
      channel: 'instagram',
      senderId: ' sender-1 ',
      externalId: ' msg-1 ',
      inboundMessage: ' ¿Puedes contarme algo más? ',
      holdReason: ' Falta contexto ',
      neededInfo: ' Tema del anuncio ',
    });

    expect(postMock).toHaveBeenCalledWith('/whatsapp/operator-question', {
      channel: 'instagram',
      senderId: 'sender-1',
      externalId: 'msg-1',
      inboundMessage: '¿Puedes contarme algo más?',
      holdReason: 'Falta contexto',
      neededInfo: 'Tema del anuncio',
    });

    postMock.mockResolvedValueOnce({ status: 'error', message: 'WhatsApp no configurado.' });
    await expect(
      SocialInboxAPI.askOperatorQuestion({
        channel: 'instagram',
        senderId: 'sender-1',
        inboundMessage: 'Hola',
        holdReason: 'Falta contexto',
        neededInfo: 'Tema del anuncio',
      }),
    ).rejects.toThrow('WhatsApp no configurado.');
  });

  it('normalizes suggest-reply messages and hints before posting to Ads Assist', async () => {
    postMock.mockResolvedValueOnce({ aasReply: 'SEND: Listo.' });

    await expect(
      SocialInboxAPI.suggestReply('instagram', '  Hola  ', '  Usa tono breve  '),
    ).resolves.toEqual({
      kind: 'send',
      text: 'Listo.',
    });

    expect(postMock).toHaveBeenCalledWith('/ads/assist', {
      aarMessage: 'Hola\n\nInstrucciones adicionales:\nUsa tono breve',
      aarChannel: 'instagram',
    });
  });
});
