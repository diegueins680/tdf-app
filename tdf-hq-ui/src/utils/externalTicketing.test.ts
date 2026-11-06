import { findExternalTicketLink, localizeTicketUnavailableReason } from './externalTicketing';

const VERDE_70 = 'Verde 70 presenta Fulgorama Tour 2026 en Miami.\n\nAnuncio oficial de la banda: https://www.instagram.com/verde70oficial/p/DeIVxihiVX6/\nEntradas y detalles: https://www.eventbrite.com/e/verde-70-en-miami-tickets-2001152275325';

describe('findExternalTicketLink', () => {
  it('prefers a known ticketing vendor over other links in the description', () => {
    expect(findExternalTicketLink(VERDE_70)).toEqual({
      url: 'https://www.eventbrite.com/e/verde-70-en-miami-tickets-2001152275325',
      vendor: 'Eventbrite',
    });
  });

  it('falls back to a link introduced by a tickets label', () => {
    expect(findExternalTicketLink('Más info: https://example.org/info\nEntradas: https://venta.example.org/evento/9.')).toEqual({
      url: 'https://venta.example.org/evento/9',
      vendor: null,
    });
    expect(findExternalTicketLink('Tickets here: https://shop.example.org/t')).toEqual({ url: 'https://shop.example.org/t', vendor: null });
  });

  it('ignores unrelated links and empty input', () => {
    expect(findExternalTicketLink('Síguenos en https://www.instagram.com/verde70oficial/')).toBeNull();
    expect(findExternalTicketLink('')).toBeNull();
    expect(findExternalTicketLink(null)).toBeNull();
    expect(findExternalTicketLink('Entradas: javascript:alert(1)')).toBeNull();
  });
});

describe('localizeTicketUnavailableReason', () => {
  it('translates the known server reasons', () => {
    expect(localizeTicketUnavailableReason('This event has no approved active ticket price and fee policy', false))
      .toBe('Este evento todavía no vende entradas a través de TDF.');
    expect(localizeTicketUnavailableReason('Ticket inventory is currently exhausted', true)).toBe('Tickets are sold out.');
  });

  it('keeps a neutral fallback for unknown reasons', () => {
    expect(localizeTicketUnavailableReason('Something new', false)).toBe('Las entradas no están disponibles en TDF por ahora.');
    expect(localizeTicketUnavailableReason(null, true)).toBe('Tickets are not available through TDF right now.');
  });
});
