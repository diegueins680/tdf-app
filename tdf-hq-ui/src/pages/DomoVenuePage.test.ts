import { calculateQuote } from './domoVenueQuote';

describe('DomoVenuePage quote', () => {
  it('charges venue usage only once', () => {
    const quote = calculateQuote({
      fullName: '',
      email: '',
      phone: '',
      eventType: 'wedding',
      guests: 80,
      startsAt: '2026-08-28T10:00',
      durationHours: 8,
      setupHours: 2,
      catering: false,
      production: false,
      transport: false,
      notes: '',
    });

    expect(quote.lines).toEqual([
      { label: 'Uso del espacio por 8 horas', amountCents: 144000 },
      { label: 'Montaje y desmontaje (2 horas)', amountCents: 14000 },
      { label: '20 invitados adicionales', amountCents: 16000 },
    ]);
    expect(quote.subtotalCents).toBe(174000);
    expect(quote.taxCents).toBe(20880);
    expect(quote.totalCents).toBe(194880);
    expect(quote.depositCents).toBe(77952);
  });
});
