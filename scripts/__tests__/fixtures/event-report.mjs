import { createCipheriv, createHash } from 'node:crypto';

// Entirely fictional data. Never import the private editorial snapshot in tests.
export const reportToken = Buffer.alloc(32, 7).toString('base64url');
export const syntheticReport = {
  eventId: '999', title: 'Synthetic report fixture', date: '1 enero 2030',
  venue: 'Example venue', snapshotDate: '2 enero 2030', currency: 'USD',
  reportedGrossCents: 12000, owedToVenueCents: 2400,
  provisionalProfitCents: 9600, provisionalShareCents: 4800,
  venueAmountLabel: 'Adeudado al local ficticio',
  profitExplanation: 'Utilidad provisional USD96; USD48 por parte, antes de comisiones.',
  beerCaption: 'Dos consumos ficticios', beerExplanation: 'Sin doble descuento.',
  beer: [{ label: 'Ejemplo', pints: 2, costCents: 2400 }],
  reconciliation: 'Diferencia ficticia pendiente de conciliar.',
  pending: ['Revisar la liquidación ficticia.'],
  accounting: 'Solo lectura; ningún movimiento creado.', source: 'Fixture synthetic-only',
};

// Independent Node/OpenSSL producer exercises interoperability with Worker WebCrypto.
export function independentEnvelope(report = syntheticReport, token = reportToken, options = {}) {
  const nonce = Buffer.alloc(12, 3);
  const cipher = createCipheriv('aes-256-gcm', Buffer.from(token, 'base64url'), nonce);
  cipher.setAAD(Buffer.from(options.aad ?? 'tdf-event-report:v1'));
  const plaintext = options.plaintext ?? JSON.stringify(report);
  const ciphertext = Buffer.concat([cipher.update(plaintext), cipher.final(), cipher.getAuthTag()]);
  return { version: 1, tokenSha256: createHash('sha256').update(token).digest('hex'),
    nonce: nonce.toString('base64url'), ciphertext: ciphertext.toString('base64url') };
}
