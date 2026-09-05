import { readFileSync } from 'node:fs';

describe('inventory checkout Party reference', () => {
  const pageSource = readFileSync(new URL('./InventoryPage.tsx', import.meta.url), 'utf8');
  const dialogSource = readFileSync(new URL('../components/AssetDialogs.tsx', import.meta.url), 'utf8');

  it('does not download the Party directory for a free-text historical reference', () => {
    expect(pageSource).not.toMatch(/Parties\.list\s*\(/);
    expect(dialogSource).not.toContain('PartyDTO');
    expect(dialogSource).toContain('Este registro histórico guarda una referencia de texto, no un Party ID.');
  });
});
