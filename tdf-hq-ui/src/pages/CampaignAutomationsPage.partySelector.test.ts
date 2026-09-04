import { readFileSync } from 'node:fs';

const source = readFileSync(new URL('./CampaignAutomationsPage.tsx', import.meta.url), 'utf8');

describe('Campaign automation Party relationship regression', () => {
  it('uses the reusable server-backed multiple selector', () => {
    expect(source).toContain('<PartyMultiSelector');
    expect(source).toContain("search={{ kind: 'any', accountOnly: false }}");
    expect(source).toContain('selectedParties.map((party) => party.partyId)');
  });

  it('never downloads or filters the CRM directory in the browser', () => {
    expect(source).not.toContain('Parties.list');
    expect(source).not.toContain("from '../api/parties'");
    expect(source).not.toContain('primaryEmail');
    expect(source).not.toContain('primaryPhone');
    expect(source).not.toContain('whatsapp');
  });
});
