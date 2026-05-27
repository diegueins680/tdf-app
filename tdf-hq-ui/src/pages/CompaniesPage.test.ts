import { readFileSync } from 'node:fs';

describe('CompaniesPage mutation naming', () => {
  it('keeps create and update mutations named by their action', () => {
    const source = readFileSync(new URL('./CompaniesPage.tsx', import.meta.url), 'utf8');

    expect(source).toMatch(/\bconst\s+createCompanyMutation\s*=\s*useMutation\(/);
    expect(source).toMatch(/\bconst\s+updateCompanyMutation\s*=\s*useMutation\(/);
    expect(source).toMatch(/\bconst\s+createCompanyQueryClient\s*=\s*useQueryClient\(\)/);
    expect(source).toMatch(/\bconst\s+editCompanyQueryClient\s*=\s*useQueryClient\(\)/);
    expect(source).not.toMatch(/\bconst\s+mutation\s*=\s*useMutation\(/);
    expect(source).not.toMatch(/\bmutation\./);
    expect(source).not.toMatch(/\bconst\s+qc\s*=\s*useQueryClient\(\)/);
  });
});
