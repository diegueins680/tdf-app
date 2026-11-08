import { serializeCsvCell } from './csv';

describe('CSV text-cell encoding', () => {
  it.each([
    '=1+1', '+1', '-1', '@SUM(1,1)', ' \t=1',
    '\uFF1D1+1', '\uFF0B1', '\uFF0D1', '\uFF201',
    '\tplain', '\rplain', '\nplain', ' \tplain',
  ])('prefixes formula-like or leading-control string %p', value => {
    expect(serializeCsvCell(value)).toBe(`"'${value}"`);
  });

  it('preserves numeric values, boolean values and empty optional cells', () => {
    expect(serializeCsvCell(-123)).toBe('"-123"');
    expect(serializeCsvCell(-9007199254740991)).toBe('"-9007199254740991"');
    expect(serializeCsvCell(-1.25)).toBe('"-1.25"');
    expect(serializeCsvCell(0)).toBe('"0"');
    expect(serializeCsvCell(true)).toBe('"true"');
    expect(serializeCsvCell(false)).toBe('"false"');
    expect(serializeCsvCell(null)).toBe('""');
    expect(serializeCsvCell(undefined)).toBe('""');
  });

  it('preserves ordinary text, embedded delimiters, quotes and newlines', () => {
    expect(serializeCsvCell('plain, "quoted"\nnext')).toBe('"plain, ""quoted""\nnext"');
    expect(serializeCsvCell(' plain')).toBe('" plain"');
    expect(serializeCsvCell("'=1+1")).toBe('"\'=1+1"');
    expect(serializeCsvCell('USD')).toBe('"USD"');
  });
});
