import de from './locales/de';
import en from './locales/en';
import es from './locales/es';
import fr from './locales/fr';
import pt from './locales/pt';

type TranslationTree = { readonly [key: string]: string | TranslationTree };

const flattenKeys = (tree: TranslationTree, prefix = ''): string[] =>
  Object.entries(tree).flatMap(([key, value]) => {
    const path = prefix ? `${prefix}.${key}` : key;
    return typeof value === 'string' ? [path] : flattenKeys(value, path);
  });

const interpolationVariables = (value: string): string[] =>
  Array.from(value.matchAll(/{{\s*([^},\s]+)[^}]*}}/g), (match) => match[1] ?? '').sort();

describe('payment locale resources', () => {
  const resources = { en, es, fr, de, pt } as const;
  const namespaces = ['pagination', 'promoCode', 'refunds'] as const;

  it.each(namespaces)('keeps the %s namespace complete in every supported locale', (namespace) => {
    const englishTree = en[namespace] as TranslationTree;
    const expectedKeys = flattenKeys(englishTree).sort();

    Object.entries(resources).forEach(([locale, resource]) => {
      expect({ locale, keys: flattenKeys(resource[namespace] as TranslationTree).sort() }).toEqual({
        locale,
        keys: expectedKeys,
      });
    });
  });

  it.each(namespaces)('preserves interpolation variables in %s translations', (namespace) => {
    const englishTree = en[namespace] as TranslationTree;
    const expectedValues = new Map(
      Object.entries(englishTree).flatMap(([key, value]) =>
        typeof value === 'string'
          ? [[key, interpolationVariables(value)] as const]
          : flattenInterpolationValues(value, key),
      ),
    );

    Object.entries(resources).forEach(([locale, resource]) => {
      const translatedTree = resource[namespace] as TranslationTree;
      const translatedValues = new Map(flattenInterpolationValues(translatedTree));
      expect({ locale, variables: translatedValues }).toEqual({ locale, variables: expectedValues });
    });
  });
});

function flattenInterpolationValues(
  tree: TranslationTree,
  prefix = '',
): ReadonlyArray<readonly [string, string[]]> {
  return Object.entries(tree).flatMap(([key, value]) => {
    const path = prefix ? `${prefix}.${key}` : key;
    return typeof value === 'string'
      ? [[path, interpolationVariables(value)] as const]
      : flattenInterpolationValues(value, path);
  });
}
