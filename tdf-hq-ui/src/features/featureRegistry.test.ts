import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  evaluateFeatureAccess,
  featureBreadcrumbs,
  featureRegistry,
  getFeatureByPath,
  searchFeatures,
} from './featureRegistry';

describe('featureRegistry', () => {
  it('has unique stable IDs and complete bilingual discovery metadata', () => {
    const ids = featureRegistry.map(({ id }) => id);
    expect(new Set(ids).size).toBe(ids.length);

    featureRegistry.forEach((feature) => {
      expect(feature.label.es.trim()).not.toBe('');
      expect(feature.label.en.trim()).not.toBe('');
      if (feature.searchable && !feature.technical) {
        expect(feature.synonyms.es.length + feature.synonyms.en.length + feature.keywords.length).toBeGreaterThan(0);
      }
    });
  });

  it('never exposes technical routes in global navigation or search', () => {
    const technical = featureRegistry.filter(({ technical }) => technical);
    expect(technical.length).toBeGreaterThan(0);
    expect(technical.every(({ globalMenu, searchable }) => !globalMenu && !searchable)).toBe(true);
    expect(searchFeatures('oauth callback').some(({ technical: isTechnical }) => isTechnical)).toBe(false);
  });

  it('matches static and dynamic routes exactly instead of treating dynamic details as global destinations', () => {
    expect(getFeatureByPath('/label/ddex')?.id).toBe('label.ddex.inbox');
    expect(getFeatureByPath('/label/ddex/documents/42')?.id).toBe('label.ddex.document');
    expect(getFeatureByPath('/label/ddex/documents/42/import')?.id).toBe('label.ddex.import');
    expect(getFeatureByPath('/social/eventos/nuevo')?.id).toBe('social.events.create');
    expect(getFeatureByPath('/social/eventos/abc')?.id).toBe('social.event.detail');
    expect(getFeatureByPath('/not-a-real-route')).toBeNull();
  });

  it('uses current backend Catalog access and returns safe locked decisions without revealing data', () => {
    const allowed = evaluateFeatureAccess('label.ddex.inbox', {
      authenticated: true,
      roles: ['A&R'],
      modules: ['CRM', 'Scheduling', 'Catalog'],
    }, 'view');
    const locked = evaluateFeatureAccess('label.ddex.inbox', {
      authenticated: true,
      roles: ['Fan', 'Customer'],
      modules: ['Packages'],
    }, 'view');

    expect(allowed.state).toBe('allowed');
    expect(locked.state).toBe('locked');
    expect(locked.missingModules).toEqual(['catalog']);
  });

  it('does not inherit view access for an undeclared action', () => {
    const decision = evaluateFeatureAccess('label.ddex.document', {
      authenticated: true,
      roles: ['Admin', 'Customer', 'Fan'],
      modules: ['Catalog'],
    }, 'publish');
    expect(decision.state).toBe('concealed');
    expect(decision.reason).toBe('concealed');
  });

  it('requires the authoritative session flag for flagged destinations', () => {
    const session = { authenticated: true, roles: ['Fan', 'Customer'], modules: ['Packages'] };
    expect(evaluateFeatureAccess('social.discovery', session, 'view').state).toBe('concealed');
    expect(evaluateFeatureAccess('social.discovery', {
      ...session,
      featureFlags: ['EVENT_DISCOVERY_ENABLED'],
    }, 'view').state).toBe('allowed');
  });

  it('uses the exact action associated with actionable routes', () => {
    const actionSession = { authenticated: true, roles: ['Fan', 'Customer'], modules: ['Packages'] };
    expect(evaluateFeatureAccess('artist.onboarding', actionSession, 'view').state).toBe('allowed');
    expect(evaluateFeatureAccess('artist.onboarding', actionSession, 'create').state).not.toBe('allowed');
    expect(getFeatureByPath('/artista/crear')?.routeAction).toBe('create');
  });

  it('searches Spanish, English, synonyms, abbreviations, and alternative spellings', () => {
    expect(searchFeatures('bandeja ddex').map(({ id }) => id)).toContain('label.ddex.inbox');
    expect(searchFeatures('ddex inbox').map(({ id }) => id)).toContain('label.ddex.inbox');
    expect(searchFeatures('DPID').map(({ id }) => id)).toContain('label.ddex.partners');
    expect(searchFeatures('reservacion').map(({ id }) => id)).toContain('bookings.public');
  });

  it('builds valid breadcrumbs from the same registry', () => {
    expect(featureBreadcrumbs(evaluateFeatureAccess('label.ddex.import', {
      authenticated: true,
      roles: ['Admin'],
      modules: ['Catalog'],
    }).feature).map(({ id }) => id)).toEqual([
      'label.ddex.inbox',
      'label.ddex.document',
      'label.ddex.import',
    ]);
  });

  it('keeps every declared route literal represented by a route or alias', () => {
    const routeDir = fileURLToPath(new URL('../routes/', import.meta.url));
    const sources = ['publicRoutes.tsx', 'protectedRoutes.tsx']
      .map((filename) => readFileSync(`${routeDir}${filename}`, 'utf8'))
      .join('\n');
    const declaredAbsolutePaths = Array.from(sources.matchAll(/<Route\s+path="(\/[^"]*)"/g), (match) => match[1]);
    const registeredRoutes = new Set(featureRegistry.flatMap((feature) => [feature.webRoute, ...feature.aliases]));
    const deliberatelyStructural = new Set(['/configuracion', '/crm', '/estudio', '/label', '/escuela', '/finanzas', '/operacion']);
    const unregistered = declaredAbsolutePaths.filter((path) =>
      path !== '*' && !registeredRoutes.has(path) && !deliberatelyStructural.has(path),
    );
    expect(unregistered).toEqual([]);
  });
});
