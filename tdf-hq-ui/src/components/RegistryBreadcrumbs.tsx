import { useEffect, useMemo, useState } from 'react';
import { Breadcrumbs, Link, Typography } from '@mui/material';
import { Link as RouterLink, matchPath, useLocation } from 'react-router-dom';
import appI18n from '../i18n';
import { useSession } from '../session/SessionContext';
import {
  evaluateFeatureAccess,
  featureBreadcrumbs,
  featureLabel,
  getFeatureByPath,
  type FeatureDefinition,
} from '../features/featureRegistry';

function resolveBreadcrumbRoute(
  target: FeatureDefinition,
  current: FeatureDefinition,
  pathname: string,
): string | null {
  if (!target.webRoute) return null;
  if (!target.webRoute.includes(':')) return target.webRoute;
  if (!current.webRoute) return null;
  const matched = matchPath({ path: current.webRoute, end: true }, pathname);
  if (!matched) return null;
  let resolved = target.webRoute;
  for (const [key, value] of Object.entries(matched.params)) {
    if (!value) continue;
    resolved = resolved.replace(`:${key}?`, encodeURIComponent(value));
    resolved = resolved.replace(`:${key}`, encodeURIComponent(value));
  }
  return resolved.includes(':') ? null : resolved.replace(/\/$/, '') || '/';
}

export default function RegistryBreadcrumbs() {
  const location = useLocation();
  const { session } = useSession();
  const [locale, setLocale] = useState(() => appI18n.resolvedLanguage ?? appI18n.language ?? 'es');
  const currentFeature = getFeatureByPath(location.pathname);

  useEffect(() => {
    const handleLanguageChanged = (language: string) => setLocale(language || 'es');
    appI18n.on('languageChanged', handleLanguageChanged);
    return () => appI18n.off('languageChanged', handleLanguageChanged);
  }, []);

  const crumbs = useMemo(() => {
    if (!currentFeature || currentFeature.technical || currentFeature.breadcrumb.length === 0) return [];
    return featureBreadcrumbs(currentFeature).flatMap((feature, index, all) => {
      const isCurrent = index === all.length - 1;
      const route = resolveBreadcrumbRoute(feature, currentFeature, location.pathname);
      if (!isCurrent) {
        const access = evaluateFeatureAccess(feature, {
          authenticated: Boolean(session),
          roles: session?.roles,
          modules: session?.modules,
          featureFlags: session?.featureFlags,
        });
        if (access.state !== 'allowed' || !route) return [];
      }
      return [{ feature, route, isCurrent }];
    });
  }, [currentFeature, location.pathname, session]);

  if (crumbs.length < 2) return null;

  return (
    <Breadcrumbs aria-label="Migas de pan" sx={{ mb: 2 }}>
      {crumbs.map(({ feature, route, isCurrent }) => isCurrent || !route ? (
        <Typography key={feature.id} color="text.primary" aria-current={isCurrent ? 'page' : undefined}>
          {featureLabel(feature, locale)}
        </Typography>
      ) : (
        <Link key={feature.id} component={RouterLink} to={route} underline="hover" color="inherit">
          {featureLabel(feature, locale)}
        </Link>
      ))}
    </Breadcrumbs>
  );
}
