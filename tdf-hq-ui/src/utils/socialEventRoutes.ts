const LEGACY_SOCIAL_EVENTS_PATH = '/social/events';

export function canonicalizeLegacySocialEventsPath(
  pathname: string,
  search = '',
  hash = '',
) {
  const isLegacyEventsPath =
    pathname === LEGACY_SOCIAL_EVENTS_PATH
    || pathname.startsWith(`${LEGACY_SOCIAL_EVENTS_PATH}/`);
  if (!isLegacyEventsPath) return null;

  const suffix = pathname.slice(LEGACY_SOCIAL_EVENTS_PATH.length);
  return `/social/eventos${suffix}${search}${hash}`;
}
