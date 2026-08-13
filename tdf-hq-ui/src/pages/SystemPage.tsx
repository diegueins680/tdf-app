import { useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Meta } from '../api/meta';
import { Catalogs } from '../api/catalogs';
import PageShell from '../components/PageShell';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useThemeMode } from '../theme/AppThemeProvider';
import { formatDateTime } from '../utils/formatters';

function availableTimezones(): string[] {
  const intl = Intl as typeof Intl & { supportedValuesOf?: (key: 'timeZone') => string[] };
  try {
    return intl.supportedValuesOf?.('timeZone') ?? [];
  } catch {
    return [];
  }
}

export default function SystemPage() {
  const { t } = useTranslation();
  const preferences = useLocalePreferences();
  const {
    preferenceId: themePreferenceId,
    options: themeOptions,
    catalogSource: themeCatalogSource,
    catalogLoading: themeCatalogLoading,
    catalogError: themeCatalogError,
    setModeById: setThemeModeById,
  } = useThemeMode();
  const [localeId, setLocaleId] = useState(preferences.localeId);
  const [currencyId, setCurrencyId] = useState(preferences.currencyId);
  const [timezone, setTimezone] = useState(preferences.timezone);
  const [countryId, setCountryId] = useState(preferences.countryId ?? '');
  const [saveError, setSaveError] = useState<string | null>(null);
  const [saved, setSaved] = useState(false);

  useEffect(() => {
    setLocaleId(preferences.localeId);
    setCurrencyId(preferences.currencyId);
    setTimezone(preferences.timezone);
    setCountryId(preferences.countryId ?? '');
  }, [preferences.localeId, preferences.currencyId, preferences.timezone, preferences.countryId]);

  const { data: version, isLoading: versionLoading, error: versionError } = useQuery({
    queryKey: ['meta', 'version'], queryFn: Meta.version,
  });
  const { data: health, isLoading: healthLoading, error: healthError } = useQuery({
    queryKey: ['meta', 'health'], queryFn: Meta.health,
  });
  const regionalCatalogsQuery = useQuery({
    queryKey: ['catalogs', 'regional-preferences', preferences.locale],
    queryFn: () => Catalogs.listPublicBatch(
      ['locales', 'currencies', 'countries'],
      { locale: preferences.locale, page: 1, pageSize: 500 },
    ),
  });
  const regionalCatalogs = useMemo(
    () => regionalCatalogsQuery.data?.catalogs ?? [],
    [regionalCatalogsQuery.data?.catalogs],
  );
  const localeOptions = useMemo(
    () => regionalCatalogs.find((page) => page.catalog.code === 'locales')?.items ?? [],
    [regionalCatalogs],
  );
  const currencyOptions = useMemo(
    () => regionalCatalogs.find((page) => page.catalog.code === 'currencies')?.items ?? [],
    [regionalCatalogs],
  );
  const countries = useMemo(
    () => regionalCatalogs.find((page) => page.catalog.code === 'countries')?.items ?? [],
    [regionalCatalogs],
  );
  useEffect(() => {
    if (countryId || !preferences.countryCode) return;
    const legacyMatch = countries.find((country) => country.code === preferences.countryCode);
    if (legacyMatch) setCountryId(legacyMatch.id);
  }, [countries, countryId, preferences.countryCode]);
  const loading = versionLoading || healthLoading || regionalCatalogsQuery.isLoading;
  const errMsg = useMemo(() => {
    if (versionError instanceof Error) return versionError.message;
    if (healthError instanceof Error) return healthError.message;
    if (regionalCatalogsQuery.error instanceof Error) return regionalCatalogsQuery.error.message;
    return null;
  }, [versionError, healthError, regionalCatalogsQuery.error]);
  const commitInfo = version?.commit ? version.commit.slice(0, 7) : null;
  const healthColor = (health?.status ?? '').toLowerCase() === 'ok' ? 'success' : 'warning';
  const timezones = [...new Set([timezone, preferences.timezone, 'UTC', ...availableTimezones()])];

  const save = async () => {
    setSaveError(null);
    setSaved(false);
    try {
      await preferences.savePreferences({
        localeId,
        currencyId,
        timezone,
        countryId: countryId || null,
      });
      setSaved(true);
    } catch (error) {
      setSaveError(error instanceof Error ? error.message : t('common.error'));
    }
  };

  return (
    <PageShell title={t('system.title')} subtitle={t('system.subtitle')}>
      <Stack gap={3}>
        <Paper sx={{ p: 3 }}>
          <Stack gap={2}>
            <Stack>
              <Typography variant="h6">{t('preferences.title')}</Typography>
              <Typography color="text.secondary">{t('preferences.subtitle')}</Typography>
            </Stack>
            {saveError && <Alert severity="error">{saveError}</Alert>}
            {saved && <Alert severity="success">{t('common.saved')}</Alert>}
            {themeCatalogSource === 'emergency' && !themeCatalogLoading && (
              <Alert severity={themeCatalogError ? 'warning' : 'info'}>
                No se pudo validar el catálogo de apariencia; se usan opciones de emergencia hasta la próxima sincronización.
              </Alert>
            )}
            <Stack direction={{ xs: 'column', md: 'row' }} gap={2}>
              <TextField
                select
                fullWidth
                label="Apariencia"
                value={themePreferenceId}
                onChange={(event) => setThemeModeById(event.target.value)}
                disabled={themeCatalogLoading && themeOptions.length === 0}
              >
                {themeOptions.map((option) => (
                  <MenuItem key={option.id} value={option.id}>{option.label}</MenuItem>
                ))}
              </TextField>
              <TextField select fullWidth label={t('preferences.language')} value={localeId} onChange={(event) => setLocaleId(event.target.value)}>
                {localeOptions.map((option) => (
                  <MenuItem key={option.id} value={option.id}>{option.name}</MenuItem>
                ))}
              </TextField>
              <TextField select fullWidth label={t('preferences.currency')} value={currencyId} onChange={(event) => setCurrencyId(event.target.value)}>
                {currencyOptions.map((option) => <MenuItem key={option.id} value={option.id}>{option.name} · {option.code}</MenuItem>)}
              </TextField>
              <TextField select fullWidth label={t('preferences.timezone')} value={timezone} onChange={(event) => setTimezone(event.target.value)}>
                {timezones.map((value) => <MenuItem key={value} value={value}>{value}</MenuItem>)}
              </TextField>
              <TextField
                select
                fullWidth
                label={t('preferences.country')}
                value={countryId}
                onChange={(event) => setCountryId(event.target.value)}
                helperText={t('preferences.countryHint')}
              >
                <MenuItem value="">—</MenuItem>
                {countries.map((country) => (
                  <MenuItem key={country.id} value={country.id}>
                    {country.name} · {country.code}
                  </MenuItem>
                ))}
              </TextField>
            </Stack>
            <Box><Button variant="contained" onClick={() => void save()} disabled={preferences.saving || !localeId || !currencyId || regionalCatalogsQuery.isError}>{preferences.saving ? t('common.saving') : t('common.save')}</Button></Box>
          </Stack>
        </Paper>

        {loading && <Box display="flex" alignItems="center" justifyContent="center" minHeight={160}><CircularProgress /></Box>}
        {!loading && errMsg && <Alert severity="error">{errMsg}</Alert>}
        {!loading && !errMsg && (
          <Paper sx={{ p: 3 }}>
            <Stack gap={2}>
              <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={2}>
                <Stack>
                  <Typography variant="subtitle2" color="text.secondary">{t('system.application')}</Typography>
                  <Typography variant="h6">{version?.name ?? '—'}</Typography>
                </Stack>
                <Stack direction="row" gap={1} alignItems="center">
                  <Typography variant="subtitle2" color="text.secondary">{t('system.version')}</Typography>
                  <Chip label={version?.version ?? '—'} color="primary" size="small" />
                  {health && <Chip label={`${t('system.status')}: ${health.status}`} color={healthColor} size="small" />}
                </Stack>
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} gap={2}>
                <Stack><Typography variant="subtitle2" color="text.secondary">Commit</Typography><Typography variant="body2">{commitInfo ?? '—'}</Typography></Stack>
                <Stack><Typography variant="subtitle2" color="text.secondary">{t('system.built')}</Typography><Typography variant="body2">{version?.buildTime ? formatDateTime(version.buildTime, { locale: preferences.locale, timeZone: preferences.timezone }) : '—'}</Typography></Stack>
                <Stack><Typography variant="subtitle2" color="text.secondary">{t('system.codebase')}</Typography><Typography variant="body2">{health?.version ?? '—'}</Typography></Stack>
              </Stack>
            </Stack>
          </Paper>
        )}
      </Stack>
    </PageShell>
  );
}
