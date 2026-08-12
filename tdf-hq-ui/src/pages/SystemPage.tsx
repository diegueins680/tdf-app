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
import PageShell from '../components/PageShell';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useThemeMode } from '../theme/AppThemeProvider';
import { formatDateTime } from '../utils/formatters';
import { useDocumentTitle } from '../hooks/useDocumentTitle';

const COMMON_TIMEZONES = [
  'UTC',
  'America/Los_Angeles',
  'America/Denver',
  'America/Chicago',
  'America/New_York',
  'America/Toronto',
  'America/Mexico_City',
  'America/Bogota',
  'America/Lima',
  'America/Sao_Paulo',
  'Europe/London',
  'Europe/Paris',
  'Europe/Berlin',
  'Europe/Madrid',
  'Africa/Johannesburg',
  'Asia/Dubai',
  'Asia/Kolkata',
  'Asia/Singapore',
  'Asia/Tokyo',
  'Australia/Sydney',
];

const LANGUAGE_LABELS: Record<string, string> = {
  en: 'English',
  es: 'Español',
  fr: 'Français',
  de: 'Deutsch',
  pt: 'Português',
};

function availableTimezones(): string[] {
  const intl = Intl as typeof Intl & { supportedValuesOf?: (key: 'timeZone') => string[] };
  try {
    return intl.supportedValuesOf?.('timeZone') ?? COMMON_TIMEZONES;
  } catch {
    return COMMON_TIMEZONES;
  }
}

export default function SystemPage() {
  useDocumentTitle('Configuración / Sistema');
  const { t } = useTranslation();
  const preferences = useLocalePreferences();
  const { preference: themePreference, setMode: setThemeMode } = useThemeMode();
  const [locale, setLocale] = useState(preferences.locale);
  const [currency, setCurrency] = useState(preferences.currency);
  const [timezone, setTimezone] = useState(preferences.timezone);
  const [countryCode, setCountryCode] = useState(preferences.countryCode ?? '');
  const [saveError, setSaveError] = useState<string | null>(null);
  const [saved, setSaved] = useState(false);

  useEffect(() => {
    setLocale(preferences.locale);
    setCurrency(preferences.currency);
    setTimezone(preferences.timezone);
    setCountryCode(preferences.countryCode ?? '');
  }, [preferences.locale, preferences.currency, preferences.timezone, preferences.countryCode]);

  const { data: version, isLoading: versionLoading, error: versionError } = useQuery({
    queryKey: ['meta', 'version'], queryFn: Meta.version,
  });
  const { data: health, isLoading: healthLoading, error: healthError } = useQuery({
    queryKey: ['meta', 'health'], queryFn: Meta.health,
  });
  const loading = versionLoading || healthLoading;
  const errMsg = useMemo(() => {
    if (versionError instanceof Error) return versionError.message;
    if (healthError instanceof Error) return healthError.message;
    return null;
  }, [versionError, healthError]);
  const commitInfo = version?.commit ? version.commit.slice(0, 7) : null;
  const healthColor = (health?.status ?? '').toLowerCase() === 'ok' ? 'success' : 'warning';
  const timezones = [...new Set([timezone, preferences.timezone, 'UTC', ...availableTimezones()])];

  const save = async () => {
    setSaveError(null);
    setSaved(false);
    try {
      const normalizedCountryCode = countryCode.trim().toUpperCase();
      await preferences.savePreferences({
        locale,
        currency,
        timezone,
        countryCode: normalizedCountryCode.length > 0 ? normalizedCountryCode : null,
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
            <Stack direction={{ xs: 'column', md: 'row' }} gap={2}>
              <TextField
                select
                fullWidth
                label="Apariencia"
                value={themePreference}
                onChange={(event) => setThemeMode(event.target.value as 'light' | 'dark' | 'system')}
              >
                <MenuItem value="system">Usar configuración del sistema</MenuItem>
                <MenuItem value="light">Tema claro</MenuItem>
                <MenuItem value="dark">Tema oscuro</MenuItem>
              </TextField>
              <TextField select fullWidth label={t('preferences.language')} value={locale} onChange={(event) => setLocale(event.target.value)}>
                {preferences.supportedLocales.map((value) => (
                  <MenuItem key={value} value={value}>{LANGUAGE_LABELS[value] ?? value}</MenuItem>
                ))}
              </TextField>
              <TextField select fullWidth label={t('preferences.currency')} value={currency} onChange={(event) => setCurrency(event.target.value)}>
                {preferences.supportedCurrencies.map((value) => <MenuItem key={value} value={value}>{value}</MenuItem>)}
              </TextField>
              <TextField select fullWidth label={t('preferences.timezone')} value={timezone} onChange={(event) => setTimezone(event.target.value)}>
                {timezones.map((value) => <MenuItem key={value} value={value}>{value}</MenuItem>)}
              </TextField>
              <TextField
                fullWidth
                label={t('preferences.country')}
                value={countryCode}
                onChange={(event) => setCountryCode(event.target.value.replace(/[^A-Za-z]/g, '').slice(0, 2).toUpperCase())}
                helperText={t('preferences.countryHint')}
              />
            </Stack>
            <Box><Button variant="contained" onClick={() => void save()} disabled={preferences.saving}>{preferences.saving ? t('common.saving') : t('common.save')}</Button></Box>
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
