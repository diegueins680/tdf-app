import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Card,
  CardContent,
  Chip,
  FormControlLabel,
  MenuItem,
  Stack,
  Switch,
  TextField,
  Typography,
} from '@mui/material';

import PageShell, { EmptyState } from '../components/PageShell';
import { get } from '../api/client';
import {
  EventDiscoverySourcesAPI,
  type EventDiscoverySource,
  type EventDiscoverySourceType,
  type EventDiscoverySourceWrite,
} from '../api/eventDiscoverySources';

interface EventCityDTO {
  eventCityId: string;
  eventCityName: string;
  eventCityCountryCode: string;
}

type SourceDraft = EventDiscoverySourceWrite;

const sourceTypes: { value: EventDiscoverySourceType; label: string }[] = [
  { value: 'ical', label: 'Venue iCalendar' },
  { value: 'json', label: 'Venue JSON' },
  { value: 'ticketmaster', label: 'Ticketmaster' },
  { value: 'buenplan', label: 'Buen Plan' },
];

const draftFromSource = (source: EventDiscoverySource): SourceDraft => ({
  discoverySourceWriteKey: source.discoverySourceKey,
  discoverySourceWriteName: source.discoverySourceName,
  discoverySourceWriteType: source.discoverySourceType,
  discoverySourceWriteFeedUrl: source.discoverySourceFeedUrl ?? null,
  discoverySourceWriteCityId: source.discoverySourceCityId ?? null,
  discoverySourceWriteEnabled: source.discoverySourceEnabled,
  discoverySourceWritePriority: source.discoverySourcePriority,
});

const emptyDraft = (): SourceDraft => ({
  discoverySourceWriteKey: '',
  discoverySourceWriteName: '',
  discoverySourceWriteType: 'ical',
  discoverySourceWriteFeedUrl: '',
  discoverySourceWriteCityId: '',
  discoverySourceWriteEnabled: true,
  discoverySourceWritePriority: 400,
});

const isVenueFeed = (sourceType: EventDiscoverySourceType) =>
  sourceType === 'ical' || sourceType === 'json';

const formatTimestamp = (value?: string | null) => {
  if (!value) return 'Nunca';
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? value : parsed.toLocaleString('es-EC');
};

export default function EventDiscoverySourcesPage() {
  const queryClient = useQueryClient();
  const [drafts, setDrafts] = useState<Record<string, SourceDraft>>({});
  const [newDraft, setNewDraft] = useState<SourceDraft>(emptyDraft);
  const sourcesQuery = useQuery({
    queryKey: ['event-discovery-sources'],
    queryFn: EventDiscoverySourcesAPI.list,
  });
  const citiesQuery = useQuery({
    queryKey: ['event-cities', 'source-admin'],
    queryFn: () => get<EventCityDTO[]>('/social-events/cities'),
  });
  const saveMutation = useMutation({
    mutationFn: ({ sourceId, draft }: { sourceId: string; draft: SourceDraft }) =>
      EventDiscoverySourcesAPI.update(sourceId, normalizeDraft(draft)),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['event-discovery-sources'] });
    },
  });
  const createMutation = useMutation({
    mutationFn: (draft: SourceDraft) => EventDiscoverySourcesAPI.create(normalizeDraft(draft)),
    onSuccess: () => {
      setNewDraft(emptyDraft());
      void queryClient.invalidateQueries({ queryKey: ['event-discovery-sources'] });
    },
  });

  const sources = sourcesQuery.data ?? [];
  const cities = useMemo(
    () => [...(citiesQuery.data ?? [])].sort((left, right) =>
      `${left.eventCityCountryCode}:${left.eventCityName}`.localeCompare(
        `${right.eventCityCountryCode}:${right.eventCityName}`,
      )),
    [citiesQuery.data],
  );
  const mutationError = saveMutation.error ?? createMutation.error;
  const readDraft = (source: EventDiscoverySource) =>
    drafts[source.discoverySourceId] ?? draftFromSource(source);
  const writeDraft = (source: EventDiscoverySource, update: Partial<SourceDraft>) => {
    setDrafts((current) => ({
      ...current,
      [source.discoverySourceId]: {
        ...(current[source.discoverySourceId] ?? draftFromSource(source)),
        ...update,
      },
    }));
  };

  return (
    <PageShell
      title="Fuentes de eventos"
      subtitle="Controla ticketing y registra feeds estructurados de venues. Una prioridad mayor gana los datos canónicos."
      loading={sourcesQuery.isLoading || citiesQuery.isLoading}
    >
      <Stack spacing={2}>
        <Alert severity="info">
          El cron consulta únicamente ciudades seguidas por usuarios. Desactivar una fuente no elimina eventos:
          sólo detiene nuevas importaciones y conserva las referencias existentes.
        </Alert>
        {(sourcesQuery.isError || citiesQuery.isError) && (
          <Alert severity="error">
            No se pudieron cargar las fuentes o ciudades. Esta pantalla requiere acceso de administrador estricto.
          </Alert>
        )}
        {mutationError && (
          <Alert severity="error">
            {mutationError instanceof Error ? mutationError.message : 'No se pudo guardar la fuente.'}
          </Alert>
        )}

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6">Registrar feed de venue</Typography>
              <SourceFields
                draft={newDraft}
                cities={cities}
                onChange={(update) => setNewDraft((current) => ({ ...current, ...update }))}
              />
              <Button
                variant="contained"
                onClick={() => createMutation.mutate(newDraft)}
                disabled={createMutation.isPending}
                sx={{ alignSelf: 'flex-start' }}
              >
                {createMutation.isPending ? 'Registrando…' : 'Registrar fuente'}
              </Button>
            </Stack>
          </CardContent>
        </Card>

        {!sourcesQuery.isLoading && !sourcesQuery.isError && sources.length === 0 && (
          <EmptyState
            title="No hay fuentes configuradas"
            description="Registra un feed de venue o reinicia el cron para restaurar Ticketmaster y Buen Plan."
          />
        )}

        {sources.map((source) => {
          const draft = readDraft(source);
          const saving =
            saveMutation.isPending && saveMutation.variables?.sourceId === source.discoverySourceId;
          return (
            <Card key={source.discoverySourceId} variant="outlined">
              <CardContent>
                <Stack spacing={1.5}>
                  <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                    <Typography variant="h6">{source.discoverySourceName}</Typography>
                    <Chip
                      size="small"
                      color={source.discoverySourceEnabled ? 'success' : 'default'}
                      label={source.discoverySourceEnabled ? 'Activa' : 'Desactivada'}
                    />
                    {source.discoverySourceConsecutiveFailures > 0 && (
                      <Chip
                        size="small"
                        color="warning"
                        label={`${source.discoverySourceConsecutiveFailures} fallo(s)`}
                      />
                    )}
                  </Stack>
                  <SourceFields
                    draft={draft}
                    cities={cities}
                    lockProviderIdentity={!isVenueFeed(source.discoverySourceType)}
                    onChange={(update) => writeDraft(source, update)}
                  />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} alignItems={{ sm: 'center' }}>
                    <Button
                      variant="contained"
                      onClick={() => saveMutation.mutate({ sourceId: source.discoverySourceId, draft })}
                      disabled={saving}
                    >
                      {saving ? 'Guardando…' : 'Guardar'}
                    </Button>
                    <Typography variant="caption" color="text.secondary">
                      Último éxito: {formatTimestamp(source.discoverySourceLastSuccessAt)}
                    </Typography>
                  </Stack>
                  {source.discoverySourceLastError && (
                    <Alert severity="warning">{source.discoverySourceLastError}</Alert>
                  )}
                </Stack>
              </CardContent>
            </Card>
          );
        })}
      </Stack>
    </PageShell>
  );
}

function normalizeDraft(draft: SourceDraft): SourceDraft {
  const venueFeed = isVenueFeed(draft.discoverySourceWriteType);
  const normalizedFeedUrl = draft.discoverySourceWriteFeedUrl?.trim() ?? null;
  const normalizedCityId = draft.discoverySourceWriteCityId?.trim() ?? null;
  return {
    ...draft,
    discoverySourceWriteKey: draft.discoverySourceWriteKey.trim().toLowerCase(),
    discoverySourceWriteName: draft.discoverySourceWriteName.trim(),
    discoverySourceWriteFeedUrl: venueFeed
      ? normalizedFeedUrl
      : null,
    discoverySourceWriteCityId: venueFeed
      ? normalizedCityId
      : null,
    discoverySourceWritePriority: Math.trunc(draft.discoverySourceWritePriority),
  };
}

function SourceFields({
  draft,
  cities,
  lockProviderIdentity = false,
  onChange,
}: {
  draft: SourceDraft;
  cities: EventCityDTO[];
  lockProviderIdentity?: boolean;
  onChange: (update: Partial<SourceDraft>) => void;
}) {
  const venueFeed = isVenueFeed(draft.discoverySourceWriteType);
  const availableSourceTypes = lockProviderIdentity
    ? sourceTypes
    : sourceTypes.filter((sourceType) => isVenueFeed(sourceType.value));
  return (
    <Stack spacing={1.25}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.25}>
        <TextField
          label="Clave"
          value={draft.discoverySourceWriteKey}
          onChange={(event) => onChange({ discoverySourceWriteKey: event.target.value })}
          disabled={lockProviderIdentity}
          size="small"
          fullWidth
          inputProps={{ maxLength: 80 }}
        />
        <TextField
          label="Nombre"
          value={draft.discoverySourceWriteName}
          onChange={(event) => onChange({ discoverySourceWriteName: event.target.value })}
          size="small"
          fullWidth
          inputProps={{ maxLength: 160 }}
        />
        <TextField
          select
          label="Tipo"
          value={draft.discoverySourceWriteType}
          onChange={(event) =>
            onChange({ discoverySourceWriteType: event.target.value as EventDiscoverySourceType })
          }
          disabled={lockProviderIdentity}
          size="small"
          fullWidth
        >
          {availableSourceTypes.map((sourceType) => (
            <MenuItem key={sourceType.value} value={sourceType.value}>
              {sourceType.label}
            </MenuItem>
          ))}
        </TextField>
      </Stack>
      {venueFeed && (
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.25}>
          <TextField
            label="URL HTTPS del feed"
            value={draft.discoverySourceWriteFeedUrl ?? ''}
            onChange={(event) => onChange({ discoverySourceWriteFeedUrl: event.target.value })}
            size="small"
            fullWidth
            inputProps={{ maxLength: 2048 }}
          />
          <TextField
            select
            label="Ciudad"
            value={draft.discoverySourceWriteCityId ?? ''}
            onChange={(event) => onChange({ discoverySourceWriteCityId: event.target.value })}
            size="small"
            fullWidth
          >
            {cities.map((city) => (
              <MenuItem key={city.eventCityId} value={city.eventCityId}>
                {city.eventCityName} · {city.eventCityCountryCode}
              </MenuItem>
            ))}
          </TextField>
        </Stack>
      )}
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} alignItems={{ sm: 'center' }}>
        <TextField
          label="Prioridad"
          type="number"
          value={draft.discoverySourceWritePriority}
          onChange={(event) =>
            onChange({ discoverySourceWritePriority: Number.parseInt(event.target.value, 10) || 0 })
          }
          size="small"
          inputProps={{ min: 0, max: 10000 }}
        />
        <FormControlLabel
          control={(
            <Switch
              checked={draft.discoverySourceWriteEnabled}
              onChange={(event) =>
                onChange({ discoverySourceWriteEnabled: event.target.checked })
              }
            />
          )}
          label="Fuente activa"
        />
      </Stack>
    </Stack>
  );
}
