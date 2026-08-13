import { useEffect, useMemo, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import {
  Alert,
  Badge,
  Box,
  Button,
  Card,
  CardActionArea,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
  Divider,
  Drawer,
  FormControl,
  FormControlLabel,
  InputLabel,
  List,
  ListItem,
  ListItemButton,
  ListItemText,
  MenuItem,
  Paper,
  Select,
  Stack,
  Tab,
  Tabs,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import AssignmentIndOutlinedIcon from '@mui/icons-material/AssignmentIndOutlined';
import FilterAltOutlinedIcon from '@mui/icons-material/FilterAltOutlined';
import OpenInNewOutlinedIcon from '@mui/icons-material/OpenInNewOutlined';
import RefreshOutlinedIcon from '@mui/icons-material/RefreshOutlined';
import SaveOutlinedIcon from '@mui/icons-material/SaveOutlined';
import WarningAmberOutlinedIcon from '@mui/icons-material/WarningAmberOutlined';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import PageShell, { EmptyState } from '../components/PageShell';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import {
  Operations,
  type OperationsFilters,
  type OperationsPriority,
  type OperationsStatus,
  type OperationsWorkItem,
} from '../api/operations';

const STATUS_ORDER: OperationsStatus[] = [
  'new', 'seen', 'assigned', 'in_progress', 'waiting', 'resolved', 'archived',
];
const KANBAN_STATUSES: OperationsStatus[] = [
  'new', 'seen', 'assigned', 'in_progress', 'waiting', 'resolved',
];
const PRIORITIES: OperationsPriority[] = ['urgent', 'high', 'normal', 'low'];
const PRIORITY_COLOR: Record<OperationsPriority, 'error' | 'warning' | 'info' | 'default'> = {
  urgent: 'error', high: 'warning', normal: 'info', low: 'default',
};
const DEFAULT_FILTERS: OperationsFilters = { limit: 60 };

const isOperationsStatus = (value: unknown): value is OperationsStatus =>
  typeof value === 'string' && STATUS_ORDER.includes(value as OperationsStatus);

function statusLabel(t: ReturnType<typeof useTranslation>['t'], status: OperationsStatus) {
  return t(`operations.status.${status}`);
}

function priorityLabel(t: ReturnType<typeof useTranslation>['t'], priority: OperationsPriority) {
  return t(`operations.priority.${priority}`);
}

function asFilters(value: unknown): OperationsFilters {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return DEFAULT_FILTERS;
  const source = value as Record<string, unknown>;
  return {
    ...DEFAULT_FILTERS,
    q: typeof source['q'] === 'string' ? source['q'] : undefined,
    entityType: typeof source['entityType'] === 'string' ? source['entityType'] : undefined,
    status: isOperationsStatus(source['status']) ? source['status'] : undefined,
    priority: PRIORITIES.includes(source['priority'] as OperationsPriority)
      ? source['priority'] as OperationsPriority
      : undefined,
    slaState: typeof source['slaState'] === 'string' ? source['slaState'] as OperationsFilters['slaState'] : undefined,
    sourceChannel: typeof source['sourceChannel'] === 'string' ? source['sourceChannel'] : undefined,
    paymentState: typeof source['paymentState'] === 'string' ? source['paymentState'] : undefined,
    service: typeof source['service'] === 'string' ? source['service'] : undefined,
    seen: typeof source['seen'] === 'boolean' ? source['seen'] : undefined,
    assigneePartyId: typeof source['assigneePartyId'] === 'number' ? source['assigneePartyId'] : undefined,
    customerPartyId: typeof source['customerPartyId'] === 'number' ? source['customerPartyId'] : undefined,
    from: typeof source['from'] === 'string' ? source['from'] : undefined,
    to: typeof source['to'] === 'string' ? source['to'] : undefined,
    minAmountMinor: typeof source['minAmountMinor'] === 'number' ? source['minAmountMinor'] : undefined,
    maxAmountMinor: typeof source['maxAmountMinor'] === 'number' ? source['maxAmountMinor'] : undefined,
  };
}

function WorkItemCard({
  item,
  selected,
  checked,
  onOpen,
  onCheck,
}: {
  item: OperationsWorkItem;
  selected: boolean;
  checked: boolean;
  onOpen: () => void;
  onCheck: (checked: boolean) => void;
}) {
  const { t } = useTranslation();
  const { locale, timezone } = useLocalePreferences();
  const title = locale.startsWith('en') ? item.titleEn : item.titleEs;
  const dueLabel = item.dueAt
    ? new Intl.DateTimeFormat(locale, { dateStyle: 'short', timeStyle: 'short', timeZone: timezone }).format(new Date(item.dueAt))
    : null;
  return (
    <Card
      variant="outlined"
      sx={{ borderColor: selected ? 'primary.main' : item.slaState === 'breached' ? 'error.main' : 'divider' }}
    >
      <CardContent sx={{ p: 1.5, '&:last-child': { pb: 1.5 } }}>
        <Stack direction="row" spacing={1} alignItems="flex-start">
          <Checkbox
            checked={checked}
            size="small"
            inputProps={{ 'aria-label': t('operations.selectItem', { title }) }}
            onChange={(event) => onCheck(event.target.checked)}
          />
          <CardActionArea
            onClick={onOpen}
            aria-label={`${title}. ${statusLabel(t, item.status)}`}
            sx={{ minWidth: 0, flex: 1, borderRadius: 1 }}
          >
            <Box sx={{ minWidth: 0, flex: 1 }}>
              <Stack direction="row" spacing={0.75} alignItems="center" sx={{ mb: 0.5 }}>
                <Chip size="small" color={PRIORITY_COLOR[item.priority]} label={priorityLabel(t, item.priority)} />
                {!item.seen && <Badge color="primary" variant="dot" aria-label={t('operations.unseen')} />}
                {item.slaState === 'breached' && (
                  <Tooltip title={t('operations.sla.breached')}>
                    <WarningAmberOutlinedIcon color="error" fontSize="small" />
                  </Tooltip>
                )}
              </Stack>
              <Typography variant="subtitle2" noWrap>{title}</Typography>
              <Typography variant="caption" color="text.secondary" noWrap>
                {item.entityType} · {item.sourceChannel}
              </Typography>
              {dueLabel && (
                <Typography variant="caption" display="block" color={item.slaState === 'breached' ? 'error' : 'text.secondary'}>
                  {t('operations.due')}: {dueLabel}
                </Typography>
              )}
            </Box>
          </CardActionArea>
        </Stack>
      </CardContent>
    </Card>
  );
}

function KpiCard({ label, value, critical = false }: { label: string; value: string | number; critical?: boolean }) {
  return (
    <Paper variant="outlined" sx={{ p: 2, minWidth: 0 }}>
      <Typography variant="caption" color="text.secondary">{label}</Typography>
      <Typography variant="h5" color={critical && Number(value) > 0 ? 'error.main' : 'text.primary'}>{value}</Typography>
    </Paper>
  );
}

export default function OperationsControlCenterPage() {
  const { t } = useTranslation();
  const { locale, currency: preferredCurrency } = useLocalePreferences();
  const queryClient = useQueryClient();
  const [filters, setFilters] = useState<OperationsFilters>(DEFAULT_FILTERS);
  const [filterDrawerOpen, setFilterDrawerOpen] = useState(false);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [checkedIds, setCheckedIds] = useState<Set<string>>(new Set());
  const [lastEventId, setLastEventId] = useState<number | undefined>();
  const [noteBody, setNoteBody] = useState('');
  const [transitionTarget, setTransitionTarget] = useState<OperationsStatus>('in_progress');
  const [transitionReason, setTransitionReason] = useState('');
  const [waitingExternal, setWaitingExternal] = useState(false);
  const [resumeAt, setResumeAt] = useState('');
  const [assigneeInput, setAssigneeInput] = useState('');
  const [teamInput, setTeamInput] = useState('');
  const [saveViewOpen, setSaveViewOpen] = useState(false);
  const [viewName, setViewName] = useState('');
  const [bulkTarget, setBulkTarget] = useState<OperationsStatus>('in_progress');
  const [bulkConfirmOpen, setBulkConfirmOpen] = useState(false);
  const [bulkReason, setBulkReason] = useState('');
  const [feedback, setFeedback] = useState<string | null>(null);
  const localizedTitle = (item: OperationsWorkItem) => locale.startsWith('en') ? item.titleEn : item.titleEs;
  const localizedDescription = (item: OperationsWorkItem) => locale.startsWith('en') ? item.descriptionEn : item.descriptionEs;

  const metricsQuery = useQuery({
    queryKey: ['operations', 'metrics'],
    queryFn: () => Operations.metrics(),
    refetchInterval: 30_000,
  });
  const workItemsQuery = useQuery({
    queryKey: ['operations', 'work-items', filters],
    queryFn: () => Operations.list(filters),
    placeholderData: (previous) => previous,
  });
  const detailQuery = useQuery({
    queryKey: ['operations', 'work-item', selectedId],
    queryFn: () => Operations.detail(selectedId ?? ''),
    enabled: selectedId !== null,
  });
  const savedViewsQuery = useQuery({
    queryKey: ['operations', 'saved-views'],
    queryFn: () => Operations.savedViews(),
  });
  const eventsQuery = useQuery({
    queryKey: ['operations', 'events', lastEventId],
    queryFn: () => Operations.events(lastEventId),
    refetchInterval: 2_000,
    retry: true,
  });

  useEffect(() => {
    const batch = eventsQuery.data;
    if (!batch || batch.events.length === 0) return;
    setLastEventId(batch.lastEventId ?? lastEventId);
    void queryClient.invalidateQueries({ queryKey: ['operations', 'work-items'] });
    void queryClient.invalidateQueries({ queryKey: ['operations', 'metrics'] });
    if (selectedId && batch.events.some((event) => event.workItemId === selectedId)) {
      void queryClient.invalidateQueries({ queryKey: ['operations', 'work-item', selectedId] });
    }
  }, [eventsQuery.data, lastEventId, queryClient, selectedId]);

  const refresh = () => {
    void workItemsQuery.refetch();
    void metricsQuery.refetch();
  };
  const afterMutation = async (item?: OperationsWorkItem) => {
    if (item) queryClient.setQueryData(['operations', 'work-item', item.id], (existing: unknown) => {
      if (!existing || typeof existing !== 'object') return existing;
      return { ...existing, workItem: item };
    });
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: ['operations', 'work-items'] }),
      queryClient.invalidateQueries({ queryKey: ['operations', 'metrics'] }),
      selectedId ? queryClient.invalidateQueries({ queryKey: ['operations', 'work-item', selectedId] }) : Promise.resolve(),
    ]);
  };

  const seenMutation = useMutation({ mutationFn: Operations.markSeen, onSuccess: afterMutation });
  const transitionMutation = useMutation({
    mutationFn: ({ item, target, reason }: { item: OperationsWorkItem; target: OperationsStatus; reason: string }) =>
      Operations.transition(item, target, reason, target === 'waiting' ? waitingExternal : undefined,
        target === 'waiting' && resumeAt ? new Date(resumeAt).toISOString() : null),
    onSuccess: async (item) => {
      setTransitionReason('');
      setFeedback(t('operations.updated'));
      await afterMutation(item);
    },
  });
  const assignmentMutation = useMutation({
    mutationFn: (item: OperationsWorkItem) => Operations.assign(
      item,
      assigneeInput.trim() ? Number(assigneeInput) : null,
      teamInput.trim() || null,
      transitionReason.trim() || t('operations.assignmentReason'),
    ),
    onSuccess: afterMutation,
  });
  const noteMutation = useMutation({
    mutationFn: (itemId: string) => Operations.addNote(itemId, noteBody),
    onSuccess: async () => {
      setNoteBody('');
      if (selectedId) await queryClient.invalidateQueries({ queryKey: ['operations', 'work-item', selectedId] });
    },
  });
  const saveViewMutation = useMutation({
    mutationFn: (organizationId: string) => Operations.saveView({
      organizationId,
      name: viewName.trim(),
      shared: false,
      filters: { ...filters },
      columns: ['priority', 'title', 'status', 'assignee', 'sla', 'source'],
      widgets: ['registrations', 'reservations', 'invoices', 'payments', 'sla', 'integrations'],
      subscribedEventTypes: [],
    }),
    onSuccess: async () => {
      setSaveViewOpen(false);
      setViewName('');
      await queryClient.invalidateQueries({ queryKey: ['operations', 'saved-views'] });
    },
  });

  const items = useMemo(() => workItemsQuery.data?.items ?? [], [workItemsQuery.data?.items]);
  const selected = detailQuery.data?.workItem ?? items.find((item) => item.id === selectedId) ?? null;
  const organizationId = selected?.organizationId ?? items[0]?.organizationId;
  const checkedItems = items.filter((item) => checkedIds.has(item.id));

  useEffect(() => {
    if (!selected || selected.seen || seenMutation.isPending) return;
    seenMutation.mutate(selected);
  // Marking shared seen state is intentionally tied to opening the detail drawer.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selected?.id]);

  const kanban = useMemo(() => new Map(KANBAN_STATUSES.map((status) => [
    status,
    items.filter((item) => item.status === status).sort((left, right) => PRIORITIES.indexOf(left.priority) - PRIORITIES.indexOf(right.priority)),
  ])), [items]);

  const metrics = metricsQuery.data;
  const pageError = metricsQuery.error ?? workItemsQuery.error;
  const currencyFormatter = new Intl.NumberFormat(locale, { style: 'currency', currency: metrics?.currency ?? preferredCurrency });
  const activeFilterCount = Object.entries(filters).filter(([key, value]) => key !== 'limit' && value !== undefined && value !== '').length;

  const executeBulk = async () => {
    const results = await Promise.allSettled(checkedItems.map((item) =>
      Operations.transition(item, bulkTarget, bulkReason.trim())));
    const succeeded = results.filter((result) => result.status === 'fulfilled').length;
    setFeedback(t('operations.bulkResult', { succeeded, total: results.length }));
    setCheckedIds(new Set());
    setBulkConfirmOpen(false);
    setBulkReason('');
    await afterMutation();
  };

  return (
    <PageShell
      title={t('operations.title')}
      subtitle={t('operations.subtitle')}
      maxWidth={false}
      actions={(
        <>
          <Button startIcon={<SaveOutlinedIcon />} onClick={() => setSaveViewOpen(true)} disabled={!organizationId}>
            {t('operations.saveView')}
          </Button>
          <Button
            startIcon={<FilterAltOutlinedIcon />}
            onClick={() => setFilterDrawerOpen(true)}
          >
            {t('operations.filters')} {activeFilterCount > 0 && `(${activeFilterCount})`}
          </Button>
          <Button startIcon={<RefreshOutlinedIcon />} onClick={refresh} disabled={workItemsQuery.isFetching}>
            {t('operations.refresh')}
          </Button>
        </>
      )}
    >
      <Stack spacing={3}>
        {pageError && (
          <Alert severity="error" action={<Button color="inherit" onClick={refresh}>{t('operations.retry')}</Button>}>
            {pageError.message}
          </Alert>
        )}
        {eventsQuery.isError && (
          <Alert severity="warning">{t('operations.realtimeDegraded')}</Alert>
        )}
        {feedback && <Alert severity="success" onClose={() => setFeedback(null)}>{feedback}</Alert>}

        <Box
          aria-label={t('operations.metrics')}
          sx={{ display: 'grid', gridTemplateColumns: { xs: 'repeat(2, minmax(0, 1fr))', md: 'repeat(4, minmax(0, 1fr))', xl: 'repeat(7, minmax(0, 1fr))' }, gap: 1.5 }}
        >
          <KpiCard label={t('operations.kpi.newRegistrations')} value={metrics?.newRegistrations ?? '—'} />
          <KpiCard label={t('operations.kpi.registrationAttention')} value={metrics?.registrationsRequiringAttention ?? '—'} />
          <KpiCard label={t('operations.kpi.reservations')} value={metrics?.reservationsAwaitingConfirmation ?? '—'} />
          <KpiCard label={t('operations.kpi.todaySessions')} value={metrics?.todaySessions ?? '—'} />
          <KpiCard label={t('operations.kpi.conflicts')} value={metrics?.schedulingConflicts ?? '—'} critical />
          <KpiCard label={t('operations.kpi.overdueInvoices')} value={metrics?.overdueInvoices ?? '—'} critical />
          <KpiCard label={t('operations.kpi.paymentVerification')} value={metrics?.paymentsAwaitingVerification ?? '—'} />
          <KpiCard label={t('operations.kpi.revenueToday')} value={metrics ? currencyFormatter.format(metrics.revenueReceivedTodayMinor / 100) : '—'} />
          <KpiCard label={t('operations.kpi.unassigned')} value={metrics?.unassignedWork ?? '—'} critical />
          <KpiCard label={t('operations.kpi.slaBreaches')} value={metrics?.slaBreaches ?? '—'} critical />
          <KpiCard label={t('operations.kpi.firstResponse')} value={metrics?.averageFirstResponseSeconds === null || metrics?.averageFirstResponseSeconds === undefined ? '—' : `${Math.round(metrics.averageFirstResponseSeconds / 60)} min`} />
          <KpiCard label={t('operations.kpi.resolution')} value={metrics?.averageResolutionSeconds === null || metrics?.averageResolutionSeconds === undefined ? '—' : `${Math.round(metrics.averageResolutionSeconds / 3600)} h`} />
          <KpiCard label={t('operations.kpi.integrationFailures')} value={metrics?.integrationFailures ?? '—'} critical />
          <KpiCard label={t('operations.kpi.unpaidInvoices')} value={metrics?.unpaidInvoices ?? '—'} />
        </Box>

        <Paper variant="outlined" sx={{ p: 2 }}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5} alignItems={{ md: 'center' }} sx={{ mb: 2 }}>
            <Typography variant="h5" sx={{ flex: 1 }}>{t('operations.kanban')}</Typography>
            <TextField
              size="small"
              label={t('operations.search')}
              value={filters.q ?? ''}
              onChange={(event) => setFilters((current) => ({ ...current, q: event.target.value || undefined }))}
              inputProps={{ 'aria-label': t('operations.search') }}
            />
            <FormControl size="small" sx={{ minWidth: 170 }}>
              <InputLabel>{t('operations.savedViews')}</InputLabel>
              <Select
                label={t('operations.savedViews')}
                inputProps={{ 'aria-label': t('operations.savedViews') }}
                value=""
                onChange={(event) => {
                  const view = savedViewsQuery.data?.find((candidate) => candidate.id === event.target.value);
                  if (view) setFilters(asFilters(view.filters));
                }}
              >
                {(savedViewsQuery.data ?? []).map((view) => <MenuItem key={view.id} value={view.id}>{view.name}</MenuItem>)}
              </Select>
            </FormControl>
          </Stack>
          {workItemsQuery.isLoading ? (
            <Stack alignItems="center" sx={{ py: 8 }} role="status" aria-label={t('operations.loading')}><CircularProgress /></Stack>
          ) : items.length === 0 ? (
            <EmptyState title={t('operations.empty')} description={t('operations.emptyDescription')} />
          ) : (
            <Box sx={{ display: 'grid', gridTemplateColumns: { xs: 'repeat(6, minmax(260px, 1fr))', xl: 'repeat(6, minmax(0, 1fr))' }, gap: 1.5, overflowX: 'auto', pb: 1 }}>
              {KANBAN_STATUSES.map((status) => (
                <Box key={status} component="section" aria-labelledby={`operations-column-${status}`} sx={{ minWidth: 260 }}>
                  <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 1 }}>
                    <Typography id={`operations-column-${status}`} variant="subtitle1">{statusLabel(t, status)}</Typography>
                    <Chip size="small" label={kanban.get(status)?.length ?? 0} />
                  </Stack>
                  <Stack spacing={1}>
                    {(kanban.get(status) ?? []).slice(0, 12).map((item) => (
                      <WorkItemCard
                        key={item.id}
                        item={item}
                        selected={item.id === selectedId}
                        checked={checkedIds.has(item.id)}
                        onOpen={() => setSelectedId(item.id)}
                        onCheck={(next) => setCheckedIds((current) => {
                          const updated = new Set(current);
                          if (next) updated.add(item.id); else updated.delete(item.id);
                          return updated;
                        })}
                      />
                    ))}
                  </Stack>
                </Box>
              ))}
            </Box>
          )}
        </Paper>

        <Paper variant="outlined">
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }} sx={{ p: 2 }}>
            <Typography variant="h5" sx={{ flex: 1 }}>{t('operations.inbox')}</Typography>
            <Typography variant="body2" color="text.secondary">{t('operations.itemsCount', { count: items.length })}</Typography>
            {checkedItems.length > 0 && (
              <>
                <FormControl size="small" sx={{ minWidth: 150 }}>
                  <InputLabel>{t('operations.bulkAction')}</InputLabel>
                  <Select label={t('operations.bulkAction')} value={bulkTarget} onChange={(event) => setBulkTarget(event.target.value as OperationsStatus)}>
                    {['in_progress', 'waiting', 'resolved', 'archived'].map((status) => (
                      <MenuItem key={status} value={status}>{statusLabel(t, status as OperationsStatus)}</MenuItem>
                    ))}
                  </Select>
                </FormControl>
                <Button variant="contained" onClick={() => setBulkConfirmOpen(true)}>{t('operations.previewImpact')}</Button>
              </>
            )}
          </Stack>
          <Divider />
          <List aria-label={t('operations.inbox')} disablePadding>
            {items.map((item) => (
              <ListItem
                key={item.id}
                disablePadding
                secondaryAction={<Chip size="small" color={PRIORITY_COLOR[item.priority]} label={priorityLabel(t, item.priority)} />}
              >
                <Checkbox
                  edge="start"
                  checked={checkedIds.has(item.id)}
                  onChange={(event) => setCheckedIds((current) => {
                    const inboxSelection = new Set(current);
                    if (event.target.checked) inboxSelection.add(item.id); else inboxSelection.delete(item.id);
                    return inboxSelection;
                  })}
                  inputProps={{ 'aria-label': t('operations.selectItem', { title: localizedTitle(item) }) }}
                  sx={{ ml: 1 }}
                />
                <ListItemButton selected={item.id === selectedId} onClick={() => setSelectedId(item.id)} sx={{ pr: 12 }}>
                  <ListItemText
                    primary={localizedTitle(item)}
                    secondary={`${statusLabel(t, item.status)} · ${item.entityType} · ${item.sourceChannel}${item.assigneePartyId ? ` · #${item.assigneePartyId}` : ''}`}
                  />
                </ListItemButton>
              </ListItem>
            ))}
          </List>
        </Paper>
      </Stack>

      <Drawer anchor="right" open={selectedId !== null} onClose={() => setSelectedId(null)} PaperProps={{ sx: { width: { xs: '100%', sm: 520 }, p: 2 } }}>
        {detailQuery.isLoading || !selected ? (
          <Stack alignItems="center" sx={{ pt: 8 }}><CircularProgress /></Stack>
        ) : detailQuery.error ? (
          <Alert severity="error">{detailQuery.error.message}</Alert>
        ) : (
          <Stack spacing={2}>
            <Box>
              <Stack direction="row" spacing={1} alignItems="center">
                <Chip size="small" color={PRIORITY_COLOR[selected.priority]} label={priorityLabel(t, selected.priority)} />
                <Chip size="small" label={statusLabel(t, selected.status)} />
                <Chip size="small" variant="outlined" label={t(`operations.sla.${selected.slaState}`)} />
              </Stack>
              <Typography variant="h5" sx={{ mt: 1 }}>{localizedTitle(selected)}</Typography>
              <Typography color="text.secondary">{localizedDescription(selected)}</Typography>
            </Box>
            {detailQuery.data?.sourceRecordUrl && (
              <Button component={RouterLink} to={detailQuery.data.sourceRecordUrl} startIcon={<OpenInNewOutlinedIcon />}>
                {t('operations.openSource')}
              </Button>
            )}
            <Divider />
            <Typography variant="h6">{t('operations.safeActions')}</Typography>
            <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
              {(detailQuery.data?.quickActions ?? []).map((action) => <Chip key={action} label={t(`operations.action.${action}`, { defaultValue: action })} variant="outlined" />)}
            </Stack>
            <FormControl fullWidth>
              <InputLabel>{t('operations.transition')}</InputLabel>
              <Select label={t('operations.transition')} value={transitionTarget} onChange={(event) => setTransitionTarget(event.target.value as OperationsStatus)}>
                {(detailQuery.data?.allowedTransitions ?? []).map((status) => <MenuItem key={status} value={status}>{statusLabel(t, status)}</MenuItem>)}
              </Select>
            </FormControl>
            {transitionTarget === 'waiting' && (
              <>
                <FormControlLabel control={<Checkbox checked={waitingExternal} onChange={(event) => setWaitingExternal(event.target.checked)} />} label={t('operations.externalDependency')} />
                <TextField type="datetime-local" label={t('operations.resumeAt')} value={resumeAt} onChange={(event) => setResumeAt(event.target.value)} InputLabelProps={{ shrink: true }} />
              </>
            )}
            <TextField required label={t('operations.reason')} value={transitionReason} onChange={(event) => setTransitionReason(event.target.value)} multiline minRows={2} />
            <Button
              variant="contained"
              disabled={!transitionReason.trim() || transitionMutation.isPending}
              onClick={() => transitionMutation.mutate({ item: selected, target: transitionTarget, reason: transitionReason.trim() })}
            >
              {t('operations.applyTransition')}
            </Button>
            <Divider />
            <Typography variant="h6">{t('operations.assignment')}</Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <TextField label={t('operations.assigneeId')} value={assigneeInput} onChange={(event) => setAssigneeInput(event.target.value.replace(/\D/g, ''))} />
              <TextField label={t('operations.team')} value={teamInput} onChange={(event) => setTeamInput(event.target.value)} />
            </Stack>
            <Button startIcon={<AssignmentIndOutlinedIcon />} onClick={() => assignmentMutation.mutate(selected)} disabled={assignmentMutation.isPending}>
              {t('operations.assign')}
            </Button>
            <Divider />
            <Typography variant="h6">{t('operations.thread')}</Typography>
            <Tabs value={0} aria-label={t('operations.thread')}><Tab label={t('operations.history')} /></Tabs>
            <List dense>
              {(detailQuery.data?.events ?? []).map((event) => (
                <ListItem key={event.id} alignItems="flex-start">
                  <ListItemText primary={event.bodyEs} secondary={`${event.eventType} · ${new Date(event.occurredAt).toLocaleString(locale)}`} />
                </ListItem>
              ))}
              {(detailQuery.data?.notes ?? []).map((note) => (
                <ListItem key={note.id} alignItems="flex-start">
                  <ListItemText primary={note.body} secondary={`${t('operations.internalNote')} · #${note.authorPartyId}`} />
                </ListItem>
              ))}
            </List>
            <TextField label={t('operations.addNote')} multiline minRows={3} value={noteBody} onChange={(event) => setNoteBody(event.target.value)} />
            <Button onClick={() => noteMutation.mutate(selected.id)} disabled={!noteBody.trim() || noteMutation.isPending}>{t('operations.saveNote')}</Button>
          </Stack>
        )}
      </Drawer>

      <Drawer anchor="left" open={filterDrawerOpen} onClose={() => setFilterDrawerOpen(false)} PaperProps={{ sx: { width: { xs: '100%', sm: 380 }, p: 2 } }}>
        <Stack spacing={2}>
          <Typography variant="h5">{t('operations.filters')}</Typography>
          <TextField label={t('operations.domain')} value={filters.entityType ?? ''} onChange={(event) => setFilters((current) => ({ ...current, entityType: event.target.value || undefined }))} />
          <TextField label={t('operations.sourceChannel')} value={filters.sourceChannel ?? ''} onChange={(event) => setFilters((current) => ({ ...current, sourceChannel: event.target.value || undefined }))} />
          <TextField label={t('operations.service')} value={filters.service ?? ''} onChange={(event) => setFilters((current) => ({ ...current, service: event.target.value || undefined }))} />
          <FormControl><InputLabel>{t('operations.statusLabel')}</InputLabel><Select label={t('operations.statusLabel')} value={filters.status ?? ''} onChange={(event) => setFilters((current) => ({ ...current, status: (event.target.value || undefined) as OperationsStatus | undefined }))}><MenuItem value="">{t('operations.all')}</MenuItem>{STATUS_ORDER.map((status) => <MenuItem key={status} value={status}>{statusLabel(t, status)}</MenuItem>)}</Select></FormControl>
          <FormControl><InputLabel>{t('operations.priorityLabel')}</InputLabel><Select label={t('operations.priorityLabel')} value={filters.priority ?? ''} onChange={(event) => setFilters((current) => ({ ...current, priority: (event.target.value || undefined) as OperationsPriority | undefined }))}><MenuItem value="">{t('operations.all')}</MenuItem>{PRIORITIES.map((priority) => <MenuItem key={priority} value={priority}>{priorityLabel(t, priority)}</MenuItem>)}</Select></FormControl>
          <FormControl><InputLabel>{t('operations.slaLabel')}</InputLabel><Select label={t('operations.slaLabel')} value={filters.slaState ?? ''} onChange={(event) => setFilters((current) => ({ ...current, slaState: (event.target.value || undefined) as OperationsFilters['slaState'] }))}><MenuItem value="">{t('operations.all')}</MenuItem>{['on_track', 'at_risk', 'due', 'breached', 'paused'].map((sla) => <MenuItem key={sla} value={sla}>{t(`operations.sla.${sla}`)}</MenuItem>)}</Select></FormControl>
          <TextField label={t('operations.assigneeId')} value={filters.assigneePartyId ?? ''} onChange={(event) => setFilters((current) => ({ ...current, assigneePartyId: event.target.value ? Number(event.target.value) : undefined }))} />
          <TextField label={t('operations.customerId')} value={filters.customerPartyId ?? ''} onChange={(event) => setFilters((current) => ({ ...current, customerPartyId: event.target.value ? Number(event.target.value) : undefined }))} />
          <TextField label={t('operations.paymentState')} value={filters.paymentState ?? ''} onChange={(event) => setFilters((current) => ({ ...current, paymentState: event.target.value || undefined }))} />
          <TextField type="date" label={t('operations.from')} value={filters.from?.slice(0, 10) ?? ''} onChange={(event) => setFilters((current) => ({ ...current, from: event.target.value ? new Date(`${event.target.value}T00:00:00`).toISOString() : undefined }))} InputLabelProps={{ shrink: true }} />
          <TextField type="date" label={t('operations.to')} value={filters.to?.slice(0, 10) ?? ''} onChange={(event) => setFilters((current) => ({ ...current, to: event.target.value ? new Date(`${event.target.value}T23:59:59`).toISOString() : undefined }))} InputLabelProps={{ shrink: true }} />
          <Stack direction="row" spacing={1}>
            <TextField label={t('operations.minAmount')} value={filters.minAmountMinor ? filters.minAmountMinor / 100 : ''} onChange={(event) => setFilters((current) => ({ ...current, minAmountMinor: event.target.value ? Math.round(Number(event.target.value) * 100) : undefined }))} />
            <TextField label={t('operations.maxAmount')} value={filters.maxAmountMinor ? filters.maxAmountMinor / 100 : ''} onChange={(event) => setFilters((current) => ({ ...current, maxAmountMinor: event.target.value ? Math.round(Number(event.target.value) * 100) : undefined }))} />
          </Stack>
          <FormControl><InputLabel>{t('operations.seenLabel')}</InputLabel><Select label={t('operations.seenLabel')} value={filters.seen === undefined ? '' : String(filters.seen)} onChange={(event) => setFilters((current) => ({ ...current, seen: event.target.value === '' ? undefined : event.target.value === 'true' }))}><MenuItem value="">{t('operations.all')}</MenuItem><MenuItem value="false">{t('operations.unseen')}</MenuItem><MenuItem value="true">{t('operations.seen')}</MenuItem></Select></FormControl>
          <Stack direction="row" spacing={1}>
            <Button onClick={() => setFilters(DEFAULT_FILTERS)}>{t('operations.clear')}</Button>
            <Button variant="contained" onClick={() => setFilterDrawerOpen(false)}>{t('operations.applyFilters')}</Button>
          </Stack>
        </Stack>
      </Drawer>

      <Dialog open={saveViewOpen} onClose={() => setSaveViewOpen(false)}>
        <DialogTitle>{t('operations.saveView')}</DialogTitle>
        <DialogContent><TextField fullWidth margin="dense" label={t('operations.viewName')} value={viewName} onChange={(event) => setViewName(event.target.value)} /></DialogContent>
        <DialogActions><Button onClick={() => setSaveViewOpen(false)}>{t('operations.cancel')}</Button><Button variant="contained" disabled={!viewName.trim() || !organizationId || saveViewMutation.isPending} onClick={() => organizationId && saveViewMutation.mutate(organizationId)}>{t('operations.save')}</Button></DialogActions>
      </Dialog>

      <Dialog open={bulkConfirmOpen} onClose={() => setBulkConfirmOpen(false)}>
        <DialogTitle>{t('operations.bulkPreviewTitle')}</DialogTitle>
        <DialogContent>
          <DialogContentText>{t('operations.bulkPreviewBody', { count: checkedItems.length, status: statusLabel(t, bulkTarget) })}</DialogContentText>
          <Alert severity="warning" sx={{ my: 2 }}>{t('operations.bulkSourceInvariant')}</Alert>
          <TextField fullWidth required multiline minRows={2} label={t('operations.reason')} value={bulkReason} onChange={(event) => setBulkReason(event.target.value)} />
        </DialogContent>
        <DialogActions><Button onClick={() => setBulkConfirmOpen(false)}>{t('operations.cancel')}</Button><Button color={bulkTarget === 'archived' ? 'error' : 'primary'} variant="contained" disabled={!bulkReason.trim()} onClick={() => void executeBulk()}>{t('operations.confirmBulk')}</Button></DialogActions>
      </Dialog>
    </PageShell>
  );
}
