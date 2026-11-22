// TAX-GENRE-SUGGESTION-001: administrators settle the genre suggestions the
// automatic checks could not (new words, near matches). Approving publishes a
// genre with the reviewed names and adds it to the suggesting artist's
// profile; merging points the artist at an existing genre; rejecting tells the
// artist why.
import { useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  MenuItem,
  Stack,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Taxonomy, type GenreSuggestion, type GenreSuggestionQueueStatus } from '../api/taxonomy';
import { genreOptions, resolveTaxonomyLanguage, taxonomyName, type TaxonomyOption } from '../features/taxonomyFilters/taxonomyFilterModel';
import { TAXONOMY_OPTIONS_QUERY_KEY, useTaxonomyFilterOptions } from '../features/taxonomyFilters/useTaxonomyFilters';
import { useDocumentTitle } from '../hooks/useDocumentTitle';

const STATUSES: GenreSuggestionQueueStatus[] = ['pending', 'approved', 'rejected', 'merged', 'all'];
const STATUS_LABEL_KEYS: Record<GenreSuggestionQueueStatus, string> = {
  pending: 'taxonomyFilters.admin.statusPending',
  approved: 'taxonomyFilters.admin.statusApproved',
  rejected: 'taxonomyFilters.admin.statusRejected',
  merged: 'taxonomyFilters.admin.statusMerged',
  all: 'taxonomyFilters.admin.statusAll',
};

function SuggestionCard({ suggestion, genres }: { suggestion: GenreSuggestion; genres: TaxonomyOption[] }) {
  const { t, i18n } = useTranslation();
  const language = resolveTaxonomyLanguage(i18n.resolvedLanguage ?? i18n.language);
  const queryClient = useQueryClient();
  const [nameEs, setNameEs] = useState(suggestion.nameEs);
  const [nameEn, setNameEn] = useState(suggestion.nameEn);
  const [parentId, setParentId] = useState(suggestion.parentGenreId ?? '');
  const [note, setNote] = useState('');
  const [mergeTarget, setMergeTarget] = useState<TaxonomyOption | null>(null);
  const [mode, setMode] = useState<'approve' | 'reject' | 'merge'>('approve');
  const [error, setError] = useState<string | null>(null);
  const pending = suggestion.status === 'pending';

  const settle = useMutation({
    mutationFn: async () => {
      if (mode === 'approve') {
        return Taxonomy.approveGenreSuggestion(suggestion.id, { approvedNameEs: nameEs, approvedNameEn: nameEn, approvedParentGenreId: parentId || null });
      }
      if (mode === 'reject') return Taxonomy.rejectGenreSuggestion(suggestion.id, note);
      return Taxonomy.mergeGenreSuggestion(suggestion.id, mergeTarget!.id);
    },
    onSuccess: async () => {
      setError(null);
      await queryClient.invalidateQueries({ queryKey: ['genre-suggestions'] });
      await queryClient.invalidateQueries({ queryKey: TAXONOMY_OPTIONS_QUERY_KEY });
    },
    onError: (failure: Error) => setError(t('taxonomyFilters.admin.failed', { message: failure.message })),
  });

  const reasons = suggestion.reviewReasons.map((reason) => {
    if (reason === 'similar_to_existing') return t('taxonomyFilters.admin.reasonSimilar');
    if (reason.startsWith('unknown_word:')) return t('taxonomyFilters.admin.reasonUnknownWord', { word: reason.slice('unknown_word:'.length) });
    return reason;
  });
  const createdAt = new Date(suggestion.createdAt);
  const canSubmit = mode === 'approve'
    ? nameEs.trim().length >= 2 && nameEn.trim().length >= 2
    : mode === 'reject' ? note.trim().length > 0 && note.length <= 500 : mergeTarget !== null;

  return (
    <Card variant="outlined" component="article" aria-label={suggestion.nameEs} data-testid="genre-suggestion-card">
      <CardContent>
        <Stack spacing={1.5}>
          <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
            <Typography component="h2" variant="h6">{taxonomyName(suggestion, language)}</Typography>
            <Chip size="small" label={t(STATUS_LABEL_KEYS[suggestion.status])} color={pending ? 'warning' : 'default'} />
            {suggestion.automatic && <Chip size="small" variant="outlined" label={t('taxonomyFilters.admin.automatic')} />}
          </Stack>
          <Typography variant="body2" color="text.secondary">
            {t('taxonomyFilters.admin.suggestedBy', {
              name: suggestion.suggestedBy.displayName ?? `#${suggestion.suggestedBy.partyId}`,
              date: Number.isNaN(createdAt.getTime()) ? suggestion.createdAt : createdAt.toLocaleDateString(language),
            })}
            {' · '}«{suggestion.submittedName}»
          </Typography>
          {reasons.length > 0 && (
            <Typography variant="body2"><strong>{t('taxonomyFilters.admin.reasons')}:</strong> {reasons.join(' · ')}</Typography>
          )}
          {suggestion.resolvedGenre && (
            <Typography variant="body2">{t('taxonomyFilters.admin.resolvedAs', { name: taxonomyName(suggestion.resolvedGenre, language) })}</Typography>
          )}
          {suggestion.reviewNote && <Typography variant="body2" color="text.secondary">{suggestion.reviewNote}</Typography>}

          {pending && (
            <>
              <ToggleButtonGroup exclusive size="small" value={mode} onChange={(_event, next: 'approve' | 'reject' | 'merge' | null) => { if (next) setMode(next); }}>
                <ToggleButton value="approve">{t('taxonomyFilters.admin.approve')}</ToggleButton>
                <ToggleButton value="merge">{t('taxonomyFilters.admin.merge')}</ToggleButton>
                <ToggleButton value="reject">{t('taxonomyFilters.admin.reject')}</ToggleButton>
              </ToggleButtonGroup>
              {mode === 'approve' && (
                <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', md: 'repeat(3, minmax(0, 1fr))' } }}>
                  <TextField size="small" label={t('taxonomyFilters.admin.nameEs')} value={nameEs} onChange={(event) => setNameEs(event.target.value)} inputProps={{ maxLength: 60 }} />
                  <TextField size="small" label={t('taxonomyFilters.admin.nameEn')} value={nameEn} onChange={(event) => setNameEn(event.target.value)} inputProps={{ maxLength: 60 }} />
                  <TextField select size="small" label={t('taxonomyFilters.admin.parent')} value={parentId} onChange={(event) => setParentId(event.target.value)}>
                    <MenuItem value="">{t('taxonomyFilters.suggest.noParent')}</MenuItem>
                    {genres.map((genre) => <MenuItem key={genre.id} value={genre.id} sx={{ pl: 2 + genre.depth * 2 }}>{genre.label}</MenuItem>)}
                  </TextField>
                </Box>
              )}
              {mode === 'merge' && (
                <Autocomplete
                  size="small"
                  options={genres}
                  value={mergeTarget}
                  onChange={(_event, next) => setMergeTarget(next)}
                  getOptionLabel={(option) => option.label}
                  isOptionEqualToValue={(option, value) => option.id === value.id}
                  renderInput={(params) => <TextField {...params} label={t('taxonomyFilters.admin.mergeTarget')} />}
                />
              )}
              {mode === 'reject' && (
                <TextField size="small" multiline minRows={2} label={t('taxonomyFilters.admin.rejectNote')} value={note} onChange={(event) => setNote(event.target.value)} inputProps={{ maxLength: 500 }} />
              )}
              <Box>
                <Button variant="contained" onClick={() => settle.mutate()} disabled={!canSubmit || settle.isPending} color={mode === 'reject' ? 'error' : 'primary'}>
                  {mode === 'approve' ? t('taxonomyFilters.admin.approve') : mode === 'merge' ? t('taxonomyFilters.admin.confirmMerge') : t('taxonomyFilters.admin.reject')}
                </Button>
              </Box>
            </>
          )}
          {error && <Alert severity="error">{error}</Alert>}
        </Stack>
      </CardContent>
    </Card>
  );
}

export default function GenreSuggestionsAdminPage() {
  const { t, i18n } = useTranslation();
  useDocumentTitle(t('taxonomyFilters.admin.title'));
  const language = resolveTaxonomyLanguage(i18n.resolvedLanguage ?? i18n.language);
  const [status, setStatus] = useState<GenreSuggestionQueueStatus>('pending');
  const options = useTaxonomyFilterOptions();
  const genres = useMemo(() => genreOptions(options.data?.genres ?? [], language), [language, options.data?.genres]);
  const queue = useQuery({
    queryKey: ['genre-suggestions', 'queue', status],
    queryFn: () => Taxonomy.genreSuggestionQueue(status),
    retry: false,
  });

  return (
    <Stack spacing={2} sx={{ maxWidth: 960, mx: 'auto', px: 2, py: 3 }}>
      <Box>
        <Typography component="h1" variant="h4" fontWeight={800}>{t('taxonomyFilters.admin.title')}</Typography>
        <Typography color="text.secondary">{t('taxonomyFilters.admin.intro')}</Typography>
      </Box>
      <TextField select size="small" label={t('taxonomyFilters.admin.status')} value={status} onChange={(event) => setStatus(event.target.value as GenreSuggestionQueueStatus)} sx={{ maxWidth: 240 }}>
        {STATUSES.map((value) => <MenuItem key={value} value={value}>{t(STATUS_LABEL_KEYS[value])}</MenuItem>)}
      </TextField>
      {queue.isLoading && <CircularProgress aria-label={t('taxonomyFilters.admin.title')} />}
      {queue.isError && <Alert severity="error" action={<Button onClick={() => void queue.refetch()}>↻</Button>}>{queue.error instanceof Error ? queue.error.message : ''}</Alert>}
      {queue.data?.length === 0 && <Alert severity="info">{t('taxonomyFilters.admin.empty')}</Alert>}
      {(queue.data ?? []).map((suggestion) => <SuggestionCard key={suggestion.id} suggestion={suggestion} genres={genres} />)}
    </Stack>
  );
}
