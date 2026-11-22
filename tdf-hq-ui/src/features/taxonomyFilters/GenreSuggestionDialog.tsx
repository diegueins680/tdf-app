// TAX-GENRE-SUGGESTION-001: an artist proposes a genre in Spanish and
// English. "Check" shows the cleaned, cased names and the verdict before
// anything is stored: a duplicate offers the existing genre, a probable typo
// offers the existing name (or "it is different", which sends it to review),
// and a name made of known words is published on submit.
import { useMemo, useState } from 'react';
import {
  Alert,
  Button,
  Checkbox,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Taxonomy, type GenreSuggestion, type GenreSuggestionCheck } from '../../api/taxonomy';
import { genreOptions, resolveTaxonomyLanguage, taxonomyName } from './taxonomyFilterModel';
import { TAXONOMY_OPTIONS_QUERY_KEY, useTaxonomyFilterOptions } from './useTaxonomyFilters';

export const MY_GENRE_SUGGESTIONS_QUERY_KEY = ['genre-suggestions', 'mine'] as const;

interface DialogProps {
  open: boolean;
  onClose: () => void;
  /** Called with the genre the artist should now have (new, or an existing one they chose). */
  onGenreReady: (genreId: string) => void;
}

export function GenreSuggestionDialog({ open, onClose, onGenreReady }: DialogProps) {
  const { t, i18n } = useTranslation();
  const language = resolveTaxonomyLanguage(i18n.resolvedLanguage ?? i18n.language);
  const queryClient = useQueryClient();
  const options = useTaxonomyFilterOptions();
  const [nameEs, setNameEs] = useState('');
  const [nameEn, setNameEn] = useState('');
  const [sameName, setSameName] = useState(true);
  const [parentGenreId, setParentGenreId] = useState('');
  const [check, setCheck] = useState<GenreSuggestionCheck | null>(null);
  const [result, setResult] = useState<{ kind: 'success' | 'info'; message: string } | null>(null);
  const [error, setError] = useState<string | null>(null);

  const effectiveEn = sameName ? nameEs : nameEn;
  const request = { nameEs, nameEn: effectiveEn, parentGenreId: parentGenreId || null };
  const parents = useMemo(() => genreOptions(options.data?.genres ?? [], language), [language, options.data?.genres]);
  const resetCheck = () => { setCheck(null); setResult(null); setError(null); };

  const checkMutation = useMutation({
    mutationFn: () => Taxonomy.checkGenreSuggestion(request),
    onSuccess: (value) => { setCheck(value); setError(null); },
    onError: (failure: Error) => setError(failure.message),
  });
  const submitMutation = useMutation({
    mutationFn: (confirmDistinct: boolean) => Taxonomy.submitGenreSuggestion({ ...request, confirmDistinct }),
    onSuccess: async (submission) => {
      await queryClient.invalidateQueries({ queryKey: MY_GENRE_SUGGESTIONS_QUERY_KEY });
      const suggestion = submission.suggestion;
      if (suggestion.status === 'approved' && suggestion.resolvedGenre) {
        await queryClient.invalidateQueries({ queryKey: TAXONOMY_OPTIONS_QUERY_KEY });
        await queryClient.invalidateQueries({ queryKey: ['catalog', 'public', 'genres'] });
        onGenreReady(suggestion.resolvedGenre.id);
        setResult({ kind: 'success', message: t('taxonomyFilters.suggest.approved', { name: taxonomyName(suggestion.resolvedGenre, language) }) });
      } else {
        setResult({ kind: 'info', message: t(submission.alreadySuggested ? 'taxonomyFilters.suggest.alreadySuggested' : 'taxonomyFilters.suggest.pendingSent') });
      }
      setCheck(null);
    },
    onError: () => setError(t('taxonomyFilters.suggest.failed')),
  });

  const close = () => {
    setNameEs(''); setNameEn(''); setSameName(true); setParentGenreId('');
    resetCheck();
    onClose();
  };
  const chooseExisting = (genreId: string) => { onGenreReady(genreId); close(); };
  const busy = checkMutation.isPending || submitMutation.isPending;
  const matchName = check?.match ? taxonomyName(check.match, language) : '';

  return (
    <Dialog open={open} onClose={close} fullWidth maxWidth="sm" aria-labelledby="genre-suggestion-title">
      <DialogTitle id="genre-suggestion-title">{t('taxonomyFilters.suggest.title')}</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ pt: 1 }}>
          <Typography variant="body2" color="text.secondary">{t('taxonomyFilters.suggest.intro')}</Typography>
          <TextField
            label={t('taxonomyFilters.suggest.nameEs')}
            value={nameEs}
            onChange={(event) => { setNameEs(event.target.value); resetCheck(); }}
            inputProps={{ maxLength: 60 }}
            required
          />
          <FormControlLabel
            control={<Checkbox checked={sameName} onChange={(event) => { setSameName(event.target.checked); if (!event.target.checked && !nameEn) setNameEn(nameEs); resetCheck(); }} />}
            label={t('taxonomyFilters.suggest.sameName')}
          />
          {!sameName && (
            <TextField
              label={t('taxonomyFilters.suggest.nameEn')}
              value={nameEn}
              onChange={(event) => { setNameEn(event.target.value); resetCheck(); }}
              inputProps={{ maxLength: 60 }}
              required
            />
          )}
          <TextField
            select
            label={t('taxonomyFilters.suggest.parent')}
            value={parentGenreId}
            onChange={(event) => { setParentGenreId(event.target.value); resetCheck(); }}
          >
            <MenuItem value="">{t('taxonomyFilters.suggest.noParent')}</MenuItem>
            {parents.map((parent) => (
              <MenuItem key={parent.id} value={parent.id} sx={{ pl: 2 + parent.depth * 2 }}>{parent.label}</MenuItem>
            ))}
          </TextField>

          {check && (
            <Stack spacing={1} role="status" aria-live="polite">
              <Typography variant="body2">{t('taxonomyFilters.suggest.willStoreAs', { es: check.nameEs, en: check.nameEn })}</Typography>
              {check.corrections.map((correction) => (
                <Typography key={correction.word} variant="body2" color="warning.main">
                  {t('taxonomyFilters.suggest.correction', { word: correction.word, suggestion: correction.suggestion })}
                </Typography>
              ))}
              {check.verdict === 'duplicate' && <Alert severity="info">{t('taxonomyFilters.suggest.duplicate', { name: matchName })}</Alert>}
              {check.verdict === 'similar' && <Alert severity="warning">{t('taxonomyFilters.suggest.similar', { name: matchName })}</Alert>}
              {check.verdict === 'review' && <Alert severity="info">{t('taxonomyFilters.suggest.review')}</Alert>}
              {check.verdict === 'approve' && <Alert severity="success">{t('taxonomyFilters.suggest.approve')}</Alert>}
            </Stack>
          )}
          {result && <Alert severity={result.kind} role="status">{result.message}</Alert>}
          {error && <Alert severity="error">{error}</Alert>}
        </Stack>
      </DialogContent>
      <DialogActions sx={{ flexWrap: 'wrap', gap: 1 }}>
        <Button onClick={close}>{result ? t('taxonomyFilters.suggest.close') : t('taxonomyFilters.suggest.cancel')}</Button>
        {!result && !check && (
          <Button variant="contained" onClick={() => checkMutation.mutate()} disabled={busy || nameEs.trim().length < 2 || effectiveEn.trim().length < 2}>
            {checkMutation.isPending ? t('taxonomyFilters.suggest.checking') : t('taxonomyFilters.suggest.check')}
          </Button>
        )}
        {check?.match && (check.verdict === 'duplicate' || check.verdict === 'similar') && (
          <Button variant="contained" onClick={() => chooseExisting(check.match!.id)}>{t('taxonomyFilters.suggest.useExisting', { name: matchName })}</Button>
        )}
        {check?.verdict === 'similar' && (
          <Button onClick={() => submitMutation.mutate(true)} disabled={busy}>{t('taxonomyFilters.suggest.submitAnyway')}</Button>
        )}
        {(check?.verdict === 'review' || check?.verdict === 'approve') && (
          <Button variant="contained" onClick={() => submitMutation.mutate(false)} disabled={busy}>{t('taxonomyFilters.suggest.submit')}</Button>
        )}
      </DialogActions>
    </Dialog>
  );
}

/** The artist's suggestions still under review (shown as pending) and recent rejections. */
export function PendingGenreSuggestions({ enabled }: { enabled: boolean }) {
  const { t, i18n } = useTranslation();
  const language = resolveTaxonomyLanguage(i18n.resolvedLanguage ?? i18n.language);
  const mine = useQuery<GenreSuggestion[]>({
    queryKey: MY_GENRE_SUGGESTIONS_QUERY_KEY,
    queryFn: () => Taxonomy.myGenreSuggestions(),
    enabled,
    staleTime: 60_000,
    retry: false,
  });
  const pending = (mine.data ?? []).filter((suggestion) => suggestion.status === 'pending');
  const rejected = (mine.data ?? []).filter((suggestion) => suggestion.status === 'rejected').slice(0, 3);
  if (!pending.length && !rejected.length) return null;
  const nameOf = (suggestion: GenreSuggestion) => taxonomyName(suggestion, language);
  return (
    <Stack spacing={0.5} data-testid="pending-genre-suggestions">
      {pending.length > 0 && (
        <>
          <Typography variant="caption" color="text.secondary">{t('taxonomyFilters.suggest.pendingListTitle')}</Typography>
          <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
            {pending.map((suggestion) => (
              <Chip key={suggestion.id} size="small" variant="outlined" label={t('taxonomyFilters.suggest.pendingChip', { name: nameOf(suggestion) })} />
            ))}
          </Stack>
        </>
      )}
      {rejected.map((suggestion) => (
        <Typography key={suggestion.id} variant="caption" color="text.secondary">
          {t('taxonomyFilters.suggest.rejected', { name: nameOf(suggestion), note: suggestion.reviewNote ?? '' })}
        </Typography>
      ))}
    </Stack>
  );
}
