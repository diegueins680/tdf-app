import { useState } from 'react';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink } from 'react-router-dom';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Chip,
  Collapse,
  Divider,
  FormControl,
  InputLabel,
  MenuItem,
  Paper,
  Rating,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import GavelIcon from '@mui/icons-material/Gavel';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';

import MarkdownText from '../../components/MarkdownText';
import { Directory, type ClassifiedApplication, type DirectoryTaxonomyItem } from '../../api/directory';
import {
  OFFER_STATUS_LABEL_KEYS,
  canAccept,
  canCounter,
  canReject,
  canWithdraw,
  formatMinor,
  moneyInputToMinor,
  waitingOn,
  describeTerms,
} from './offers';

interface OfferCardProps {
  offer: ClassifiedApplication;
  currencies?: DirectoryTaxonomyItem[];
  /** The author's own profile id, needed to open the conversation. */
  authorProfileId?: string;
  /** Whether to show the classified title (the offerer's "my offers" list). */
  showClassified?: boolean;
  onChanged?: () => void;
}

/**
 * One offer with its negotiation chain and the actions the current
 * participant may take (ADR 0134). The same card serves the author's
 * comparison board and the offerer's "my offers" list; the role comes from
 * the API (`participantRole`), never from the client.
 */
export default function OfferCard({ offer, currencies, authorProfileId: authorProfileIdProp, showClassified = false, onChanged }: OfferCardProps) {
  const { t, i18n } = useTranslation();
  const authorProfileId = authorProfileIdProp ?? offer.classified?.authorProfileId;
  const client = useQueryClient();
  const actor = offer.participantRole === 'applicant' ? 'applicant' : 'author';
  const [countering, setCountering] = useState(false);
  const [showRounds, setShowRounds] = useState(false);
  const [counterAmount, setCounterAmount] = useState('');
  const [counterCurrency, setCounterCurrency] = useState(offer.currencyId ?? offer.classified?.budget?.currencyId ?? '');
  const [counterMessage, setCounterMessage] = useState('');
  const [conversationMessage, setConversationMessage] = useState(t('marketplace.offer.defaultConversation'));

  const refresh = async () => {
    await Promise.all([
      client.invalidateQueries({ queryKey: ['directory', 'applications'] }),
      client.invalidateQueries({ queryKey: ['directory', 'my-offers'] }),
      client.invalidateQueries({ queryKey: ['directory', 'managed-classifieds'] }),
    ]);
    onChanged?.();
  };
  const transition = useMutation({ mutationFn: (status: string) => Directory.setApplicationStatus(offer.id, status), onSuccess: refresh });
  const counter = useMutation({
    mutationFn: () => {
      const minorUnits = currencies?.find((item) => item.id === counterCurrency)?.minorUnits ?? 2;
      const amountMinor = moneyInputToMinor(counterAmount, minorUnits);
      if (amountMinor === undefined) throw new Error(t('marketplace.offer.invalidAmount'));
      if (!counterCurrency) throw new Error(t('marketplace.offer.currencyRequired'));
      return Directory.counterOffer(offer.id, { amountMinor, currencyId: counterCurrency, message: counterMessage.trim() || undefined });
    },
    onSuccess: async () => { setCountering(false); setCounterAmount(''); setCounterMessage(''); await refresh(); },
  });
  const conversation = useMutation({
    mutationFn: async () => {
      const senderProfileId = actor === 'author' ? authorProfileId : offer.applicantProfileId;
      const targetProfileId = actor === 'author' ? offer.applicantProfileId : authorProfileId;
      if (!senderProfileId || !targetProfileId) throw new Error(t('marketplace.offer.conversationUnavailable'));
      await Directory.contact({ senderProfileId, targetProfileId, contextKind: 'application', contextId: offer.id, message: conversationMessage.trim() }, `directory-application-contact-${offer.id}`);
      return Directory.setApplicationStatus(offer.id, 'conversation_open');
    },
    onSuccess: refresh,
  });

  const currentAmount = formatMinor(offer.currentAmountMinor ?? offer.proposedAmountMinor, offer.currencyId, currencies, i18n.language);
  const acceptedAmount = formatMinor(offer.acceptedAmountMinor, offer.currencyId, currencies, i18n.language);
  const priceMode = offer.classified?.priceMode;
  const turn = waitingOn(offer.status);
  const myTurn = turn === actor;
  const error = transition.error ?? counter.error ?? conversation.error;
  const statusColor = offer.status === 'accepted' || offer.status === 'conversation_open' || offer.status === 'converted' ? 'success'
    : offer.status === 'rejected' || offer.status === 'withdrawn' || offer.status === 'expired' ? 'default' : 'primary';
  const counterpartName = actor === 'author' ? offer.applicantProfile.name : (offer.classified?.title ?? t('marketplace.offer.author'));
  const rounds = offer.rounds ?? [];
  const currentTerms = describeTerms(offer.terms, t);

  return (
    <Paper variant="outlined" sx={{ p: 2, borderRadius: 3, borderColor: myTurn ? 'primary.main' : undefined }} data-testid="offer-card">
      <Stack spacing={1.25}>
        <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
          <Box>
            <Typography fontWeight={800}>
              {actor === 'author'
                ? <RouterLink to={`/directorio/${encodeURIComponent(offer.applicantProfile.slug)}`}>{offer.applicantProfile.name}</RouterLink>
                : showClassified && offer.classified?.slug
                  ? <RouterLink to={`/clasificados/${encodeURIComponent(offer.classified.slug)}`}>{offer.classified.title}</RouterLink>
                  : counterpartName}
            </Typography>
            {offer.reputation && (offer.reputation.ratingCount ?? 0) > 0 && (
              <Stack direction="row" alignItems="center" gap={0.5}>
                <Rating size="small" readOnly precision={0.1} value={offer.reputation.ratingAvg ?? 0} />
                <Typography variant="caption" color="text.secondary">
                  {t('marketplace.offer.reputation', { rating: offer.reputation.ratingAvg?.toFixed(1), count: offer.reputation.ratingCount, completed: offer.reputation.completedCount ?? 0 })}
                </Typography>
              </Stack>
            )}
          </Box>
          <Stack direction="row" gap={1} alignItems="center" flexWrap="wrap">
            {currentAmount && <Chip icon={<GavelIcon />} color="primary" variant={offer.status === 'accepted' ? 'filled' : 'outlined'} label={acceptedAmount ?? currentAmount} />}
            <Chip size="small" color={statusColor} label={t(OFFER_STATUS_LABEL_KEYS[offer.status])} />
            {turn && <Chip size="small" variant="outlined" label={myTurn ? t('marketplace.offer.yourTurn') : t('marketplace.offer.theirTurn')} />}
          </Stack>
        </Stack>

        <MarkdownText>{offer.message}</MarkdownText>
        {offer.availability && <Typography variant="body2" color="text.secondary">{t('marketplace.offer.availability')}: {offer.availability}</Typography>}
        {currentTerms.length > 0 && (
          <Stack spacing={0.25} data-testid="offer-terms">
            <Typography variant="caption" color="text.secondary" fontWeight={700}>{t('marketplace.offer.terms')}</Typography>
            {currentTerms.map((line) => <Typography key={line} variant="body2" color="text.secondary">{line}</Typography>)}
          </Stack>
        )}
        {offer.rejectionReason && offer.rejectionReason !== 'author_decision' && (
          <Typography variant="body2" color="text.secondary">{t(`marketplace.offer.rejection.${offer.rejectionReason}`)}</Typography>
        )}
        {offer.offerExpiresAt && turn && (
          <Typography variant="caption" color="text.secondary">{t('marketplace.offer.expires', { date: new Date(offer.offerExpiresAt).toLocaleString(i18n.language) })}</Typography>
        )}

        {rounds.length > 1 && (
          <>
            <Button size="small" onClick={() => setShowRounds((value) => !value)} sx={{ alignSelf: 'flex-start' }}>
              {showRounds ? t('marketplace.offer.hideRounds') : t('marketplace.offer.showRounds', { count: rounds.length })}
            </Button>
            <Collapse in={showRounds}>
              <Stack spacing={0.5} divider={<Divider flexItem />}>
                {rounds.map((round) => (
                  <Stack key={round.roundNumber} direction="row" justifyContent="space-between" gap={1}>
                    <Typography variant="body2">
                      {t(`marketplace.offer.round.${round.kind}`, { actor: t(`marketplace.offer.actor.${round.actorRole}`), number: round.roundNumber })}
                      {round.message ? ` — ${round.message}` : ''}
                      {describeTerms(round.terms, t).length > 0 ? ` · ${t('marketplace.offer.roundTerms', { terms: describeTerms(round.terms, t).join('; ') })}` : ''}
                    </Typography>
                    <Typography variant="body2" fontWeight={700}>{formatMinor(round.amountMinor, round.currencyId, currencies, i18n.language) ?? '—'}</Typography>
                  </Stack>
                ))}
              </Stack>
            </Collapse>
          </>
        )}

        {offer.status === 'accepted' && authorProfileId && (
          <TextField label={t('marketplace.offer.conversationMessage')} value={conversationMessage} onChange={(event) => setConversationMessage(event.target.value)} multiline minRows={2} inputProps={{ minLength: 1, maxLength: 5000 }} />
        )}

        <Collapse in={countering}>
          <Stack component="form" spacing={1.5} onSubmit={(event) => { event.preventDefault(); counter.mutate(); }} sx={{ p: 1.5, bgcolor: 'action.hover', borderRadius: 2 }}>
            <Typography fontWeight={700}>{t('marketplace.offer.counterTitle')}</Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} gap={1.5}>
              <FormControl sx={{ minWidth: 140 }}>
                <InputLabel id={`counter-currency-${offer.id}`}>{t('marketplace.offer.currency')}</InputLabel>
                <Select labelId={`counter-currency-${offer.id}`} label={t('marketplace.offer.currency')} value={counterCurrency} onChange={(event) => setCounterCurrency(event.target.value)}>
                  {(currencies ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.code} · {item.symbol ?? item.name}</MenuItem>)}
                </Select>
              </FormControl>
              <TextField fullWidth label={t('marketplace.offer.amount')} type="number" value={counterAmount} onChange={(event) => setCounterAmount(event.target.value)} inputProps={{ min: 0, step: '0.01' }} required />
            </Stack>
            <TextField label={t('marketplace.offer.counterMessage')} value={counterMessage} onChange={(event) => setCounterMessage(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 5000 }} />
            <Stack direction="row" gap={1}>
              <Button type="submit" variant="contained" disabled={counter.isPending || !counterAmount}>{t('marketplace.offer.sendCounter')}</Button>
              <Button onClick={() => setCountering(false)}>{t('marketplace.offer.cancel')}</Button>
            </Stack>
          </Stack>
        </Collapse>

        {error && <Alert severity="error">{error.message}</Alert>}

        <Stack direction="row" gap={1} flexWrap="wrap">
          {actor === 'author' && offer.status === 'submitted' && <Button size="small" onClick={() => transition.mutate('viewed')}>{t('marketplace.offer.markViewed')}</Button>}
          {actor === 'author' && ['submitted', 'viewed'].includes(offer.status) && <Button size="small" onClick={() => transition.mutate('shortlisted')}>{t('marketplace.offer.shortlist')}</Button>}
          {canAccept(offer.status, actor) && <Button size="small" variant="contained" startIcon={<CheckCircleIcon />} onClick={() => transition.mutate('accepted')} disabled={transition.isPending}>{t('marketplace.offer.accept', { amount: currentAmount ?? '' })}</Button>}
          {canCounter(offer.status, actor, priceMode) && !countering && <Button size="small" variant="outlined" onClick={() => setCountering(true)}>{t('marketplace.offer.counter')}</Button>}
          {canReject(offer.status, actor) && <Button size="small" color="error" onClick={() => transition.mutate('rejected')}>{t('marketplace.offer.reject')}</Button>}
          {canWithdraw(offer.status, actor) && <Button size="small" color="warning" onClick={() => transition.mutate('withdrawn')}>{offer.status === 'countered_by_author' ? t('marketplace.offer.decline') : t('marketplace.offer.withdraw')}</Button>}
          {offer.status === 'accepted' && authorProfileId && (
            <Button size="small" variant="contained" onClick={() => conversation.mutate()} disabled={!conversationMessage.trim() || conversation.isPending}>{t('marketplace.offer.openConversation')}</Button>
          )}
        </Stack>
      </Stack>
    </Paper>
  );
}
