import { useEffect, useState, type FormEvent } from 'react';
import { useTranslation } from 'react-i18next';
import { useMutation, useQuery } from '@tanstack/react-query';
import { Alert, Button, Chip, FormControl, InputLabel, MenuItem, Paper, Select, Stack, TextField, Typography } from '@mui/material';

import { Directory, type DirectoryTaxonomies, type ManagedDirectoryProfile } from '../../api/directory';
import { formatMinor, moneyInputToMinor, referenceAmount, describeTerms } from './offers';

interface OfferFormProps {
  classifiedId: string;
  /** Public slug, when known, to show the classified's price mode and reference price. */
  classifiedSlug?: string | null;
  profiles: ManagedDirectoryProfile[];
  taxonomies?: DirectoryTaxonomies;
  onDone: () => void;
}

/**
 * Submit an offer on a classified (ADR 0134): acting profile, message, amount
 * and currency, optional availability and terms. Fixed-price classifieds
 * lock the amount to the reference price; suggested prices prefill it.
 */
export default function OfferForm({ classifiedId, classifiedSlug, profiles, taxonomies, onDone }: OfferFormProps) {
  const { t, i18n } = useTranslation();
  const classified = useQuery({ queryKey: ['directory', 'classified', classifiedSlug], queryFn: () => Directory.classified(classifiedSlug!), enabled: Boolean(classifiedSlug) });
  const reference = referenceAmount(classified.data ? { priceMode: classified.data.priceMode, budget: classified.data.compensation ? { maxMinor: classified.data.compensation.maxMinor, currencyId: classified.data.compensation.currencyId } : null } : undefined);
  const priceMode = classified.data?.priceMode ?? 'open';
  const currencies = taxonomies?.currencies ?? [];
  const [profileId, setProfileId] = useState('');
  const [message, setMessage] = useState('');
  const [availability, setAvailability] = useState('');
  const [currencyId, setCurrencyId] = useState('');
  const [amount, setAmount] = useState('');
  const [durationMinutes, setDurationMinutes] = useState('');
  const [equipmentProvided, setEquipmentProvided] = useState('');

  useEffect(() => {
    if (reference) {
      const minorUnits = currencies.find((item) => item.id === reference.currencyId)?.minorUnits ?? 2;
      setCurrencyId(reference.currencyId ?? '');
      setAmount((reference.amountMinor / 10 ** minorUnits).toString());
    } else if (!currencyId) {
      setCurrencyId(currencies.find((item) => item.code === 'USD')?.id ?? currencies[0]?.id ?? '');
    }
    // Only react to the loaded reference price and currency catalog.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [reference?.amountMinor, reference?.currencyId, currencies.length]);

  const mutation = useMutation({
    mutationFn: () => {
      const minorUnits = currencies.find((item) => item.id === currencyId)?.minorUnits ?? 2;
      const amountMinor = moneyInputToMinor(amount, minorUnits);
      const terms: Record<string, unknown> = {};
      if (durationMinutes.trim()) terms['durationMinutes'] = Number(durationMinutes);
      if (equipmentProvided.trim()) terms['equipmentProvided'] = equipmentProvided.trim();
      return Directory.apply(classifiedId, {
        applicantProfileId: profileId,
        message: message.trim(),
        portfolio: [],
        availability: availability.trim() || undefined,
        amountMinor,
        currencyId: amountMinor !== undefined ? currencyId : undefined,
        terms: Object.keys(terms).length ? terms : undefined,
      });
    },
    onSuccess: onDone,
  });

  const slotsLeft = classified.data?.slotsRemaining;
  const closed = classified.data ? (slotsLeft === 0 || (classified.data.offerDeadlineAt ? new Date(classified.data.offerDeadlineAt) < new Date() : false)) : false;
  // Only a fixed price makes the amount mandatory (it is locked to the
  // reference). Open and suggested listings, including unpaid or exchange
  // compensation, accept a message-only offer; a suggested price is prefilled.
  const amountRequired = priceMode === 'fixed';
  // Conditions the publisher attached to the listing (duration, capacity,
  // equipment…) are shown before quoting, so an offer answers them.
  const listingTerms = describeTerms(classified.data?.terms, t);

  return (
    <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }} data-testid="offer-form">
      <Stack component="form" spacing={2} onSubmit={(event: FormEvent) => { event.preventDefault(); mutation.mutate(); }}>
        <Typography variant="h5" fontWeight={800}>{t('marketplace.form.title')}</Typography>
        {classified.data && (
          <Stack direction="row" gap={1} flexWrap="wrap">
            <Chip size="small" label={classified.data.title} />
            <Chip size="small" color="primary" variant="outlined" label={t(`marketplace.priceMode.${priceMode}`, { amount: reference ? formatMinor(reference.amountMinor, reference.currencyId, currencies, i18n.language) : '' })} />
            {typeof slotsLeft === 'number' && <Chip size="small" variant="outlined" label={t('marketplace.board.slots', { remaining: slotsLeft, total: classified.data.slots ?? 1 })} />}
            {classified.data.offerStats && classified.data.offerStats.count > 0 && (
              <Chip size="small" variant="outlined" label={classified.data.offerStats.minMinor != null
                ? t('marketplace.form.statsRange', { count: classified.data.offerStats.count, min: formatMinor(classified.data.offerStats.minMinor, classified.data.offerStats.currencyId, currencies, i18n.language), max: formatMinor(classified.data.offerStats.maxMinor, classified.data.offerStats.currencyId, currencies, i18n.language) })
                : t('marketplace.form.statsCount', { count: classified.data.offerStats.count })} />
            )}
          </Stack>
        )}
        {listingTerms.length > 0 && (
          <Alert severity="info" icon={false} data-testid="listing-terms">
            <Typography variant="subtitle2" fontWeight={700}>{t('marketplace.offer.listingTerms')}</Typography>
            {listingTerms.map((line) => <Typography key={line} variant="body2">{line}</Typography>)}
          </Alert>
        )}
        {closed && <Alert severity="warning">{t('marketplace.form.closed')}</Alert>}
        <FormControl>
          <InputLabel id="offer-profile">{t('marketplace.form.actAs')}</InputLabel>
          <Select labelId="offer-profile" label={t('marketplace.form.actAs')} value={profileId} onChange={(event) => setProfileId(event.target.value)}>
            <MenuItem value="" disabled>{t('marketplace.form.selectProfile')}</MenuItem>
            {profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}
          </Select>
        </FormControl>
        <Stack direction={{ xs: 'column', sm: 'row' }} gap={1.5}>
          <FormControl sx={{ minWidth: 150 }}>
            <InputLabel id="offer-currency">{t('marketplace.offer.currency')}</InputLabel>
            <Select labelId="offer-currency" label={t('marketplace.offer.currency')} value={currencyId} onChange={(event) => setCurrencyId(event.target.value)} disabled={priceMode === 'fixed'}>
              {currencies.map((item) => <MenuItem key={item.id} value={item.id}>{item.code} · {item.symbol ?? item.name}</MenuItem>)}
            </Select>
          </FormControl>
          <TextField fullWidth label={t('marketplace.offer.amount')} type="number" value={amount} onChange={(event) => setAmount(event.target.value)} inputProps={{ min: 0, step: '0.01' }} required={amountRequired} disabled={priceMode === 'fixed'}
            helperText={priceMode === 'fixed' ? t('marketplace.form.fixedHelp') : priceMode === 'suggested' ? t('marketplace.form.suggestedHelp') : t('marketplace.form.openHelp')} />
        </Stack>
        <TextField label={t('marketplace.form.message')} multiline minRows={4} value={message} onChange={(event) => setMessage(event.target.value)} required inputProps={{ minLength: 10, maxLength: 5000 }} helperText={t('marketplace.form.messageHelp')} />
        <Stack direction={{ xs: 'column', sm: 'row' }} gap={1.5}>
          <TextField fullWidth label={t('marketplace.form.availability')} value={availability} onChange={(event) => setAvailability(event.target.value)} inputProps={{ maxLength: 500 }} />
          <TextField label={t('marketplace.form.duration')} type="number" value={durationMinutes} onChange={(event) => setDurationMinutes(event.target.value)} inputProps={{ min: 0, step: 15 }} sx={{ minWidth: 180 }} />
        </Stack>
        <TextField label={t('marketplace.form.equipment')} value={equipmentProvided} onChange={(event) => setEquipmentProvided(event.target.value)} inputProps={{ maxLength: 500 }} />
        {mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}
        <Button type="submit" variant="contained" disabled={closed || !profileId || message.trim().length < 10 || (amountRequired && !amount) || mutation.isPending}>
          {mutation.isPending ? t('marketplace.form.sending') : t('marketplace.form.submit')}
        </Button>
      </Stack>
    </Paper>
  );
}
