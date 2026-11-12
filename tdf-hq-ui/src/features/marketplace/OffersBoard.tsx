import { useState } from 'react';
import { useTranslation } from 'react-i18next';
import { useQuery } from '@tanstack/react-query';
import { Alert, Chip, CircularProgress, Stack, ToggleButton, ToggleButtonGroup, Typography } from '@mui/material';

import { Directory, type DirectoryTaxonomyItem, type ManagedClassified, type OfferSort } from '../../api/directory';
import OfferCard from './OfferCard';
import { formatMinor, isOfferOpen, offerRangesByCurrency, sortOffers } from './offers';

const SORTS: OfferSort[] = ['recent', 'price_asc', 'price_desc', 'reputation'];

/**
 * The author's comparison board for one classified: every offer received,
 * sortable by price, reputation or recency (ADR 0134, inDrive-style choice).
 * Sorting happens client-side on the already-fetched list so switching is
 * instant; the server accepts the same `sort` for larger lists.
 */
export default function OffersBoard({ classified, currencies }: { classified: ManagedClassified; currencies?: DirectoryTaxonomyItem[] }) {
  const { t, i18n } = useTranslation();
  const [sort, setSort] = useState<OfferSort>('recent');
  const offers = useQuery({ queryKey: ['directory', 'applications', classified.id], queryFn: () => Directory.applications(classified.id) });

  if (offers.isLoading) return <CircularProgress size={24} aria-label={t('auditAccessibility.loadingApplications')} />;
  if (offers.isError) return <Alert severity="error">{t('marketplace.board.loadError')}</Alert>;
  const list = sortOffers(offers.data ?? [], sort);
  const open = list.filter((offer) => isOfferOpen(offer.status));
  const ranges = offerRangesByCurrency(open, classified.budget?.currencyId);

  return (
    <Stack spacing={1.5} mt={2} data-testid="offers-board">
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ md: 'center' }} gap={1}>
        <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center">
          <Typography fontWeight={700}>{t('marketplace.board.title', { count: list.length })}</Typography>
          <Chip size="small" label={t('marketplace.board.slots', { remaining: classified.slotsRemaining, total: classified.slots })} color={classified.slotsRemaining === 0 ? 'default' : 'success'} />
          {ranges.map((range) => (
            <Chip key={range.currencyId ?? 'none'} size="small" variant="outlined" label={t('marketplace.board.range', { range: `${formatMinor(range.minMinor, range.currencyId, currencies, i18n.language)} – ${formatMinor(range.maxMinor, range.currencyId, currencies, i18n.language)}` })} />
          ))}
          {classified.priceMode !== 'open' && classified.budget?.maxMinor != null && (
            <Chip size="small" variant="outlined" label={t(`marketplace.priceMode.${classified.priceMode}`, { amount: formatMinor(classified.budget.maxMinor, classified.budget.currencyId, currencies, i18n.language) })} />
          )}
        </Stack>
        <ToggleButtonGroup exclusive size="small" value={sort} onChange={(_, value: unknown) => { if (typeof value === 'string' && SORTS.includes(value as OfferSort)) setSort(value as OfferSort); }} aria-label={t('marketplace.board.sortLabel')}>
          {SORTS.map((value) => <ToggleButton key={value} value={value}>{t(`marketplace.board.sort.${value}`)}</ToggleButton>)}
        </ToggleButtonGroup>
      </Stack>
      {list.length === 0 && <Typography color="text.secondary">{t('marketplace.board.empty')}</Typography>}
      {list.map((offer) => <OfferCard key={offer.id} offer={offer} currencies={currencies} authorProfileId={classified.authorProfileId} />)}
    </Stack>
  );
}
