import { useState } from 'react';
import { useTranslation } from 'react-i18next';
import { useQuery } from '@tanstack/react-query';
import { Alert, Button, CircularProgress, Stack, Tab, Tabs } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

import { Directory, type DirectoryTaxonomyItem, type MyOffersStatus } from '../../api/directory';
import OfferCard from './OfferCard';

const FILTERS: MyOffersStatus[] = ['open', 'accepted', 'closed'];

/** The offerer's view: every offer made through profiles the account manages (ADR 0134). */
export default function MyOffersPanel({ currencies }: { currencies?: DirectoryTaxonomyItem[] }) {
  const { t } = useTranslation();
  const [filter, setFilter] = useState<MyOffersStatus>('open');
  const offers = useQuery({ queryKey: ['directory', 'my-offers', filter], queryFn: () => Directory.myApplications(filter) });

  return (
    <Stack spacing={2} data-testid="my-offers">
      <Tabs value={filter} onChange={(_, value: unknown) => { if (typeof value === 'string' && FILTERS.includes(value as MyOffersStatus)) setFilter(value as MyOffersStatus); }} aria-label={t('marketplace.mine.filterLabel')}>
        {FILTERS.map((value) => <Tab key={value} value={value} label={t(`marketplace.mine.filter.${value}`)} />)}
      </Tabs>
      {offers.isLoading && <CircularProgress aria-label={t('marketplace.mine.loading')} />}
      {offers.isError && <Alert severity="error">{t('marketplace.mine.loadError')}</Alert>}
      {offers.data?.length === 0 && (
        <Alert severity="info" action={<Button component={RouterLink} to="/buscar?entityType=classified&intent=seeking" color="inherit">{t('marketplace.mine.browse')}</Button>}>
          {t(`marketplace.mine.empty.${filter}`)}
        </Alert>
      )}
      {offers.data?.map((offer) => <OfferCard key={offer.id} offer={offer} currencies={currencies} showClassified />)}
    </Stack>
  );
}
