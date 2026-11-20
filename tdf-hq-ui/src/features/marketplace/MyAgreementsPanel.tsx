import { useState } from 'react';
import { useTranslation } from 'react-i18next';
import { useQuery } from '@tanstack/react-query';
import { Alert, CircularProgress, Stack, Tab, Tabs } from '@mui/material';

import { Directory, type DirectoryTaxonomyItem, type MyAgreementsStatus } from '../../api/directory';
import AgreementCard from './AgreementCard';

/** Agreements where the account manages the buyer or the provider profile (ADR 0135). */
export default function MyAgreementsPanel({ currencies }: { currencies?: DirectoryTaxonomyItem[] }) {
  const { t } = useTranslation();
  const [filter, setFilter] = useState<MyAgreementsStatus>('open');
  const agreements = useQuery({ queryKey: ['directory', 'my-agreements', filter], queryFn: () => Directory.myAgreements(filter) });

  return (
    <Stack spacing={2} data-testid="my-agreements">
      <Tabs value={filter} onChange={(_, value: unknown) => { if (value === 'open' || value === 'closed') setFilter(value); }} aria-label={t('marketplace.agreement.filterLabel')}>
        <Tab value="open" label={t('marketplace.agreement.filter.open')} />
        <Tab value="closed" label={t('marketplace.agreement.filter.closed')} />
      </Tabs>
      {agreements.isLoading && <CircularProgress aria-label={t('marketplace.agreement.loading')} />}
      {agreements.isError && <Alert severity="error">{t('marketplace.agreement.loadError')}</Alert>}
      {agreements.data?.length === 0 && <Alert severity="info">{t(`marketplace.agreement.empty.${filter}`)}</Alert>}
      {agreements.data?.map((agreement) => <AgreementCard key={agreement.id} agreement={agreement} currencies={currencies} />)}
    </Stack>
  );
}
