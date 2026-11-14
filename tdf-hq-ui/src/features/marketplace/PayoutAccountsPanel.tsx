import { useState, type FormEvent } from 'react';
import { useTranslation } from 'react-i18next';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Chip, FormControl, FormControlLabel, InputLabel, MenuItem, Paper, Select, Stack, Switch, TextField, Typography } from '@mui/material';

import { ApiError } from '../../api/client';
import { Directory, type ManagedDirectoryProfile, type PayoutAccount, type PayoutAccountRequest } from '../../api/directory';
import { payoutIdentifierValid } from './agreements';

interface PayoutAccountsPanelProps {
  profiles: ManagedDirectoryProfile[];
}

/**
 * Where the profiles the user manages are paid for their agreements (ADR
 * 0136). Only PayPhone recipients are accepted for now. The recipient is sent
 * once and comes back masked; the gate that enables agreement payments is a
 * server decision, so a 503 is shown as "not enabled yet" and nothing is kept.
 */
export default function PayoutAccountsPanel({ profiles }: PayoutAccountsPanelProps) {
  const { t } = useTranslation();
  const queryClient = useQueryClient();
  const [profileId, setProfileId] = useState('');
  const [usePhone, setUsePhone] = useState(false);
  const kind: PayoutAccountRequest['identifierType'] = usePhone ? 'phone' : 'identification';
  const [identifier, setIdentifier] = useState('');
  const accounts = useQuery({ queryKey: ['directory', 'payout-accounts'], queryFn: Directory.payoutAccounts });
  const selectedProfile = profileId !== '' ? profileId : profiles[0]?.id ?? '';
  const recipient = identifier.trim();
  const valid = selectedProfile !== '' && payoutIdentifierValid(kind, recipient);
  const refresh = async () => { await queryClient.invalidateQueries({ queryKey: ['directory'] }); };
  const save = useMutation({
    mutationFn: () => Directory.savePayoutAccount(selectedProfile, 'payphone', { identifierType: kind, identifier: recipient }),
    onSuccess: async () => { setIdentifier(''); await refresh(); },
  });
  const disable = useMutation({
    mutationFn: (account: PayoutAccount) => Directory.disablePayoutAccount(account.profileId, account.provider),
    onSuccess: refresh,
  });
  const failure = save.error ?? disable.error;
  const notEnabled = failure instanceof ApiError && failure.status === 503;
  const submit = (event: FormEvent) => { event.preventDefault(); if (valid) save.mutate(); };

  if (profiles.length === 0) return null;
  return (
    <Paper variant="outlined" sx={{ p: 2 }} data-testid="payout-accounts">
      <Stack spacing={2}>
        <Typography component="h3" variant="h6" fontWeight={800}>{t('marketplace.payout.title')}</Typography>
        <Typography variant="body2" color="text.secondary">{t('marketplace.payout.intro')}</Typography>
        {accounts.data?.map((account) => (
          <Stack key={account.id} direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
            <Typography fontWeight={700}>{account.profileName}</Typography>
            <Chip size="small" label={t(`marketplace.payout.provider.${account.provider}`)} />
            <Typography variant="body2">{account.identifierMasked}</Typography>
            <Chip size="small" color={account.status === 'ready' ? 'success' : 'default'} label={t(`marketplace.payout.status.${account.status}`)} />
            {account.status !== 'disabled' && (
              <Button size="small" color="warning" disabled={disable.isPending} onClick={() => disable.mutate(account)}>
                {t('marketplace.payout.disable')}
              </Button>
            )}
          </Stack>
        ))}
        <Stack component="form" spacing={2} onSubmit={submit} noValidate>
          {profiles.length > 1 && (
            <FormControl size="small">
              <InputLabel id="payout-profile-label">{t('marketplace.payout.profile')}</InputLabel>
              <Select labelId="payout-profile-label" label={t('marketplace.payout.profile')} value={selectedProfile} onChange={(event) => setProfileId(event.target.value)}>
                {profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}
              </Select>
            </FormControl>
          )}
          <FormControlLabel
            control={<Switch checked={usePhone} onChange={(event) => setUsePhone(event.target.checked)} />}
            label={t('marketplace.payout.usePhone')}
          />
          <TextField
            size="small"
            label={t(`marketplace.payout.identifier.${kind}`)}
            value={identifier}
            onChange={(event) => setIdentifier(event.target.value)}
            error={recipient !== '' && !valid}
            helperText={t(`marketplace.payout.identifierHelp.${kind}`)}
            inputProps={{ inputMode: kind === 'phone' ? 'tel' : 'numeric', autoComplete: 'off' }}
          />
          {notEnabled && <Alert severity="info">{t('marketplace.payout.notEnabled')}</Alert>}
          {failure !== null && !notEnabled && <Alert severity="error">{t('marketplace.payout.error')}</Alert>}
          <Button type="submit" variant="contained" disabled={!valid || save.isPending} sx={{ alignSelf: 'flex-start' }}>
            {t('marketplace.payout.save')}
          </Button>
        </Stack>
      </Stack>
    </Paper>
  );
}
