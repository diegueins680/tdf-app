import { useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import {
  ArtistInvitations,
  type ArtistInvitationLinkStatus,
} from '../api/artistInvitations';
import { buildArtistInvitationLink } from '../utils/loginRouting';

const DEFAULT_CAMPAIGN = 'tu_escena_conectada_piloto';
const PUBLIC_ORIGIN = 'https://www.tdfrecords.net';

const statusLabel: Record<ArtistInvitationLinkStatus, string> = {
  active: 'Activo',
  redeemed: 'Usado',
  expired: 'Vencido',
  revoked: 'Revocado',
};

const statusTone: Record<ArtistInvitationLinkStatus, 'success' | 'default' | 'warning' | 'error'> = {
  active: 'success',
  redeemed: 'default',
  expired: 'warning',
  revoked: 'error',
};

function formatDate(value: string) {
  return new Date(value).toLocaleString('es-EC', { dateStyle: 'medium', timeStyle: 'short' });
}

// Staff-only: each link works once, for the first account that opens it.
export default function ArtistInvitationLinksPanel() {
  const queryClient = useQueryClient();
  const [inviteeLabel, setInviteeLabel] = useState('');
  const [issuedLink, setIssuedLink] = useState<{ label: string; url: string } | null>(null);
  const [copied, setCopied] = useState(false);

  const listQuery = useQuery({
    queryKey: ['artist-invitations'],
    queryFn: ArtistInvitations.list,
  });

  const createMutation = useMutation({
    mutationFn: () => ArtistInvitations.create({ inviteeLabel: inviteeLabel.trim(), campaign: DEFAULT_CAMPAIGN }),
    onSuccess: (issued) => {
      setIssuedLink({
        label: issued.invitation.inviteeLabel,
        url: buildArtistInvitationLink(PUBLIC_ORIGIN, issued.token, issued.invitation.campaign),
      });
      setCopied(false);
      setInviteeLabel('');
      void queryClient.invalidateQueries({ queryKey: ['artist-invitations'] });
    },
  });

  const revokeMutation = useMutation({
    mutationFn: (id: number) => ArtistInvitations.revoke(id),
    onSuccess: () => { void queryClient.invalidateQueries({ queryKey: ['artist-invitations'] }); },
  });

  const copyLink = async () => {
    if (!issuedLink) return;
    try {
      await navigator.clipboard.writeText(issuedLink.url);
      setCopied(true);
    } catch {
      setCopied(false);
    }
  };

  return (
    <Stack spacing={2} component="section" aria-labelledby="artist-invitations-title">
      <Box>
        <Typography id="artist-invitations-title" variant="h5" component="h2">Invitaciones de artista</Typography>
        <Typography color="text.secondary">
          Cada enlace es personal y funciona una sola vez: la primera cuenta que lo abre recibe el rol de Artista.
          Vence en 30 días.
        </Typography>
      </Box>
      <Stack
        component="form"
        direction={{ xs: 'column', sm: 'row' }}
        spacing={1}
        onSubmit={(event) => {
          event.preventDefault();
          if (inviteeLabel.trim()) createMutation.mutate();
        }}
      >
        <TextField
          label="Artista o banda invitada"
          value={inviteeLabel}
          onChange={(event) => setInviteeLabel(event.target.value)}
          inputProps={{ maxLength: 120 }}
          size="small"
          sx={{ flex: 1 }}
        />
        <Button type="submit" variant="contained" disabled={!inviteeLabel.trim() || createMutation.isPending}>
          Crear enlace
        </Button>
      </Stack>
      {createMutation.isError ? <Alert severity="error">{createMutation.error.message}</Alert> : null}
      {issuedLink ? (
        <Alert
          severity="success"
          action={<Button color="inherit" size="small" onClick={() => { void copyLink(); }}>{copied ? 'Copiado' : 'Copiar'}</Button>}
        >
          <Typography variant="body2">
            Enlace para {issuedLink.label}. Cópialo ahora: no se vuelve a mostrar.
          </Typography>
          <Typography variant="body2" sx={{ wordBreak: 'break-all' }} data-testid="artist-invitation-link">
            {issuedLink.url}
          </Typography>
        </Alert>
      ) : null}
      {listQuery.isPending ? <CircularProgress aria-label="Invitaciones de artista" /> : null}
      {listQuery.isError ? <Alert severity="error">{listQuery.error.message}</Alert> : null}
      {revokeMutation.isError ? <Alert severity="error">{revokeMutation.error.message}</Alert> : null}
      <Stack spacing={1} aria-live="polite">
        {listQuery.data?.map((invitation) => (
          <Card key={invitation.id} variant="outlined">
            <CardContent>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                <Box sx={{ flex: 1 }}>
                  <Typography fontWeight={600}>{invitation.inviteeLabel}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    Creado {formatDate(invitation.createdAt)}
                    {invitation.redeemedAt
                      ? ` · usado ${formatDate(invitation.redeemedAt)}${invitation.redeemedByName ? ` por ${invitation.redeemedByName}` : ''}`
                      : ` · vence ${formatDate(invitation.expiresAt)}`}
                  </Typography>
                </Box>
                <Chip size="small" label={statusLabel[invitation.status]} color={statusTone[invitation.status]} />
                {invitation.status === 'active' ? (
                  <Button
                    size="small"
                    color="error"
                    disabled={revokeMutation.isPending}
                    onClick={() => revokeMutation.mutate(invitation.id)}
                  >
                    Revocar
                  </Button>
                ) : null}
              </Stack>
            </CardContent>
          </Card>
        ))}
      </Stack>
    </Stack>
  );
}
