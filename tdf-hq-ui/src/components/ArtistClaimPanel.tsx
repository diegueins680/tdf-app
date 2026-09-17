import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { useEffect, useRef, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { Directory } from '../api/directory';
import { getActiveSession } from '../session/SessionContext';

// Claims reuse the directory's reviewed management grants. Never change the
// signed-in Party, create artist credentials, or grant access from email alone.
export default function ArtistClaimPanel({ artistId, accountPartyId }: {
  artistId: number;
  accountPartyId: number;
}) {
  const [profile, setProfile] = useState<{ id: string; name: string } | null>(null);
  const [evidence, setEvidence] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const request = useRef<{ evidence: string; key: string } | null>(null);
  const active = useRef(true);
  const sending = useRef(false);

  useEffect(() => {
    active.current = true;
    let cancelled = false;
    void Directory.profileByParty(artistId).then((value) => {
      if (!cancelled && getActiveSession()?.partyId === accountPartyId) setProfile(value);
    }).catch(() => {
      if (!cancelled) setError('No encontramos un perfil público disponible para reclamar. Tu sesión sigue activa.');
    }).finally(() => { if (!cancelled) setLoading(false); });
    return () => { cancelled = true; active.current = false; };
  }, [artistId, accountPartyId]);

  const submit = async () => {
    const description = evidence.trim();
    if (!profile || sending.current || submitted || description.length < 20
      || description.length > 2000 || getActiveSession()?.partyId !== accountPartyId) return;
    sending.current = true;
    setSubmitting(true);
    setError(null);
    try {
      if (request.current?.evidence !== description) {
        request.current = { evidence: description, key: crypto.randomUUID() };
      }
      await Directory.claim({
        profileId: profile.id,
        claimType: 'administration',
        evidence: [{ description }],
      }, request.current.key);
      if (active.current && getActiveSession()?.partyId === accountPartyId) setSubmitted(true);
    } catch {
      if (active.current && getActiveSession()?.partyId === accountPartyId) {
        setError('No pudimos confirmar el envío. Puedes reintentar sin cerrar tu sesión.');
      }
    } finally {
      sending.current = false;
      if (active.current) setSubmitting(false);
    }
  };

  return (
    <Stack spacing={2}>
      <Alert severity="info">
        Conserva tu cuenta y solicita permiso para administrar este artista.
        El equipo revisará las pruebas de titularidad antes de conceder acceso.
        Los seguidores, compras e historial de ambas identidades se conservan.
      </Alert>
      {loading && <Typography role="status">Buscando el perfil del artista…</Typography>}
      {error && <Alert severity="error">{error}</Alert>}
      {submitted ? (
        <Alert severity="success">
          Solicitud enviada para revisión. Todavía no se ha concedido acceso.
          Después de la aprobación, el perfil aparecerá en Mis perfiles y clasificados.
        </Alert>
      ) : profile && (
        <>
          <Typography fontWeight={700}>{profile.name}</Typography>
          <TextField label="Pruebas de titularidad o representación" multiline minRows={3}
            value={evidence} onChange={(event) => setEvidence(event.target.value)} disabled={submitting}
            inputProps={{ maxLength: 2000 }}
            helperText="Incluye enlaces oficiales y explica tu relación con el artista (20–2000 caracteres). No envíes contraseñas ni documentos sensibles." />
          <Button variant="contained" disabled={submitting || evidence.trim().length < 20}
            onClick={() => void submit()}>
            {submitting ? 'Enviando…' : 'Solicitar administración del perfil'}
          </Button>
        </>
      )}
      <Button component={RouterLink} to="/mis-clasificados">Mis perfiles y clasificados</Button>
    </Stack>
  );
}
