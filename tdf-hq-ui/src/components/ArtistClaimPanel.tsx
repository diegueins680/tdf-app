import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { useCallback, useEffect, useRef, useState } from 'react';
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
  const [attempt, setAttempt] = useState(0);
  const [submitting, setSubmitting] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const request = useRef<{ evidence: string; key: string } | null>(null);
  const active = useRef(true);
  const authority = useRef(getActiveSession()).current;
  const isCurrent = useCallback(() => active.current && getActiveSession() === authority
    && authority?.partyId === accountPartyId, [authority, accountPartyId]);
  const sending = useRef(false);

  useEffect(() => {
    active.current = true;
    let cancelled = false;
    setLoading(true);
    setError(null);
    void Directory.prepareArtistClaim(artistId).then((value) => {
      if (!cancelled && isCurrent()) setProfile(value);
    }).catch(() => {
      if (!cancelled && isCurrent()) setError('No pudimos preparar la solicitud para este artista. Tu sesión sigue activa.');
    }).finally(() => { if (!cancelled) setLoading(false); });
    return () => { cancelled = true; active.current = false; };
  }, [artistId, accountPartyId, isCurrent, attempt]);

  const submit = async () => {
    const description = evidence.trim();
    if (!profile || sending.current || submitted || description.length < 20
      || description.length > 2000 || !isCurrent()) return;
    sending.current = true;
    setSubmitting(true);
    setError(null);
    try {
      if (request.current?.evidence !== description) {
        request.current = { evidence: description, key: crypto.randomUUID() };
      }
      const receipt = await Directory.claim({
        profileId: profile.id,
        claimType: 'administration',
        evidence: [{ description }],
      }, request.current.key);
      if (typeof receipt?.['id'] !== 'string' || !receipt['id']
        || receipt['profileId'] !== profile.id || receipt['claimType'] !== 'administration'
        || typeof receipt['status'] !== 'string' || !receipt['status']
        || typeof receipt['submittedAt'] !== 'string' || !Number.isFinite(Date.parse(receipt['submittedAt']))) {
        throw new Error('Unconfirmed claim receipt');
      }
      if (isCurrent()) setSubmitted(true);
    } catch {
      if (active.current && isCurrent()) {
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
      {!loading && !profile && <Button onClick={() => setAttempt(value => value + 1)}>Reintentar</Button>}
      {submitted ? (
        <Alert severity="success">
          Solicitud registrada para revisión. El acceso requiere una aprobación verificada.
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
