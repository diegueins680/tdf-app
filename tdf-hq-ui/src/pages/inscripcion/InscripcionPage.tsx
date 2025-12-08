import { useParams, useSearchParams } from 'react-router-dom';
import { useEffect, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Container,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import PublicBrandBar from '../../components/PublicBrandBar';

export default function InscripcionPage() {
  const { slug } = useParams();
  const [sp] = useSearchParams();
  const leadId = sp.get('lead') ?? '';
  const token = sp.get('t') ?? '';

  const storageKey = leadId ? `inscripcion-lead-${leadId}` : 'inscripcion-lead';
  const [name, setName] = useState('');
  const [email, setEmail] = useState('');
  const [done, setDone] = useState(false);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const stored = window.localStorage.getItem(storageKey);
      if (stored) {
        const parsed = JSON.parse(stored) as { name?: string; email?: string };
        setName(parsed.name ?? '');
        setEmail(parsed.email ?? '');
      }
    } catch {
      // ignore storage read errors
    }
  }, [storageKey]);

  const submit = async () => {
    setBusy(true);
    setError(null);
    try {
      const res = await fetch(`${import.meta.env.VITE_API_BASE}/api/leads/${leadId}/complete`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ token, name, email }),
      });
      if (!res.ok) {
        setError('No pudimos validar tu enlace. Revisa que sea el link más reciente.');
        return;
      }
      const body = (await res.json().catch(() => ({}))) as { ok?: boolean };
      if (body.ok) {
        setDone(true);
        if (typeof window !== 'undefined') {
          window.localStorage.setItem(storageKey, JSON.stringify({ name, email }));
        }
      } else {
        setError('No pudimos completar tu inscripción. Revisa el enlace e inténtalo de nuevo.');
      }
    } finally {
      setBusy(false);
    }
  };

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    void submit();
  };

  const title = slug?.replace(/-/g, ' ') ?? 'Programa';
  const enrollmentUrl =
    typeof window !== 'undefined'
      ? slug
        ? `${window.location.origin}/inscripcion/${slug}`
        : window.location.href
      : '';

  const renderFrame = (children: React.ReactNode) => (
    <Box
      sx={{
        minHeight: '100vh',
        background: 'linear-gradient(135deg, #0b1224, #0f172a)',
        color: '#e2e8f0',
        py: { xs: 4, md: 6 },
      }}
    >
      <Container maxWidth="sm">
        <Stack spacing={3} alignItems="center">
          <PublicBrandBar tagline="Inscripción TDF Records" compact />
          <Paper
            elevation={0}
            sx={{
              p: { xs: 3, md: 4 },
              width: '100%',
              bgcolor: 'rgba(15,23,42,0.75)',
              border: '1px solid rgba(255,255,255,0.08)',
              backdropFilter: 'blur(12px)',
            }}
          >
            {children}
          </Paper>
        </Stack>
      </Container>
    </Box>
  );

  if (!leadId || !token) {
    return renderFrame(
      <Stack spacing={2}>
        <Typography variant="h4" fontWeight={800}>
          Invitación inválida o incompleta
        </Typography>
        <Typography variant="body1" color="rgba(226,232,240,0.85)">
          Vuelve al mensaje de WhatsApp y abre nuevamente el enlace. Si persiste, contáctanos por WhatsApp.
        </Typography>
      </Stack>,
    );
  }

  if (done) {
    return renderFrame(
      <Stack spacing={2}>
        <Typography variant="h4" fontWeight={800}>
          ¡Listo! 🎉
        </Typography>
        <Typography variant="body1" color="rgba(226,232,240,0.85)">
          Hemos recibido tus datos para <strong>{title}</strong>. Revisa tu correo y WhatsApp; te enviaremos los
          siguientes pasos y confirmación de cupo.
        </Typography>
      </Stack>,
    );
  }

  return renderFrame(
    <Stack spacing={3} component="form" onSubmit={handleSubmit}>
      <Stack spacing={1}>
        <Typography variant="h4" fontWeight={800}>
          Inscripción — {title}
        </Typography>
        <Typography variant="body2" color="rgba(226,232,240,0.8)">
          Confirma tus datos para reservar tu cupo. Validamos el enlace y el token antes de registrar la inscripción.
          Si el enlace expiró, escribe por WhatsApp para recibir uno nuevo.
        </Typography>
      </Stack>
      <Stack spacing={2}>
        <TextField
          label="Nombre completo"
          value={name}
          onChange={(e) => setName(e.target.value)}
          required
          fullWidth
          InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.8)' } }}
          sx={{
            '& .MuiOutlinedInput-root': {
              color: '#f8fafc',
              bgcolor: 'rgba(255,255,255,0.03)',
              '& fieldset': { borderColor: 'rgba(255,255,255,0.12)' },
              '&:hover fieldset': { borderColor: 'rgba(255,255,255,0.25)' },
              '&.Mui-focused fieldset': { borderColor: '#7c3aed' },
            },
          }}
        />
        <TextField
          label="Correo"
          type="email"
          value={email}
          onChange={(e) => setEmail(e.target.value)}
          required
          fullWidth
          InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.8)' } }}
          sx={{
            '& .MuiOutlinedInput-root': {
              color: '#f8fafc',
              bgcolor: 'rgba(255,255,255,0.03)',
              '& fieldset': { borderColor: 'rgba(255,255,255,0.12)' },
              '&:hover fieldset': { borderColor: 'rgba(255,255,255,0.25)' },
              '&.Mui-focused fieldset': { borderColor: '#7c3aed' },
            },
          }}
        />
        <Alert severity="info" sx={{ bgcolor: 'rgba(14,165,233,0.08)', color: '#e0f2fe' }}>
          Asegúrate de usar el mismo correo con el que conversamos para validar tu cupo. Guardamos tu nombre y correo en este dispositivo para que no tengas que volver a escribirlos. Si ves un error de enlace, pide que te reenvíen el link: {enrollmentUrl}
        </Alert>
        {error && (
          <Alert severity="error" sx={{ bgcolor: 'rgba(239,68,68,0.08)', color: '#fecdd3' }}>
            {error}
          </Alert>
        )}
      </Stack>
      <Stack direction="row" spacing={2} justifyContent="flex-end">
        <Button type="submit" variant="contained" disabled={busy} sx={{ textTransform: 'none', fontWeight: 800 }}>
          {busy ? 'Enviando…' : 'Enviar'}
        </Button>
      </Stack>
    </Stack>,
  );
}
