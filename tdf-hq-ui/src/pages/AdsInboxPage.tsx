import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Avatar,
  Box,
  Chip,
  LinearProgress,
  Paper,
  Stack,
  Typography,
} from '@mui/material';
import EmailIcon from '@mui/icons-material/Email';
import PhoneIphoneIcon from '@mui/icons-material/PhoneIphone';
import CampaignIcon from '@mui/icons-material/Campaign';
import TagIcon from '@mui/icons-material/Tag';
import { Ads } from '../api/ads';

export default function AdsInboxPage() {
  const inquiriesQuery = useQuery({
    queryKey: ['ads-inquiries'],
    queryFn: Ads.list,
  });

  const inquiries = useMemo(() => inquiriesQuery.data ?? [], [inquiriesQuery.data]);

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems="flex-start" spacing={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">Escuela</Typography>
          <Typography variant="h4" fontWeight={800}>Inbox de Ads</Typography>
          <Typography color="text.secondary">
            Leads de Instagram/Facebook y WhatsApp con curso de interés y CTA para agendar trial.
          </Typography>
        </Box>
      </Stack>

      {inquiriesQuery.isLoading && <LinearProgress />}
      {inquiriesQuery.error && (
        <Alert severity="error">
          {inquiriesQuery.error instanceof Error ? inquiriesQuery.error.message : 'No se pudieron cargar los leads.'}
        </Alert>
      )}

      <Stack spacing={1.5}>
        {inquiries.length === 0 && !inquiriesQuery.isLoading && (
          <Typography color="text.secondary">No hay leads aún.</Typography>
        )}
        {inquiries.map((inq) => (
          <Paper key={inq.inquiryId} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} alignItems={{ xs: 'flex-start', sm: 'center' }}>
              <Avatar sx={{ bgcolor: '#1d4ed8', color: '#fff' }}>{(inq.name ?? 'L')[0]}</Avatar>
              <Box sx={{ flexGrow: 1 }}>
                <Typography fontWeight={700}>{inq.name ?? 'Lead sin nombre'}</Typography>
                <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 0.5 }}>
                  {inq.course && <Chip icon={<TagIcon fontSize="small" />} label={inq.course} size="small" />}
                  {inq.channel && <Chip icon={<CampaignIcon fontSize="small" />} label={inq.channel} size="small" variant="outlined" />}
                  {inq.email && <Chip icon={<EmailIcon fontSize="small" />} label={inq.email} size="small" variant="outlined" />}
                  {inq.phone && <Chip icon={<PhoneIphoneIcon fontSize="small" />} label={inq.phone} size="small" variant="outlined" />}
                  <Chip label={inq.status} size="small" color="primary" variant="outlined" />
                </Stack>
                {inq.message && (
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                    {inq.message}
                  </Typography>
                )}
              </Box>
              <Typography variant="body2" color="text.secondary">
                {new Date(inq.createdAt).toLocaleString('es-EC', { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })}
              </Typography>
            </Stack>
          </Paper>
        ))}
      </Stack>
    </Stack>
  );
}
