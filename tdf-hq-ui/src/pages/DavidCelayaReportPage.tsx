import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  Alert,
  Box,
  Button,
  Chip,
  Grid,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import DownloadIcon from '@mui/icons-material/Download';
import EventNoteIcon from '@mui/icons-material/EventNote';
import PhotoLibraryIcon from '@mui/icons-material/PhotoLibrary';
import PrintIcon from '@mui/icons-material/Print';
import ScheduleIcon from '@mui/icons-material/Schedule';
import PageShell from '../components/PageShell';
import {
  DAVID_CELAYA_REPORT_PDF_FILENAME,
  buildDavidCelayaReport,
  buildDavidCelayaReportPdfBlob,
  formatDateLabel,
  formatDateTimeLabel,
  formatDuration,
  formatCurrency,
} from '../features/finance/davidCelayaReport';

const tableSx = {
  minWidth: 860,
  '& thead th': {
    bgcolor: 'action.hover',
    color: 'text.secondary',
    fontSize: 12,
    fontWeight: 800,
    textTransform: 'uppercase',
  },
  '& tbody td': {
    verticalAlign: 'top',
  },
  '& tbody tr:last-of-type td': {
    borderBottom: 0,
  },
} as const;

function Section({
  title,
  subtitle,
  children,
}: {
  title: string;
  subtitle?: string;
  children: ReactNode;
}) {
  return (
    <Paper variant="outlined" sx={{ p: { xs: 2, md: 2.5 }, borderRadius: 2 }}>
      <Stack spacing={2}>
        <Box>
          <Typography variant="h6" fontWeight={850}>{title}</Typography>
          {subtitle && <Typography color="text.secondary">{subtitle}</Typography>}
        </Box>
        {children}
      </Stack>
    </Paper>
  );
}

function Metric({
  icon,
  label,
  value,
  caption,
}: {
  icon: ReactNode;
  label: string;
  value: string;
  caption?: string;
}) {
  return (
    <Box
      sx={{
        height: '100%',
        minHeight: 96,
        p: 2,
        border: 1,
        borderColor: 'divider',
        borderRadius: 2,
        bgcolor: 'background.paper',
      }}
    >
      <Stack spacing={1}>
        <Box sx={{ color: 'primary.main', display: 'flex' }}>{icon}</Box>
        <Typography variant="body2" color="text.secondary">{label}</Typography>
        <Typography variant="h5" fontWeight={850} color="primary.main">{value}</Typography>
        {caption && <Typography variant="caption" color="text.secondary">{caption}</Typography>}
      </Stack>
    </Box>
  );
}

export default function DavidCelayaReportPage() {
  const report = useMemo(() => buildDavidCelayaReport(), []);
  const [pdfUrl, setPdfUrl] = useState('');
  const { source } = report;

  useEffect(() => {
    const url = URL.createObjectURL(buildDavidCelayaReportPdfBlob(report));
    setPdfUrl(url);
    return () => URL.revokeObjectURL(url);
  }, [report]);

  return (
    <PageShell
      title="Reporte de cuenta David Celaya"
      subtitle="Bloques de trabajo consolidados a partir de las capturas de cámara compartidas."
    >
      <Stack spacing={3} sx={{ pb: 7, '@media print': { '& .print-hidden': { display: 'none' } } }}>
        <Stack
          className="print-hidden"
          direction={{ xs: 'column', md: 'row' }}
          spacing={1.5}
          justifyContent="space-between"
          alignItems={{ xs: 'stretch', md: 'center' }}
        >
          <Alert severity="info" sx={{ flex: 1 }}>
            Reporte interno de tiempo. Las fotos se usaron como límites de bloque y la tarifa aplicada es de USD 25 por hora, redondeando cada bloque al entero más cercano.
          </Alert>
          <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
            <Button
              variant="outlined"
              startIcon={<DownloadIcon />}
              component="a"
              href={pdfUrl || undefined}
              download={DAVID_CELAYA_REPORT_PDF_FILENAME}
              disabled={!pdfUrl}
            >
              Descargar PDF
            </Button>
            <Button variant="contained" startIcon={<PrintIcon />} onClick={() => window.print()}>
              Imprimir
            </Button>
          </Stack>
        </Stack>

        <Paper variant="outlined" sx={{ p: { xs: 2.5, md: 3 }, borderRadius: 2 }}>
          <Stack spacing={3}>
            <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" spacing={2.5}>
              <Box>
                <Typography variant="overline" color="text.secondary">Cuenta de trabajo</Typography>
                <Typography variant="h4" fontWeight={900}>{source.personName}</Typography>
                <Typography color="text.secondary">
                  Periodo consolidado: {source.periodLabel}. Corte del reporte: {formatDateLabel(source.cutoffDate)}.
                </Typography>
              </Box>
              <Box sx={{ textAlign: { xs: 'left', md: 'right' }, minWidth: 220 }}>
                <Chip label="Tiempo registrado" color="primary" sx={{ mb: 1, fontWeight: 800 }} />
                <Typography variant="h3" fontWeight={900}>
                  {formatCurrency(report.totalBillableCents)}
                </Typography>
                <Typography color="text.secondary">
                  {report.totalBillableHours}h facturables netas, 4 capturas de evidencia
                </Typography>
              </Box>
            </Stack>

            <Grid container spacing={1.5}>
              <Grid item xs={12} sm={6} md={4}>
                <Metric
                  icon={<EventNoteIcon />}
                  label="Bloques de trabajo"
                  value={String(report.workBlocks.length)}
                  caption="Dos jornadas separadas por fecha."
                />
              </Grid>
              <Grid item xs={12} sm={6} md={4}>
                <Metric
                  icon={<PhotoLibraryIcon />}
                  label="Capturas de respaldo"
                  value={String(report.evidenceCount)}
                  caption="Dos fotos por bloque."
                />
              </Grid>
              <Grid item xs={12} sm={6} md={4}>
                <Metric
                  icon={<AccessTimeIcon />}
                  label="Duración total"
                  value={formatDuration(report.totalDurationSeconds)}
                  caption="Suma de ambos bloques."
                />
              </Grid>
              <Grid item xs={12} sm={6} md={6}>
                <Metric
                  icon={<ScheduleIcon />}
                  label="Promedio por bloque"
                  value={formatDuration(report.averageDurationSeconds)}
                  caption="Referencia operativa."
                />
              </Grid>
              <Grid item xs={12} sm={6} md={6}>
                <Metric
                  icon={<AccessTimeIcon />}
                  label="Total facturable"
                  value={formatCurrency(report.totalBillableCents)}
                  caption="Tarifa USD 25/h con descuento explícito por bloque."
                />
              </Grid>
              <Grid item xs={12} sm={6} md={6}>
                <Metric
                  icon={<AccessTimeIcon />}
                  label="Total descontado"
                  value={`${report.totalDiscountHours}h`}
                  caption={`${formatCurrency(report.totalDiscountCents)} descontados en total.`}
                />
              </Grid>
            </Grid>
          </Stack>
        </Paper>

        <Section
          title="Bloques registrados"
          subtitle="Los tiempos visibles en las capturas se usaron para abrir y cerrar cada bloque de trabajo."
        >
          <TableContainer>
            <Table sx={tableSx}>
              <TableHead>
                <TableRow>
                  <TableCell>Fecha</TableCell>
                  <TableCell>Inicio</TableCell>
                  <TableCell>Fin</TableCell>
                  <TableCell align="right">Duración</TableCell>
                  <TableCell align="right">Horas base</TableCell>
                  <TableCell align="right">Descuento</TableCell>
                  <TableCell align="right">Horas netas</TableCell>
                  <TableCell align="right">Tarifa</TableCell>
                  <TableCell align="right">Importe</TableCell>
                  <TableCell align="center">Evidencia</TableCell>
                  <TableCell>Nota</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {report.workBlocks.map((block) => (
                  <TableRow key={block.id}>
                    <TableCell>
                      <Typography fontWeight={800}>{formatDateLabel(block.date)}</Typography>
                    </TableCell>
                    <TableCell>
                      <Typography fontWeight={700}>{formatDateTimeLabel(block.startDateTime)}</Typography>
                      <Typography variant="body2" color="text.secondary">{block.startPhotoLabel}</Typography>
                    </TableCell>
                    <TableCell>
                      <Typography fontWeight={700}>{formatDateTimeLabel(block.endDateTime)}</Typography>
                      <Typography variant="body2" color="text.secondary">{block.endPhotoLabel}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography fontWeight={800}>{block.durationLabel}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography variant="body2">{`${block.billableHours}h`}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography variant="body2">{block.discountLabel}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography fontWeight={800}>{`${block.netBillableHours}h`}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography variant="body2">{formatCurrency(source.hourlyRateCents)}</Typography>
                    </TableCell>
                    <TableCell align="right">
                      <Typography fontWeight={800}>{block.billableLabel}</Typography>
                    </TableCell>
                    <TableCell align="center">
                      <Chip label="2 fotos" size="small" variant="outlined" />
                    </TableCell>
                    <TableCell>
                      <Stack spacing={0.5}>
                        <Typography variant="body2">{block.description}</Typography>
                        <Typography variant="caption" color="text.secondary">
                          {block.discountReason}
                        </Typography>
                      </Stack>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>
        </Section>

        <Section title="Cierre">
          <Stack spacing={1}>
            <Typography>
              Primer registro: <strong>{report.firstStart ? formatDateTimeLabel(report.firstStart) : 'N/D'}</strong>.
            </Typography>
            <Typography>
              Último cierre: <strong>{report.lastEnd ? formatDateTimeLabel(report.lastEnd) : 'N/D'}</strong>.
            </Typography>
            <Typography color="text.secondary">
              Cálculo monetario: {formatCurrency(source.hourlyRateCents)} por hora, con descuento explícito por bloque.
            </Typography>
            <Typography color="text.secondary">
              Descuento acumulado: {report.totalDiscountHours}h = {formatCurrency(report.totalDiscountCents)}.
            </Typography>
          </Stack>
        </Section>
      </Stack>
    </PageShell>
  );
}
