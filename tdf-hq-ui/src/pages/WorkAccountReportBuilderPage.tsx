import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  Grid,
  IconButton,
  Paper,
  Stack,
  TextField,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
} from '@mui/material';
import AddCircleOutlineIcon from '@mui/icons-material/AddCircleOutline';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DownloadIcon from '@mui/icons-material/Download';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import PageShell from '../components/PageShell';
import {
  buildWorkAccountReport,
  buildWorkAccountReportPdfBlob,
  formatCurrency,
  formatDateLabel,
  formatDuration,
  type WorkAccountReportSource,
  type WorkBlockSource,
} from '../features/finance/davidCelayaReport';

type BuilderBlock = WorkBlockSource;

const DRAFT_SOURCE: WorkAccountReportSource = {
  personName: 'David Celaya',
  reportTitle: 'Reporte de cuenta',
  periodLabel: '9 y 10 de julio de 2026',
  hourlyRateCents: 2_500,
  notes: 'Bloques de trabajo creados desde el sistema.',
  workBlocks: [
    {
      id: 'block-1',
      date: '2026-07-09',
      startTime: '14:35:10',
      endTime: '20:43:50',
      startPhotoLabel: 'Foto inicio 1',
      endPhotoLabel: 'Foto fin 1',
      description: 'Bloque continuo en estudio.',
      discountHours: 1,
      discountReason: 'Descuento explícito de 1 hora.',
    },
    {
      id: 'block-2',
      date: '2026-07-10',
      startTime: '17:37:28',
      endTime: '23:50:32',
      startPhotoLabel: 'Foto inicio 2',
      endPhotoLabel: 'Foto fin 2',
      description: 'Bloque nocturno en estudio.',
      discountHours: 2,
      discountReason: 'Descuento explícito de 2 horas.',
    },
  ],
};

const makeId = () => `block-${Math.random().toString(36).slice(2, 8)}`;

const cloneBlocks = (blocks: BuilderBlock[]) => blocks.map((block) => ({ ...block, id: block.id || makeId() }));

const emptyBlock = (): BuilderBlock => ({
  id: makeId(),
  date: new Date().toISOString().slice(0, 10) as `${number}-${number}-${number}`,
  startTime: '09:00:00',
  endTime: '10:00:00',
  startPhotoLabel: 'Foto inicio',
  endPhotoLabel: 'Foto fin',
  description: 'Bloque de trabajo.',
  discountHours: 0,
  discountReason: 'Sin descuento.',
});

export default function WorkAccountReportBuilderPage() {
  const [personName, setPersonName] = useState(DRAFT_SOURCE.personName);
  const [reportTitle, setReportTitle] = useState(DRAFT_SOURCE.reportTitle);
  const [periodLabel, setPeriodLabel] = useState(DRAFT_SOURCE.periodLabel);
  const [hourlyRate, setHourlyRate] = useState('25');
  const [notes, setNotes] = useState(DRAFT_SOURCE.notes);
  const [asOfDate, setAsOfDate] = useState('2026-07-10');
  const [blocks, setBlocks] = useState<BuilderBlock[]>(() => cloneBlocks(DRAFT_SOURCE.workBlocks));
  const [pdfUrl, setPdfUrl] = useState('');

  const source = useMemo<WorkAccountReportSource>(
    () => ({
      personName: personName.trim() || 'Sin nombre',
      reportTitle: reportTitle.trim() || 'Reporte de cuenta',
      periodLabel: periodLabel.trim() || 'Sin periodo',
      hourlyRateCents: Math.max(0, Math.round(Number.parseFloat(hourlyRate.replace(',', '.')) * 100) || 0),
      notes: notes.trim() || 'Sin notas.',
      workBlocks: blocks.map((block) => ({
        ...block,
        discountHours: Math.max(0, Number.isFinite(Number(block.discountHours)) ? Number(block.discountHours) : 0),
        discountReason: block.discountReason.trim() || 'Sin descuento.',
      })),
    }),
    [blocks, hourlyRate, notes, periodLabel, personName, reportTitle],
  );

  const report = useMemo(() => buildWorkAccountReport(source, { asOfDate }), [source, asOfDate]);

  useEffect(() => {
    const url = URL.createObjectURL(buildWorkAccountReportPdfBlob(report));
    setPdfUrl(url);
    return () => URL.revokeObjectURL(url);
  }, [report]);

  const loadExample = () => {
    setPersonName(DRAFT_SOURCE.personName);
    setReportTitle(DRAFT_SOURCE.reportTitle);
    setPeriodLabel(DRAFT_SOURCE.periodLabel);
    setHourlyRate('25');
    setNotes(DRAFT_SOURCE.notes);
    setAsOfDate('2026-07-10');
    setBlocks(cloneBlocks(DRAFT_SOURCE.workBlocks));
  };

  const addBlock = () => setBlocks((prev) => [...prev, emptyBlock()]);

  const updateBlock = (id: string, patch: Partial<BuilderBlock>) => {
    setBlocks((prev) => prev.map((block) => (block.id === id ? { ...block, ...patch } : block)));
  };

  const removeBlock = (id: string) => {
    setBlocks((prev) => prev.filter((block) => block.id !== id));
  };

  return (
    <PageShell
      title="Creador de reportes de cuenta"
      subtitle="Construye reportes de tiempo y cuenta dentro del sistema, sin tocar código."
    >
      <Stack spacing={3} sx={{ pb: 7 }}>
        <Alert severity="info">
          Este generador crea reportes del mismo tipo que el de David Celaya, con bloques editables, descuentos explícitos y PDF descargable.
        </Alert>

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={2} flexWrap="wrap">
                <Box>
                  <Typography variant="h6" fontWeight={850}>Datos del reporte</Typography>
                  <Typography color="text.secondary">Modifica nombre, periodo, tarifa y notas generales.</Typography>
                </Box>
                <Stack direction="row" spacing={1}>
                  <Button startIcon={<ContentCopyIcon />} variant="outlined" onClick={loadExample}>
                    Cargar ejemplo
                  </Button>
                  <Button startIcon={<DownloadIcon />} variant="contained" component="a" href={pdfUrl || undefined} download="reporte-cuenta.pdf" disabled={!pdfUrl}>
                    Descargar PDF
                  </Button>
                </Stack>
              </Stack>

              <Grid container spacing={2}>
                <Grid item xs={12} md={4}>
                  <TextField label="Nombre" value={personName} onChange={(e) => setPersonName(e.target.value)} fullWidth />
                </Grid>
                <Grid item xs={12} md={4}>
                  <TextField label="Título" value={reportTitle} onChange={(e) => setReportTitle(e.target.value)} fullWidth />
                </Grid>
                <Grid item xs={12} md={4}>
                  <TextField label="Periodo" value={periodLabel} onChange={(e) => setPeriodLabel(e.target.value)} fullWidth />
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField label="Tarifa USD/h" value={hourlyRate} onChange={(e) => setHourlyRate(e.target.value)} fullWidth />
                </Grid>
                <Grid item xs={12} md={3}>
                  <TextField label="Corte" type="date" value={asOfDate} onChange={(e) => setAsOfDate(e.target.value)} InputLabelProps={{ shrink: true }} fullWidth />
                </Grid>
                <Grid item xs={12} md={6}>
                  <TextField label="Notas" value={notes} onChange={(e) => setNotes(e.target.value)} fullWidth multiline minRows={2} />
                </Grid>
              </Grid>
            </Stack>
          </CardContent>
        </Card>

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <Stack direction="row" justifyContent="space-between" alignItems="center">
                <Box>
                  <Typography variant="h6" fontWeight={850}>Bloques de trabajo</Typography>
                  <Typography color="text.secondary">Agrega tantos bloques como necesites.</Typography>
                </Box>
                <Button startIcon={<AddCircleOutlineIcon />} onClick={addBlock} variant="outlined">
                  Agregar bloque
                </Button>
              </Stack>

              <Stack spacing={2}>
                {blocks.map((block, index) => (
                  <Paper key={block.id} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                    <Stack spacing={2}>
                      <Stack direction="row" justifyContent="space-between" alignItems="center">
                        <Typography fontWeight={800}>Bloque {index + 1}</Typography>
                        <IconButton onClick={() => removeBlock(block.id)} aria-label="Eliminar bloque">
                          <DeleteOutlineIcon />
                        </IconButton>
                      </Stack>
                      <Grid container spacing={2}>
                        <Grid item xs={12} md={3}>
                          <TextField label="Fecha" type="date" value={block.date} onChange={(e) => updateBlock(block.id, { date: e.target.value as BuilderBlock['date'] })} InputLabelProps={{ shrink: true }} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={3}>
                          <TextField label="Inicio" value={block.startTime} onChange={(e) => updateBlock(block.id, { startTime: e.target.value })} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={3}>
                          <TextField label="Fin" value={block.endTime} onChange={(e) => updateBlock(block.id, { endTime: e.target.value })} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={3}>
                          <TextField label="Descuento horas" type="number" value={String(block.discountHours)} onChange={(e) => updateBlock(block.id, { discountHours: Number.parseInt(e.target.value || '0', 10) || 0 })} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={4}>
                          <TextField label="Foto inicio" value={block.startPhotoLabel} onChange={(e) => updateBlock(block.id, { startPhotoLabel: e.target.value })} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={4}>
                          <TextField label="Foto fin" value={block.endPhotoLabel} onChange={(e) => updateBlock(block.id, { endPhotoLabel: e.target.value })} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={4}>
                          <TextField label="Razón del descuento" value={block.discountReason} onChange={(e) => updateBlock(block.id, { discountReason: e.target.value })} fullWidth />
                        </Grid>
                        <Grid item xs={12}>
                          <TextField label="Descripción" value={block.description} onChange={(e) => updateBlock(block.id, { description: e.target.value })} fullWidth multiline minRows={2} />
                        </Grid>
                      </Grid>
                    </Stack>
                  </Paper>
                ))}
              </Stack>
            </Stack>
          </CardContent>
        </Card>

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2}>
              <Typography variant="h6" fontWeight={850}>Vista previa</Typography>
              <Grid container spacing={2}>
                <Grid item xs={12} md={3}>
                  <PreviewMetric label="Horas netas" value={`${report.totalBillableHours}h`} />
                </Grid>
                <Grid item xs={12} md={3}>
                  <PreviewMetric label="Descuento total" value={`${report.totalDiscountHours}h`} />
                </Grid>
                <Grid item xs={12} md={3}>
                  <PreviewMetric label="Importe neto" value={formatCurrency(report.totalBillableCents)} />
                </Grid>
                <Grid item xs={12} md={3}>
                  <PreviewMetric label="Duración total" value={formatDuration(report.totalDurationSeconds)} />
                </Grid>
              </Grid>

              <Divider />

              <TableContainer>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>Fecha</TableCell>
                      <TableCell>Inicio</TableCell>
                      <TableCell>Fin</TableCell>
                      <TableCell align="right">Base</TableCell>
                      <TableCell align="right">Desc.</TableCell>
                      <TableCell align="right">Neto</TableCell>
                      <TableCell align="right">Importe</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {report.workBlocks.map((block) => (
                      <TableRow key={block.id}>
                        <TableCell>{formatDateLabel(block.date)}</TableCell>
                        <TableCell>{block.startTime}</TableCell>
                        <TableCell>{block.endTime}</TableCell>
                        <TableCell align="right">{`${block.billableHours}h`}</TableCell>
                        <TableCell align="right">{`${block.discountHours}h`}</TableCell>
                        <TableCell align="right">{`${block.netBillableHours}h`}</TableCell>
                        <TableCell align="right">{block.billableLabel}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </TableContainer>
            </Stack>
          </CardContent>
        </Card>
      </Stack>
    </PageShell>
  );
}

function PreviewMetric({ label, value }: { label: string; value: string }) {
  return (
    <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
      <Stack spacing={0.5}>
        <Typography variant="body2" color="text.secondary">{label}</Typography>
        <Typography variant="h6" fontWeight={850}>{value}</Typography>
      </Stack>
    </Paper>
  );
}
