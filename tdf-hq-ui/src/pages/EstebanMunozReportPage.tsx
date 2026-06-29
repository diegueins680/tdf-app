import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Chip,
  Divider,
  Grid,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import AccountBalanceWalletIcon from '@mui/icons-material/AccountBalanceWallet';
import CalculateIcon from '@mui/icons-material/Calculate';
import DownloadIcon from '@mui/icons-material/Download';
import LocalOfferIcon from '@mui/icons-material/LocalOffer';
import PrintIcon from '@mui/icons-material/Print';
import ReceiptLongIcon from '@mui/icons-material/ReceiptLong';
import SchoolIcon from '@mui/icons-material/School';
import PageShell from '../components/PageShell';
import {
  ESTEBAN_MUNOZ_REPORT_PDF_FILENAME,
  buildEstebanMunozReport,
  buildEstebanMunozReportPdfBlob,
  formatDateLabel,
  formatMonthLabel,
  type AccountDirection,
} from '../features/finance/estebanMunozReport';

const money = (cents: number) =>
  new Intl.NumberFormat('es-EC', {
    style: 'currency',
    currency: 'USD',
    maximumFractionDigits: 2,
    minimumFractionDigits: 2,
  }).format(cents / 100).replace('USD', '$');

const directionCopy: Record<AccountDirection, { label: string; color: 'default' | 'error' | 'success' }> = {
  esteban_owes_tdf: { label: 'Esteban debe a TDF', color: 'error' },
  tdf_owes_esteban: { label: 'TDF debe a Esteban', color: 'success' },
  settled: { label: 'Registrado', color: 'default' },
};

const metricToneColor = (tone?: 'debt' | 'credit' | 'neutral') => {
  if (tone === 'debt') return 'error.main';
  if (tone === 'credit') return 'success.main';
  return 'text.primary';
};

const netMetricTone = (direction: AccountDirection) => {
  if (direction === 'esteban_owes_tdf') return 'debt';
  if (direction === 'tdf_owes_esteban') return 'credit';
  return 'neutral';
};

function Metric({
  icon,
  label,
  value,
  tone,
}: {
  icon: ReactNode;
  label: string;
  value: string;
  tone?: 'debt' | 'credit' | 'neutral';
}) {
  const toneColor = metricToneColor(tone);

  return (
    <Paper variant="outlined" sx={{ p: 2, height: '100%', borderRadius: 2 }}>
      <Stack spacing={1}>
        <Box sx={{ color: toneColor, display: 'flex' }}>{icon}</Box>
        <Typography variant="body2" color="text.secondary">{label}</Typography>
        <Typography variant="h5" fontWeight={800} color={toneColor}>{value}</Typography>
      </Stack>
    </Paper>
  );
}

export default function EstebanMunozReportPage() {
  const report = useMemo(() => buildEstebanMunozReport(), []);
  const [pdfUrl, setPdfUrl] = useState('');
  const { source } = report;
  const netDirection = directionCopy[report.netDirection];

  useEffect(() => {
    const url = URL.createObjectURL(buildEstebanMunozReportPdfBlob(report));
    setPdfUrl(url);
    return () => URL.revokeObjectURL(url);
  }, [report]);

  return (
    <PageShell
      title="Reporte Esteban Muñoz"
      subtitle={`Estado de cuentas con TDF al ${formatDateLabel(source.cutoffDate)}.`}
    >
      <Stack spacing={3} sx={{ '@media print': { '& .print-hidden': { display: 'none' } } }}>
        <Stack className="print-hidden" direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="space-between">
          <Alert severity="info" sx={{ flex: 1 }}>
            Reporte interno. La compensación de saldos no está ejecutada; requiere aprobación contable antes de liquidar.
          </Alert>
          <Stack direction="row" spacing={1}>
            <Button
              variant="outlined"
              startIcon={<ReceiptLongIcon />}
              component={RouterLink}
              to="/finanzas/pagos"
            >
              Pagos
            </Button>
            <Button
              variant="outlined"
              startIcon={<DownloadIcon />}
              component="a"
              href={pdfUrl || undefined}
              download={ESTEBAN_MUNOZ_REPORT_PDF_FILENAME}
              disabled={!pdfUrl}
            >
              Descargar PDF
            </Button>
            <Button variant="contained" startIcon={<PrintIcon />} onClick={() => window.print()}>
              Imprimir
            </Button>
          </Stack>
        </Stack>

        <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2 }}>
          <Stack spacing={2.5}>
            <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" spacing={2}>
              <Box>
                <Typography variant="overline" color="text.secondary">Cuenta personal TDF</Typography>
                <Typography variant="h4" fontWeight={900}>{source.personName}</Typography>
                <Typography color="text.secondary">
                  Último mes pagado: {formatMonthLabel(source.rent.lastPaidMonth)}. Corte de arriendo: {formatMonthLabel(source.rent.throughMonth)}.
                </Typography>
              </Box>
              <Box sx={{ textAlign: { xs: 'left', md: 'right' } }}>
                <Chip label={netDirection.label} color={netDirection.color} sx={{ mb: 1 }} />
                <Typography variant="h3" fontWeight={900}>
                  {money(Math.abs(report.netAfterOffsetCents))}
                </Typography>
                <Typography color="text.secondary">Saldo neto si se compensa</Typography>
              </Box>
            </Stack>

            <Grid container spacing={2}>
              <Grid item xs={12} md={3}>
                <Metric
                  icon={<AccountBalanceWalletIcon />}
                  label="Arriendo pendiente"
                  value={money(report.rentDueCents)}
                  tone="debt"
                />
              </Grid>
              <Grid item xs={12} md={3}>
                <Metric
                  icon={<SchoolIcon />}
                  label="Clases de producción"
                  value={money(report.coursePayableCents)}
                  tone="credit"
                />
              </Grid>
              <Grid item xs={12} md={3}>
                <Metric
                  icon={<LocalOfferIcon />}
                  label="Promoción masters"
                  value={money(report.promotionShareRow.estebanShareCents)}
                  tone="credit"
                />
              </Grid>
              <Grid item xs={12} md={3}>
                <Metric
                  icon={<CalculateIcon />}
                  label="Neto después de compensar"
                  value={money(Math.abs(report.netAfterOffsetCents))}
                  tone={netMetricTone(report.netDirection)}
                />
              </Grid>
            </Grid>
          </Stack>
        </Paper>

        <Grid container spacing={2}>
          <Grid item xs={12} lg={7}>
            <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2, height: '100%' }}>
              <Stack spacing={2}>
                <Box>
                  <Typography variant="h6" fontWeight={800}>Arriendo mensual</Typography>
                  <Typography color="text.secondary">
                    {money(source.rent.monthlyAmountCents)} por mes. El comprobante adjunto se toma como pago aplicado hasta {formatMonthLabel(source.rent.lastPaidMonth)}.
                  </Typography>
                </Box>
                <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                  {report.unpaidRentMonths.map((month) => (
                    <Chip key={month} label={formatMonthLabel(month)} color="error" variant="outlined" />
                  ))}
                </Stack>
                <Divider />
                <Table size="small">
                  <TableHead>
                    <TableRow>
                      <TableCell>Concepto</TableCell>
                      <TableCell align="right">Cantidad</TableCell>
                      <TableCell align="right">Valor unitario</TableCell>
                      <TableCell align="right">Subtotal</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    <TableRow>
                      <TableCell>Meses pendientes de renta</TableCell>
                      <TableCell align="right">{report.unpaidRentMonths.length}</TableCell>
                      <TableCell align="right">{money(source.rent.monthlyAmountCents)}</TableCell>
                      <TableCell align="right">{money(report.rentDueCents)}</TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </Stack>
            </Paper>
          </Grid>

          <Grid item xs={12} lg={5}>
            <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2, height: '100%' }}>
              <Stack spacing={2}>
                <Box>
                  <Typography variant="h6" fontWeight={800}>Comprobante base</Typography>
                  <Typography color="text.secondary">{source.lastReceipt.source}</Typography>
                </Box>
                <Table size="small">
                  <TableBody>
                    <TableRow>
                      <TableCell>Fecha</TableCell>
                      <TableCell align="right">{source.lastReceipt.originalDateLabel} ({formatDateLabel(source.lastReceipt.date)})</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell>No. comprobante</TableCell>
                      <TableCell align="right">{source.lastReceipt.receiptNumber}</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell>Valor transferido</TableCell>
                      <TableCell align="right">{money(source.lastReceipt.amountCents)}</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell>Descripción</TableCell>
                      <TableCell align="right">{source.lastReceipt.description}</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell>Destino</TableCell>
                      <TableCell align="right">{source.lastReceipt.financialEntity} · {source.lastReceipt.destinationAccount}</TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </Stack>
            </Paper>
          </Grid>
        </Grid>

        <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2 }}>
          <Stack spacing={2}>
            <Box>
              <Typography variant="h6" fontWeight={800}>Honorarios por clases de producción</Typography>
              <Typography color="text.secondary">
                Tarifa aplicada: {money(source.coursePayment.hourlyRateCents)} por hora. Total: 2 cursos de 16 horas cada uno.
              </Typography>
            </Box>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Curso</TableCell>
                  <TableCell>Fechas registradas</TableCell>
                  <TableCell align="right">Horas</TableCell>
                  <TableCell align="right">Tarifa</TableCell>
                  <TableCell align="right">Subtotal</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {report.courseRows.map((course) => (
                  <TableRow key={course.slug}>
                    <TableCell>
                      <Typography fontWeight={700}>{course.title}</Typography>
                      <Typography variant="body2" color="text.secondary">{course.sourceLabel}</Typography>
                    </TableCell>
                    <TableCell>
                      {course.sessionDates.map(formatDateLabel).join(', ')}
                    </TableCell>
                    <TableCell align="right">{course.hours}</TableCell>
                    <TableCell align="right">{money(source.coursePayment.hourlyRateCents)}</TableCell>
                    <TableCell align="right">{money(course.subtotalCents)}</TableCell>
                  </TableRow>
                ))}
                <TableRow>
                  <TableCell colSpan={4}>
                    <Typography fontWeight={800}>Total honorarios</Typography>
                  </TableCell>
                  <TableCell align="right">
                    <Typography fontWeight={800}>{money(report.coursePayableCents)}</Typography>
                  </TableCell>
                </TableRow>
              </TableBody>
            </Table>
          </Stack>
        </Paper>

        <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2 }}>
          <Stack spacing={2}>
            <Box>
              <Typography variant="h6" fontWeight={800}>Participación por promoción de masters</Typography>
              <Typography color="text.secondary">
                {source.promotionShare.sourceLabel}. A Esteban le corresponde el {source.promotionShare.estebanSharePercent}% del pago recibido.
              </Typography>
            </Box>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Cliente</TableCell>
                  <TableCell>Concepto</TableCell>
                  <TableCell align="right">Pago recibido</TableCell>
                  <TableCell align="right">Participación</TableCell>
                  <TableCell align="right">A favor de Esteban</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                <TableRow>
                  <TableCell>{report.promotionShareRow.payerName}</TableCell>
                  <TableCell>{report.promotionShareRow.concept}</TableCell>
                  <TableCell align="right">{money(report.promotionShareRow.totalPaidCents)}</TableCell>
                  <TableCell align="right">{report.promotionShareRow.estebanSharePercent}%</TableCell>
                  <TableCell align="right">{money(report.promotionShareRow.estebanShareCents)}</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell colSpan={4}>
                    <Typography fontWeight={800}>Total promoción masters</Typography>
                  </TableCell>
                  <TableCell align="right">
                    <Typography fontWeight={800}>{money(report.promotionShareRow.estebanShareCents)}</Typography>
                  </TableCell>
                </TableRow>
              </TableBody>
            </Table>
          </Stack>
        </Paper>

        <Paper variant="outlined" sx={{ p: { xs: 2, md: 3 }, borderRadius: 2 }}>
          <Stack spacing={2}>
            <Box>
              <Typography variant="h6" fontWeight={800}>Cuentas mantenidas con TDF</Typography>
              <Typography color="text.secondary">
                Vista consolidada de saldos a cobrar, saldos a pagar y movimientos base.
              </Typography>
            </Box>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Cuenta</TableCell>
                  <TableCell>Concepto</TableCell>
                  <TableCell>Estado</TableCell>
                  <TableCell>Dirección</TableCell>
                  <TableCell align="right">Monto</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {report.accountPositions.map((item) => {
                  const copy = directionCopy[item.direction];
                  return (
                    <TableRow key={item.id}>
                      <TableCell>
                        <Typography fontWeight={700}>{item.account}</Typography>
                        <Typography variant="body2" color="text.secondary">{item.detail}</Typography>
                      </TableCell>
                      <TableCell>{item.concept}</TableCell>
                      <TableCell>{item.status}</TableCell>
                      <TableCell><Chip size="small" label={copy.label} color={copy.color} /></TableCell>
                      <TableCell align="right">{money(item.amountCents)}</TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </Stack>
        </Paper>
      </Stack>
    </PageShell>
  );
}
