import { useState, useCallback, useEffect, useMemo, useRef } from 'react';
import { useMetaTags } from '../hooks/useMetaTags';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  Chip,
  Container,
  Divider,
  FormControl,
  Grid,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  type SelectChangeEvent,
  Stack,
  Step,
  StepLabel,
  Stepper,
  TextField,
  Typography,
  Link,
  Snackbar,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
} from '@mui/material';
import {
  MusicNote,
  CheckCircle,
  ExpandMore,
  ArrowBack,
  ArrowForward,
  Headphones,
  Mic,
  Album,
  Timer,
  Refresh,
  Security,
  Speed,
} from '@mui/icons-material';
import { useQuery } from '@tanstack/react-query';
import {
  ServiceStorefront,
  type ServiceStorefrontOrderDTO,
  type ServiceStorefrontPackageDTO,
} from '../api/serviceStorefront';
import ExperienceReviews from '../components/reviews/ExperienceReviews';

const IMPORT_META_ENV = (import.meta as unknown as { env?: Record<string, string | undefined> }).env ?? {};

// Service package interface (matches API DTO shape)
interface ServicePackage {
  id: string;
  serviceKind: 'Mixing' | 'Mastering' | 'Bundle';
  tier: 'Basic' | 'Pro' | 'Premium';
  name: string;
  description: string;
  priceUsdCents: number;
  currency: string;
  minSongCount: number;
  maxSongCount: number;
  turnaroundDays: number;
  revisionCount: number;
  deliverables: string[];
  features: string[];
}

// Map API DTO to local interface
const mapPackageDTO = (dto: ServiceStorefrontPackageDTO): ServicePackage => ({
  id: dto.sspId,
  serviceKind: dto.sspServiceKind as ServicePackage['serviceKind'],
  tier: dto.sspTier as ServicePackage['tier'],
  name: dto.sspName,
  description: dto.sspDescription ?? '',
  priceUsdCents: dto.sspPriceUsdCents,
  currency: dto.sspCurrency,
  minSongCount: dto.sspMinSongCount,
  maxSongCount: dto.sspMaxSongCount,
  turnaroundDays: dto.sspTurnaroundDays,
  revisionCount: dto.sspRevisionCount,
  deliverables: dto.sspDeliverables ?? [],
  features: dto.sspFeatures ?? [],
});

const formatPrice = (cents: number): string => {
  return `$${(cents / 100).toFixed(0)}`;
};

const serviceKindLabel = (kind: string): string => {
  switch (kind) {
    case 'Mixing': return 'Mezcla';
    case 'Mastering': return 'Mastering';
    case 'Bundle': return 'Paquete';
    default: return kind;
  }
};

const serviceKindIcon = (kind: string) => {
  switch (kind) {
    case 'Mixing': return <Headphones />;
    case 'Mastering': return <Mic />;
    case 'Bundle': return <Album />;
    default: return <MusicNote />;
  }
};

const tierColor = (tier: string): 'default' | 'primary' | 'secondary' => {
  switch (tier) {
    case 'Pro': return 'primary';
    case 'Premium': return 'secondary';
    default: return 'default';
  }
};

type ServiceFilter = 'all' | 'Mixing' | 'Mastering' | 'Bundle';
type OrderStep = 'select' | 'details' | 'payment' | 'confirmation';

interface OrderFormData {
  buyerName: string;
  buyerEmail: string;
  buyerPhone: string;
  artistName: string;
  genre: string;
  songCount: number;
  notes: string;
  referenceTrackUrl: string;
}

const initialFormData: OrderFormData = {
  buyerName: '',
  buyerEmail: '',
  buyerPhone: '',
  artistName: '',
  genre: '',
  songCount: 1,
  notes: '',
  referenceTrackUrl: '',
};

export default function MixingMasteringPage() {
  useMetaTags({
    title: 'Mezcla y Masterización',
    description: 'Servicios profesionales de mezcla y masterización en TDF Records.',
  });

  const [serviceFilter, setServiceFilter] = useState<ServiceFilter>('all');
  const [selectedPackage, setSelectedPackage] = useState<ServicePackage | null>(null);
  const [currentStep, setCurrentStep] = useState<OrderStep>('select');
  const [formData, setFormData] = useState<OrderFormData>(initialFormData);
  const [orderNumber, setOrderNumber] = useState<string | null>(null);
  const [lookupToken, setLookupToken] = useState<string | null>(null);
  const [confirmedOrder, setConfirmedOrder] = useState<ServiceStorefrontOrderDTO | null>(null);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [datafastDialogOpen, setDatafastDialogOpen] = useState(false);
  const [datafastCheckout, setDatafastCheckout] = useState<Awaited<ReturnType<typeof ServiceStorefront.createDatafastCheckout>> | null>(null);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const [datafastError, setDatafastError] = useState<string | null>(null);
  const datafastFormRef = useRef<HTMLDivElement>(null);
  const [paypalDialogOpen, setPaypalDialogOpen] = useState(false);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalOrder, setPaypalOrder] = useState<{ orderNumber: string; lookupToken: string; providerOrderId: string } | null>(null);
  const [paypalError, setPaypalError] = useState<string | null>(null);
  const paypalButtonRef = useRef<HTMLDivElement>(null);
  const paypalClientId = IMPORT_META_ENV['VITE_PAYPAL_CLIENT_ID']?.trim() ?? '';
  const [snackbar, setSnackbar] = useState<{ open: boolean; message: string; severity: 'success' | 'error' | 'info' }>({
    open: false,
    message: '',
    severity: 'info',
  });

  // Prices and quantity bounds are authoritative server configuration.
  const { data: apiPackages, isLoading: packagesLoading, isError: packagesError } = useQuery({
    queryKey: ['serviceStorefrontPackages'],
    queryFn: () => ServiceStorefront.listPackages(),
    staleTime: 5 * 60 * 1000, // 5 minutes
  });

  const packages = useMemo(() => (apiPackages ?? []).map(mapPackageDTO), [apiPackages]);

  const filteredPackages = useMemo(() => {
    if (serviceFilter === 'all') return packages;
    return packages.filter((p) => p.serviceKind === serviceFilter);
  }, [serviceFilter, packages]);

  const handleFilterChange = useCallback((event: SelectChangeEvent<ServiceFilter>) => {
    setServiceFilter(event.target.value as ServiceFilter);
  }, []);

  const handleSelectPackage = useCallback((pkg: ServicePackage) => {
    setSelectedPackage(pkg);
    setFormData((current) => ({ ...current, songCount: pkg.minSongCount }));
    setCurrentStep('details');
  }, []);

  const handleFormChange = useCallback((field: keyof OrderFormData) => (
    event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    const minSongs = selectedPackage?.minSongCount ?? 1;
    const maxSongs = selectedPackage?.maxSongCount ?? minSongs;
    const value = field === 'songCount'
      ? Math.min(maxSongs, Math.max(minSongs, parseInt(event.target.value) || minSongs))
      : event.target.value;
    setFormData((prev) => ({ ...prev, [field]: value }));
  }, [selectedPackage]);

  const createServiceOrder = useCallback(async (): Promise<{ order: ServiceStorefrontOrderDTO; token: string }> => {
    if (!selectedPackage) throw new Error('Selecciona un paquete para continuar.');
    if (!formData.buyerName.trim() || !formData.buyerEmail.trim()) {
      throw new Error('Por favor completa tu nombre y email.');
    }
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(formData.buyerEmail)) {
      throw new Error('Por favor ingresa un email válido.');
    }
    if (formData.songCount < selectedPackage.minSongCount || formData.songCount > selectedPackage.maxSongCount) {
      throw new Error(`Este paquete admite entre ${selectedPackage.minSongCount} y ${selectedPackage.maxSongCount} canciones.`);
    }
    const idempotencyStorageKey = `tdf-service-checkout:${selectedPackage.id}:${formData.buyerEmail.trim().toLowerCase()}`;
    const idempotencyKey = sessionStorage.getItem(idempotencyStorageKey) ?? crypto.randomUUID();
    sessionStorage.setItem(idempotencyStorageKey, idempotencyKey);
    const order = await ServiceStorefront.createOrder(idempotencyKey, {
      ssocPackageId: selectedPackage.id,
      ssocBuyerName: formData.buyerName.trim(),
      ssocBuyerEmail: formData.buyerEmail.trim(),
      ssocBuyerPhone: formData.buyerPhone.trim() || null,
      ssocArtistName: formData.artistName.trim() || null,
      ssocGenre: formData.genre.trim() || null,
      ssocSongCount: formData.songCount,
      ssocNotes: formData.notes.trim() || null,
      ssocReferenceTrackUrl: formData.referenceTrackUrl.trim() || null,
    });
    const token = order.ssoLookupToken?.trim();
    if (!token) throw new Error('El servidor no entregó acceso seguro al pedido. Contacta a soporte antes de pagar.');
    sessionStorage.setItem(`tdf-service-order:${order.ssoOrderNumber}`, token);
    sessionStorage.removeItem(idempotencyStorageKey);
    setOrderNumber(order.ssoOrderNumber);
    setLookupToken(token);
    return { order, token };
  }, [selectedPackage, formData]);

  const paymentError = useCallback((error: unknown) => {
    const message = error instanceof Error ? error.message : 'No se pudo iniciar el pago. No se confirmó ningún cobro.';
    setSnackbar({ open: true, message, severity: 'error' });
  }, []);

  const handleDatafastPayment = useCallback(async () => {
    setPaymentBusy(true);
    setDatafastError(null);
    try {
      const { order, token } = await createServiceOrder();
      const checkout = await ServiceStorefront.createDatafastCheckout(order.ssoOrderNumber, token);
      setDatafastCheckout(checkout);
      setDatafastDialogOpen(true);
    } catch (error) {
      paymentError(error);
    } finally {
      setPaymentBusy(false);
    }
  }, [createServiceOrder, paymentError]);

  const handlePaypalPayment = useCallback(async () => {
    if (!paypalClientId) {
      paymentError(new Error('PayPal no está habilitado para este comercio.'));
      return;
    }
    setPaymentBusy(true);
    setPaypalError(null);
    try {
      const { order, token } = await createServiceOrder();
      const providerOrder = await ServiceStorefront.createPaypalOrder(order.ssoOrderNumber, token);
      setPaypalOrder({
        orderNumber: order.ssoOrderNumber,
        lookupToken: token,
        providerOrderId: providerOrder.pcPaypalOrderId,
      });
      setPaypalDialogOpen(true);
    } catch (error) {
      paymentError(error);
    } finally {
      setPaymentBusy(false);
    }
  }, [createServiceOrder, paymentError, paypalClientId]);

  const handleManualPayment = useCallback(async () => {
    setPaymentBusy(true);
    try {
      const { order, token } = await createServiceOrder();
      const updated = await ServiceStorefront.selectManualPayment(order.ssoOrderNumber, token);
      setConfirmedOrder(updated);
      setCurrentStep('confirmation');
      setSnackbar({
        open: true,
        message: 'Pedido creado. La transferencia sigue pendiente de verificación manual.',
        severity: 'info',
      });
    } catch (error) {
      paymentError(error);
    } finally {
      setPaymentBusy(false);
    }
  }, [createServiceOrder, paymentError]);

  const datafastReturnUrl = useMemo(() => {
    if (!orderNumber || typeof window === 'undefined') return '';
    const url = new URL('/mezcla-mastering/pago-datafast', window.location.origin);
    url.searchParams.set('orderId', orderNumber);
    return url.toString();
  }, [orderNumber]);

  useEffect(() => {
    if (!datafastDialogOpen || !datafastCheckout || typeof window === 'undefined') return;
    if (datafastFormRef.current) datafastFormRef.current.innerHTML = '';
    window.wpwlOptions = { locale: 'es', style: 'card' };
    const script = document.createElement('script');
    script.src = datafastCheckout.dcWidgetUrl;
    script.async = true;
    script.onerror = () => setDatafastError('No se pudo cargar el formulario de Datafast. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [datafastCheckout, datafastDialogOpen, datafastWidgetKey]);

  useEffect(() => {
    if (!paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=USD`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setPaypalError('No se pudo cargar PayPal. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [paypalClientId]);

  useEffect(() => {
    if (!paypalDialogOpen || !paypalReady || !paypalOrder || !paypalButtonRef.current || !window.paypal) return;
    paypalButtonRef.current.innerHTML = '';
    const buttons = window.paypal.Buttons({
      createOrder: () => paypalOrder.providerOrderId,
      onApprove: async (data) => {
        if (data.orderID !== paypalOrder.providerOrderId) {
          setPaypalError('PayPal devolvió una referencia distinta. No se capturó el pago.');
          return;
        }
        try {
          const updated = await ServiceStorefront.capturePaypalOrder(
            data.orderID,
            paypalOrder.orderNumber,
            paypalOrder.lookupToken,
          );
          setConfirmedOrder(updated);
          setPaypalDialogOpen(false);
          setCurrentStep('confirmation');
          setSnackbar({
            open: true,
            message: updated.ssoStatus === 'paid'
              ? 'PayPal confirmó el pago en el servidor.'
              : 'PayPal respondió, pero el pago todavía no está confirmado.',
            severity: updated.ssoStatus === 'paid' ? 'success' : 'info',
          });
        } catch (error) {
          setPaypalError(error instanceof Error ? error.message : 'No se pudo verificar el pago con PayPal.');
        }
      },
      onCancel: () => setPaypalError('Cancelaste PayPal. El pedido continúa sin pago.'),
      onError: () => setPaypalError('PayPal no pudo procesar la operación. El pedido continúa sin pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => { buttons.close?.(); };
  }, [paypalDialogOpen, paypalOrder, paypalReady]);

  const handleReset = useCallback(() => {
    setSelectedPackage(null);
    setCurrentStep('select');
    setFormData(initialFormData);
    setOrderNumber(null);
    setLookupToken(null);
    setConfirmedOrder(null);
  }, []);

  const steps = ['Seleccionar Servicio', 'Detalles del Proyecto', 'Pago', 'Confirmación'];
  const stepIndex = currentStep === 'select' ? 0 : currentStep === 'details' ? 1 : currentStep === 'payment' ? 2 : 3;

  return (
    <Container maxWidth="lg" sx={{ py: 4 }}>
      {/* Header */}
      <Box sx={{ textAlign: 'center', mb: 6 }}>
        <Typography variant="h3" component="h1" gutterBottom fontWeight={700}>
          Servicios de Mezcla & Mastering
        </Typography>
        <Typography variant="h6" color="text.secondary" sx={{ maxWidth: 700, mx: 'auto' }}>
          Lleva tu música al siguiente nivel con nuestros ingenieros profesionales.
          Calidad de estudio, entrega digital, precios accesibles.
        </Typography>
      </Box>

      {/* Trust signals */}
      <Stack direction="row" spacing={3} justifyContent="center" sx={{ mb: 4, flexWrap: 'wrap', gap: 2 }}>
        <Chip icon={<Security />} label="Pago seguro" variant="outlined" />
        <Chip icon={<Timer />} label="Entrega puntual" variant="outlined" />
        <Chip icon={<Refresh />} label="Revisiones incluidas" variant="outlined" />
        <Chip icon={<Speed />} label="Calidad profesional" variant="outlined" />
      </Stack>

      {/* Stepper */}
      {currentStep !== 'select' && (
        <Stepper activeStep={stepIndex} sx={{ mb: 4 }}>
          {steps.map((label) => (
            <Step key={label}>
              <StepLabel>{label}</StepLabel>
            </Step>
          ))}
        </Stepper>
      )}

      {/* Step 1: Package Selection */}
      {currentStep === 'select' && (
        <>
          {/* Filter */}
          <Box sx={{ display: 'flex', justifyContent: 'center', mb: 4 }}>
            <FormControl sx={{ minWidth: 200 }}>
              <InputLabel>Tipo de servicio</InputLabel>
              <Select value={serviceFilter} label="Tipo de servicio" onChange={handleFilterChange}>
                <MenuItem value="all">Todos los servicios</MenuItem>
                <MenuItem value="Mixing">Mezcla</MenuItem>
                <MenuItem value="Mastering">Mastering</MenuItem>
                <MenuItem value="Bundle">Paquetes</MenuItem>
              </Select>
            </FormControl>
          </Box>

          {/* Package Grid */}
          {packagesLoading && (
            <Stack alignItems="center" sx={{ py: 6 }} spacing={1}>
              <CircularProgress size={28} />
              <Typography color="text.secondary">Cargando tarifas vigentes…</Typography>
            </Stack>
          )}
          {packagesError && (
            <Alert severity="error" sx={{ mb: 3 }}>
              No pudimos cargar la tarifa autorizada. La compra está deshabilitada para evitar mostrar precios incorrectos.
            </Alert>
          )}
          {!packagesLoading && !packagesError && filteredPackages.length === 0 && (
            <Alert severity="info" sx={{ mb: 3 }}>
              No hay paquetes activos para este servicio.
            </Alert>
          )}
          <Grid container spacing={3}>
            {filteredPackages.map((pkg) => (
              <Grid item xs={12} sm={6} md={4} key={pkg.id}>
                <Card
                  sx={{
                    height: '100%',
                    display: 'flex',
                    flexDirection: 'column',
                    border: pkg.tier === 'Pro' ? 2 : 1,
                    borderColor: pkg.tier === 'Pro' ? 'primary.main' : 'divider',
                    position: 'relative',
                  }}
                >
                  {pkg.tier === 'Pro' && (
                    <Chip
                      label="Más popular"
                      color="primary"
                      size="small"
                      sx={{ position: 'absolute', top: 12, right: 12 }}
                    />
                  )}
                  <CardHeader
                    avatar={serviceKindIcon(pkg.serviceKind)}
                    title={pkg.name}
                    subheader={
                      <Stack direction="row" spacing={1} sx={{ mt: 0.5 }}>
                        <Chip label={serviceKindLabel(pkg.serviceKind)} size="small" color={tierColor(pkg.tier)} />
                        <Chip label={pkg.tier} size="small" variant="outlined" />
                      </Stack>
                    }
                  />
                  <CardContent sx={{ flexGrow: 1 }}>
                    <Typography variant="h4" fontWeight={700} gutterBottom>
                      {formatPrice(pkg.priceUsdCents)}
                      <Typography component="span" variant="body2" color="text.secondary" sx={{ ml: 0.5 }}>
                        USD
                      </Typography>
                    </Typography>
                    <Typography variant="body2" color="text.secondary" gutterBottom>
                      {pkg.description}
                    </Typography>
                    <Divider sx={{ my: 2 }} />
                    <Stack spacing={1}>
                      {pkg.features.map((feature, i) => (
                        <Stack key={i} direction="row" spacing={1} alignItems="center">
                          <CheckCircle fontSize="small" color="success" />
                          <Typography variant="body2">{feature}</Typography>
                        </Stack>
                      ))}
                    </Stack>
                    <Stack direction="row" spacing={1} sx={{ mt: 2 }}>
                      <Chip icon={<Timer />} label={`${pkg.turnaroundDays} días`} size="small" variant="outlined" />
                      <Chip icon={<Refresh />} label={`${pkg.revisionCount} revisión${pkg.revisionCount > 1 ? 'es' : ''}`} size="small" variant="outlined" />
                    </Stack>
                  </CardContent>
                  <Box sx={{ p: 2, pt: 0 }}>
                    <Button
                      variant="contained"
                      fullWidth
                      size="large"
                      onClick={() => handleSelectPackage(pkg)}
                    >
                      Seleccionar
                    </Button>
                  </Box>
                </Card>
              </Grid>
            ))}
          </Grid>

          {/* FAQ Section */}
          <Box sx={{ mt: 8 }}>
            <Typography variant="h5" fontWeight={600} gutterBottom textAlign="center">
              Preguntas Frecuentes
            </Typography>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography fontWeight={500}>¿Cómo envío mis pistas para la mezcla?</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="body2">
                  Después de realizar el pago, recibirás un enlace para subir tus pistas.
                  Aceptamos WAV, AIFF, o FLAC. Te enviaremos instrucciones detalladas por email.
                </Typography>
              </AccordionDetails>
            </Accordion>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography fontWeight={500}>¿Qué formatos de entrega incluyen?</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="body2">
                  Todos los paquetes incluyen WAV de alta calidad. Los paquetes Pro y Premium
                  también incluyen formatos optimizados para streaming (Spotify, Apple Music)
                  y otros formatos según el paquete seleccionado.
                </Typography>
              </AccordionDetails>
            </Accordion>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography fontWeight={500}>¿Cómo funcionan las revisiones?</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="body2">
                  Cada paquete incluye un número de revisiones. Después de recibir la primera
                  versión, puedes solicitar cambios. Las revisiones adicionales tienen un costo
                  de $30 USD por revisión extra.
                </Typography>
              </AccordionDetails>
            </Accordion>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography fontWeight={500}>¿Qué métodos de pago aceptan?</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="body2">
                  Aceptamos tarjetas de crédito/débito (Visa, Mastercard, Diners), PayPal,
                  y transferencias bancarias. Todos los pagos son procesados de forma segura.
                </Typography>
              </AccordionDetails>
            </Accordion>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography fontWeight={500}>¿Ofrecen garantía de satisfacción?</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Typography variant="body2">
                  Sí. Si no estás satisfecho con el resultado final después de usar todas tus
                  revisiones, te devolvemos el 100% de tu dinero. Tu satisfacción es nuestra prioridad.
                </Typography>
              </AccordionDetails>
            </Accordion>
          </Box>
        </>
      )}

      {/* Step 2: Order Details */}
      {currentStep === 'details' && selectedPackage && (
        <Stack spacing={3} sx={{ maxWidth: 700, mx: 'auto' }}>
          <Paper sx={{ p: 4 }}>
          <Typography variant="h5" gutterBottom fontWeight={600}>
            Detalles del Proyecto
          </Typography>
          <Typography variant="body2" color="text.secondary" gutterBottom>
            Servicio seleccionado: <strong>{selectedPackage.name}</strong> — {formatPrice(selectedPackage.priceUsdCents)} USD
          </Typography>
          <Divider sx={{ my: 3 }} />
          <Stack spacing={3}>
            <TextField
              label="Tu nombre *"
              value={formData.buyerName}
              onChange={handleFormChange('buyerName')}
              fullWidth
              required
            />
            <TextField
              label="Email *"
              type="email"
              value={formData.buyerEmail}
              onChange={handleFormChange('buyerEmail')}
              fullWidth
              required
              helperText="Te enviaremos la confirmación y las instrucciones aquí"
            />
            <TextField
              type="tel"
              label="Teléfono (opcional)"
              value={formData.buyerPhone}
              onChange={handleFormChange('buyerPhone')}
              fullWidth
            />
            <Divider />
            <TextField
              label="Nombre del artista / banda"
              value={formData.artistName}
              onChange={handleFormChange('artistName')}
              fullWidth
            />
            <TextField
              label="Género musical"
              value={formData.genre}
              onChange={handleFormChange('genre')}
              fullWidth
              placeholder="Rock, Pop, Hip-Hop, Electrónica, etc."
            />
            <TextField
              label="Número de canciones"
              type="number"
              value={formData.songCount}
              onChange={handleFormChange('songCount')}
              fullWidth
              inputProps={{ min: selectedPackage.minSongCount, max: selectedPackage.maxSongCount }}
              helperText={
                selectedPackage.minSongCount === selectedPackage.maxSongCount
                  ? `Este paquete incluye ${selectedPackage.maxSongCount} canción(es).`
                  : `Este paquete incluye de ${selectedPackage.minSongCount} a ${selectedPackage.maxSongCount} canciones por el total mostrado.`
              }
            />
            <TextField
              label="Notas adicionales"
              value={formData.notes}
              onChange={handleFormChange('notes')}
              fullWidth
              multiline
              rows={3}
              placeholder="Cuéntanos sobre tu proyecto, referencias de sonido, expectativas..."
            />
            <TextField
              label="URL de referencia (opcional)"
              value={formData.referenceTrackUrl}
              onChange={handleFormChange('referenceTrackUrl')}
              fullWidth
              placeholder="https://soundcloud.com/... o https://youtube.com/..."
              helperText="Comparte una canción de referencia para el sonido que buscas"
            />
          </Stack>
          <Stack direction="row" spacing={2} sx={{ mt: 4 }}>
            <Button onClick={() => setCurrentStep('select')} startIcon={<ArrowBack />}>
              Volver
            </Button>
            <Button
              variant="contained"
              onClick={() => setCurrentStep('payment')}
              endIcon={<ArrowForward />}
              disabled={!formData.buyerName.trim() || !formData.buyerEmail.trim()}
            >
              Continuar al pago
            </Button>
          </Stack>
          </Paper>
          <ExperienceReviews
            targetKind="service_package"
            targetId={selectedPackage.id}
            title={`Reseñas de ${selectedPackage.name}`}
          />
        </Stack>
      )}

      {/* Step 3: Payment */}
      {currentStep === 'payment' && selectedPackage && (
        <Paper sx={{ p: 4, maxWidth: 600, mx: 'auto' }}>
          <Typography variant="h5" gutterBottom fontWeight={600}>
            Pago
          </Typography>
          <Divider sx={{ my: 3 }} />

          {/* Order Summary */}
          <Box sx={{ mb: 3, p: 2, bgcolor: 'grey.50', borderRadius: 1 }}>
            <Typography variant="subtitle2" gutterBottom>Resumen del pedido</Typography>
            <Stack direction="row" justifyContent="space-between">
              <Typography variant="body2">{selectedPackage.name}</Typography>
              <Typography variant="body2" fontWeight={600}>{formatPrice(selectedPackage.priceUsdCents)} USD</Typography>
            </Stack>
            <Stack direction="row" justifyContent="space-between">
              <Typography variant="body2" color="text.secondary">Entrega</Typography>
              <Typography variant="body2" color="text.secondary">{selectedPackage.turnaroundDays} días</Typography>
            </Stack>
            <Stack direction="row" justifyContent="space-between">
              <Typography variant="body2" color="text.secondary">Revisiones</Typography>
              <Typography variant="body2" color="text.secondary">{selectedPackage.revisionCount}</Typography>
            </Stack>
            <Divider sx={{ my: 1 }} />
            <Stack direction="row" justifyContent="space-between">
              <Typography variant="subtitle1" fontWeight={700}>Total</Typography>
              <Typography variant="subtitle1" fontWeight={700}>{formatPrice(selectedPackage.priceUsdCents)} USD</Typography>
            </Stack>
          </Box>

          {/* Payment Methods */}
          <Typography variant="subtitle2" gutterBottom>Método de pago</Typography>
          <Stack spacing={2} sx={{ mb: 3 }}>
            <Button
              variant="outlined"
              size="large"
              fullWidth
              sx={{ justifyContent: 'flex-start', py: 2 }}
              onClick={() => { void handleDatafastPayment(); }}
              disabled={paymentBusy}
            >
              💳 Tarjeta de crédito/débito (Datafast)
            </Button>
            <Button
              variant="outlined"
              size="large"
              fullWidth
              sx={{ justifyContent: 'flex-start', py: 2 }}
              onClick={() => { void handlePaypalPayment(); }}
              disabled={paymentBusy || !paypalClientId || !paypalReady}
            >
              🅿️ PayPal
            </Button>
            <Button
              variant="outlined"
              size="large"
              fullWidth
              sx={{ justifyContent: 'flex-start', py: 2 }}
              onClick={() => { void handleManualPayment(); }}
              disabled={paymentBusy}
            >
              🏦 Transferencia bancaria
            </Button>
          </Stack>

          {!paypalClientId && (
            <Alert severity="info" sx={{ mb: 2 }}>
              PayPal no está habilitado para este comercio. Datafast y transferencia permanecen disponibles.
            </Alert>
          )}

          <Alert severity="info" sx={{ mb: 2 }}>
            Al confirmar, aceptas nuestros términos de servicio y política de revisiones.
            Recibirás las instrucciones para enviar tus pistas por email.
          </Alert>

          <Stack direction="row" spacing={2}>
            <Button onClick={() => setCurrentStep('details')} startIcon={<ArrowBack />}>
              Volver
            </Button>
          </Stack>
        </Paper>
      )}

      {/* Step 4: Confirmation */}
      {currentStep === 'confirmation' && orderNumber && confirmedOrder && (
        <Paper sx={{ p: 4, maxWidth: 600, mx: 'auto', textAlign: 'center' }}>
          <CheckCircle
            sx={{
              fontSize: 64,
              color: confirmedOrder.ssoStatus === 'paid' ? 'success.main' : 'info.main',
              mb: 2,
            }}
          />
          <Typography variant="h5" gutterBottom fontWeight={600}>
            {confirmedOrder.ssoStatus === 'paid' ? 'Pago confirmado' : 'Pedido creado'}
          </Typography>
          <Typography variant="body1" gutterBottom>
            Tu número de pedido es:
          </Typography>
          <Typography variant="h4" fontWeight={700} color="primary" gutterBottom>
            {orderNumber}
          </Typography>
          <Alert severity={confirmedOrder.ssoStatus === 'paid' ? 'success' : 'info'} sx={{ my: 2, textAlign: 'left' }}>
            {confirmedOrder.ssoStatus === 'paid'
              ? 'El proveedor confirmó el pago ante el servidor. El servicio todavía requiere la recepción y validación de tus archivos.'
              : confirmedOrder.ssoStatus === 'awaiting_manual_confirmation'
                ? 'La transferencia fue seleccionada, pero todavía no es un pago. TDF debe verificar el comprobante antes de marcarla como pagada.'
                : 'El pedido existe, pero el pago todavía está procesándose o pendiente. Consulta el seguimiento antes de intentar pagar otra vez.'}
          </Alert>
          <Divider sx={{ my: 3 }} />
          <Typography variant="body2" gutterBottom>
            <strong>Próximos pasos:</strong>
          </Typography>
          <Stack spacing={1} sx={{ textAlign: 'left', maxWidth: 400, mx: 'auto', mb: 3 }}>
            <Typography variant="body2">1. Conserva el enlace privado de seguimiento</Typography>
            <Typography variant="body2">2. Espera la verificación del pago y las instrucciones de ingreso</Typography>
            <Typography variant="body2">3. El plazo operativo no comienza hasta completar pago e ingreso de archivos</Typography>
          </Stack>
          <Stack direction="row" spacing={2} justifyContent="center">
            <Button variant="outlined" onClick={handleReset}>
              Solicitar otro servicio
            </Button>
            <Button
              variant="contained"
              component={Link}
              href={lookupToken
                ? `/mezcla-mastering/pedido/${encodeURIComponent(orderNumber)}#access=${encodeURIComponent(lookupToken)}`
                : undefined}
              disabled={!lookupToken}
            >
              Ver estado del pedido
            </Button>
          </Stack>
        </Paper>
      )}

      <Dialog
        open={datafastDialogOpen}
        onClose={() => setDatafastDialogOpen(false)}
        maxWidth="xs"
        fullWidth
      >
        <DialogTitle>Pagar con Datafast</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            <Typography variant="body2" color="text.secondary">
              El pedido está creado, pero solo la verificación del servidor después del retorno puede marcarlo pagado.
            </Typography>
            {datafastError && <Alert severity="error">{datafastError}</Alert>}
            {datafastCheckout && datafastReturnUrl ? (
              <Box ref={datafastFormRef} key={datafastWidgetKey} sx={{ minHeight: 360, '& form': { width: '100%' } }}>
                <form
                  action={datafastReturnUrl}
                  className="paymentWidgets"
                  data-brands="VISA MASTER DINERS AMEX DISCOVER"
                />
              </Box>
            ) : (
              <CircularProgress size={24} />
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDatafastWidgetKey((value) => value + 1)}>Reintentar</Button>
          <Button onClick={() => setDatafastDialogOpen(false)} color="inherit">Cerrar</Button>
        </DialogActions>
      </Dialog>

      <Dialog open={paypalDialogOpen} onClose={() => setPaypalDialogOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>Pagar con PayPal</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            <Typography variant="body2" color="text.secondary">
              PayPal autoriza en su componente; TDF captura y verifica importe, moneda y referencia en el servidor.
            </Typography>
            {paypalError && <Alert severity="error">{paypalError}</Alert>}
            <Box ref={paypalButtonRef} minHeight={48} />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setPaypalDialogOpen(false)} color="inherit">Cerrar</Button>
        </DialogActions>
      </Dialog>

      {/* Snackbar */}
      <Snackbar
        open={snackbar.open}
        autoHideDuration={6000}
        onClose={() => setSnackbar((s) => ({ ...s, open: false }))}
      >
        <Alert severity={snackbar.severity} onClose={() => setSnackbar((s) => ({ ...s, open: false }))}>
          {snackbar.message}
        </Alert>
      </Snackbar>
    </Container>
  );
}
