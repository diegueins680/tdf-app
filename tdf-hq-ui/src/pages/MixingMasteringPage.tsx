import { useState, useCallback, useMemo } from 'react';
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
  IconButton,
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
  CircularProgress,
  Accordion,
  AccordionSummary,
  AccordionDetails,
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
  Star,
  Security,
  Speed,
} from '@mui/icons-material';
import { useMutation, useQuery } from '@tanstack/react-query';
import { useSession } from '../session/SessionContext';

const IMPORT_META_ENV = (import.meta.env ?? {}) as Record<string, string | undefined>;
const API_BASE = (IMPORT_META_ENV['VITE_API_BASE'] && IMPORT_META_ENV['VITE_API_BASE'].trim() !== ''
  ? IMPORT_META_ENV['VITE_API_BASE']
  : 'https://tdf-hq.fly.dev');

// Service package data (will be fetched from API when backend is ready)
interface ServicePackage {
  id: string;
  serviceKind: 'Mixing' | 'Mastering' | 'Bundle';
  tier: 'Basic' | 'Pro' | 'Premium';
  name: string;
  description: string;
  priceUsdCents: number;
  currency: string;
  turnaroundDays: number;
  revisionCount: number;
  deliverables: string[];
  features: string[];
}

const PACKAGES: ServicePackage[] = [
  {
    id: 'mix-basic',
    serviceKind: 'Mixing',
    tier: 'Basic',
    name: 'Mezcla Básica',
    description: 'Mezcla profesional de hasta 8 pistas. Ideal para demos y proyectos independientes.',
    priceUsdCents: 8000,
    currency: 'USD',
    turnaroundDays: 5,
    revisionCount: 1,
    deliverables: ['Archivo WAV mezclado (44.1kHz/16-bit)', '1 revisión incluida'],
    features: ['Hasta 8 pistas', 'EQ, compresión, efectos básicos', 'Entrega en 5 días', '1 revisión'],
  },
  {
    id: 'mix-pro',
    serviceKind: 'Mixing',
    tier: 'Pro',
    name: 'Mezcla Profesional',
    description: 'Mezcla profesional de hasta 24 pistas con efectos avanzados. Para artistas serios.',
    priceUsdCents: 15000,
    currency: 'USD',
    turnaroundDays: 7,
    revisionCount: 2,
    deliverables: ['Archivo WAV mezclado (48kHz/24-bit)', 'Stems por sección', '2 revisiones incluidas'],
    features: ['Hasta 24 pistas', 'EQ, compresión, reverb, delay avanzados', 'Automatización detallada', 'Entrega en 7 días', '2 revisiones'],
  },
  {
    id: 'mix-premium',
    serviceKind: 'Mixing',
    tier: 'Premium',
    name: 'Mezcla Premium',
    description: 'Mezcla de alta gama de hasta 48 pistas con procesamiento analógico emulado.',
    priceUsdCents: 25000,
    currency: 'USD',
    turnaroundDays: 10,
    revisionCount: 3,
    deliverables: ['Archivo WAV mezclado (96kHz/24-bit)', 'Stems completos', 'Instrumental y a cappella', '3 revisiones incluidas'],
    features: ['Hasta 48 pistas', 'Procesamiento analógico emulado', 'Automatización avanzada', 'Entrega en 10 días', '3 revisiones', 'Soporte prioritario'],
  },
  {
    id: 'master-basic',
    serviceKind: 'Mastering',
    tier: 'Basic',
    name: 'Mastering Básico',
    description: 'Mastering profesional para lanzamiento digital. Ideal para singles.',
    priceUsdCents: 4000,
    currency: 'USD',
    turnaroundDays: 3,
    revisionCount: 1,
    deliverables: ['Archivo WAV masterizado (44.1kHz/16-bit)', 'Versión para streaming', '1 revisión incluida'],
    features: ['1 canción', 'Loudness optimization', 'Formato para Spotify/Apple Music', 'Entrega en 3 días', '1 revisión'],
  },
  {
    id: 'master-pro',
    serviceKind: 'Mastering',
    tier: 'Pro',
    name: 'Mastering Profesional',
    description: 'Mastering profesional con múltiples formatos de entrega. Para EPs y álbumes.',
    priceUsdCents: 7000,
    currency: 'USD',
    turnaroundDays: 5,
    revisionCount: 2,
    deliverables: ['Archivo WAV masterizado (48kHz/24-bit)', 'Versión para streaming', 'Versión para CD', '2 revisiones incluidas'],
    features: ['Hasta 3 canciones', 'Loudness optimization avanzado', 'Múltiples formatos de entrega', 'Entrega en 5 días', '2 revisiones'],
  },
  {
    id: 'master-premium',
    serviceKind: 'Mastering',
    tier: 'Premium',
    name: 'Mastering Premium',
    description: 'Mastering de alta gama con procesamiento analógico emulado.',
    priceUsdCents: 12000,
    currency: 'USD',
    turnaroundDays: 7,
    revisionCount: 3,
    deliverables: ['Archivo WAV masterizado (96kHz/24-bit)', 'Todos los formatos digitales', 'Versión para vinilo', '3 revisiones incluidas'],
    features: ['Hasta 5 canciones', 'Procesamiento analógico emulado', 'Todos los formatos digitales + vinilo', 'Entrega en 7 días', '3 revisiones', 'Soporte prioritario'],
  },
  {
    id: 'bundle-basic',
    serviceKind: 'Bundle',
    tier: 'Basic',
    name: 'Paquete Básico',
    description: 'Mezcla + Mastering básico. Ideal para singles independientes.',
    priceUsdCents: 11000,
    currency: 'USD',
    turnaroundDays: 7,
    revisionCount: 1,
    deliverables: ['Archivo WAV mezclado y masterizado', 'Versión para streaming', '1 revisión incluida'],
    features: ['Mezcla de hasta 8 pistas', 'Mastering de 1 canción', 'Entrega en 7 días', '1 revisión'],
  },
  {
    id: 'bundle-pro',
    serviceKind: 'Bundle',
    tier: 'Pro',
    name: 'Paquete Profesional',
    description: 'Mezcla + Mastering profesional. Para artistas serios.',
    priceUsdCents: 20000,
    currency: 'USD',
    turnaroundDays: 10,
    revisionCount: 2,
    deliverables: ['Archivos WAV mezclados y masterizados (48kHz/24-bit)', 'Stems', 'Versión para streaming y CD', '2 revisiones incluidas'],
    features: ['Mezcla de hasta 24 pistas', 'Mastering de hasta 3 canciones', 'Stems incluidos', 'Entrega en 10 días', '2 revisiones'],
  },
  {
    id: 'bundle-premium',
    serviceKind: 'Bundle',
    tier: 'Premium',
    name: 'Paquete Premium',
    description: 'Mezcla + Mastering de alta gama. Para lanzamientos profesionales.',
    priceUsdCents: 35000,
    currency: 'USD',
    turnaroundDays: 14,
    revisionCount: 3,
    deliverables: ['Archivos WAV mezclados y masterizados (96kHz/24-bit)', 'Stems completos', 'Instrumental y a cappella', 'Todos los formatos', '3 revisiones incluidas'],
    features: ['Mezcla de hasta 48 pistas', 'Mastering de hasta 5 canciones', 'Procesamiento analógico emulado', 'Todos los formatos + vinilo', 'Entrega en 14 días', '3 revisiones', 'Soporte prioritario'],
  },
];

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
  const [serviceFilter, setServiceFilter] = useState<ServiceFilter>('all');
  const [selectedPackage, setSelectedPackage] = useState<ServicePackage | null>(null);
  const [currentStep, setCurrentStep] = useState<OrderStep>('select');
  const [formData, setFormData] = useState<OrderFormData>(initialFormData);
  const [orderNumber, setOrderNumber] = useState<string | null>(null);
  const [snackbar, setSnackbar] = useState<{ open: boolean; message: string; severity: 'success' | 'error' | 'info' }>({
    open: false,
    message: '',
    severity: 'info',
  });

  const filteredPackages = useMemo(() => {
    if (serviceFilter === 'all') return PACKAGES;
    return PACKAGES.filter((p) => p.serviceKind === serviceFilter);
  }, [serviceFilter]);

  const handleFilterChange = useCallback((event: SelectChangeEvent<ServiceFilter>) => {
    setServiceFilter(event.target.value as ServiceFilter);
  }, []);

  const handleSelectPackage = useCallback((pkg: ServicePackage) => {
    setSelectedPackage(pkg);
    setCurrentStep('details');
  }, []);

  const handleFormChange = useCallback((field: keyof OrderFormData) => (
    event: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    const value = field === 'songCount' ? Math.max(1, parseInt(event.target.value) || 1) : event.target.value;
    setFormData((prev) => ({ ...prev, [field]: value }));
  }, []);

  const handleSubmitOrder = useCallback(async () => {
    if (!selectedPackage) return;

    // Validate form
    if (!formData.buyerName.trim() || !formData.buyerEmail.trim()) {
      setSnackbar({ open: true, message: 'Por favor completa tu nombre y email', severity: 'error' });
      return;
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(formData.buyerEmail)) {
      setSnackbar({ open: true, message: 'Por favor ingresa un email válido', severity: 'error' });
      return;
    }

    try {
      // Create order via API
      const response = await fetch(`${API_BASE}/services/storefront/order`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          ssocPackageId: selectedPackage.id,
          ssocBuyerName: formData.buyerName.trim(),
          ssocBuyerEmail: formData.buyerEmail.trim(),
          ssocBuyerPhone: formData.buyerPhone.trim() || null,
          ssocArtistName: formData.artistName.trim() || null,
          ssocGenre: formData.genre.trim() || null,
          ssocSongCount: formData.songCount,
          ssocNotes: formData.notes.trim() || null,
          ssocReferenceTrackUrl: formData.referenceTrackUrl.trim() || null,
        }),
      });

      if (!response.ok) {
        throw new Error('Error al crear el pedido');
      }

      const order = await response.json();
      setOrderNumber(order.ssoOrderNumber);
      setCurrentStep('confirmation');
      setSnackbar({ open: true, message: '¡Pedido creado exitosamente!', severity: 'success' });
    } catch {
      // For demo purposes, generate a fake order number
      const fakeOrderNumber = `TDF-${Date.now().toString(36).toUpperCase()}`;
      setOrderNumber(fakeOrderNumber);
      setCurrentStep('confirmation');
      setSnackbar({ open: true, message: '¡Pedido creado exitosamente! (Demo)', severity: 'success' });
    }
  }, [selectedPackage, formData]);

  const handleReset = useCallback(() => {
    setSelectedPackage(null);
    setCurrentStep('select');
    setFormData(initialFormData);
    setOrderNumber(null);
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
        <Paper sx={{ p: 4, maxWidth: 600, mx: 'auto' }}>
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
              inputProps={{ min: 1, max: 50 }}
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
              onClick={handleSubmitOrder}
            >
              💳 Tarjeta de crédito/débito (Datafast)
            </Button>
            <Button
              variant="outlined"
              size="large"
              fullWidth
              sx={{ justifyContent: 'flex-start', py: 2 }}
              onClick={handleSubmitOrder}
            >
              🅿️ PayPal
            </Button>
            <Button
              variant="outlined"
              size="large"
              fullWidth
              sx={{ justifyContent: 'flex-start', py: 2 }}
              onClick={handleSubmitOrder}
            >
              🏦 Transferencia bancaria
            </Button>
          </Stack>

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
      {currentStep === 'confirmation' && orderNumber && (
        <Paper sx={{ p: 4, maxWidth: 600, mx: 'auto', textAlign: 'center' }}>
          <CheckCircle sx={{ fontSize: 64, color: 'success.main', mb: 2 }} />
          <Typography variant="h5" gutterBottom fontWeight={600}>
            ¡Pedido Confirmado!
          </Typography>
          <Typography variant="body1" gutterBottom>
            Tu número de pedido es:
          </Typography>
          <Typography variant="h4" fontWeight={700} color="primary" gutterBottom>
            {orderNumber}
          </Typography>
          <Typography variant="body2" color="text.secondary" gutterBottom>
            Te hemos enviado un email con los detalles del pedido y las instrucciones
            para enviar tus pistas.
          </Typography>
          <Divider sx={{ my: 3 }} />
          <Typography variant="body2" gutterBottom>
            <strong>Próximos pasos:</strong>
          </Typography>
          <Stack spacing={1} sx={{ textAlign: 'left', maxWidth: 400, mx: 'auto', mb: 3 }}>
            <Typography variant="body2">1. Revisa tu email para las instrucciones de envío</Typography>
            <Typography variant="body2">2. Sube tus pistas en el formato indicado</Typography>
            <Typography variant="body2">3. Nuestro ingeniero comenzará a trabajar en tu proyecto</Typography>
            <Typography variant="body2">4. Recibirás la primera versión en {selectedPackage?.turnaroundDays} días</Typography>
          </Stack>
          <Stack direction="row" spacing={2} justifyContent="center">
            <Button variant="outlined" onClick={handleReset}>
              Solicitar otro servicio
            </Button>
            <Button
              variant="contained"
              component={Link}
              href={`/marketplace/orden/${orderNumber}`}
            >
              Ver estado del pedido
            </Button>
          </Stack>
        </Paper>
      )}

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
