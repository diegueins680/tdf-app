import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, Chip, CircularProgress, FormControlLabel, MenuItem, Paper, Stack, Tab, Tabs, TextField, Typography } from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DownloadIcon from '@mui/icons-material/Download';
import { useTranslation } from 'react-i18next';
import { Directory } from '../api/directory';
import { Merch, createMerchIdempotencyKey, type MerchPermissions, type MerchProduct, type MerchProductRequest } from '../api/merch';
import { PartySelector } from '../components/party-selector/PartySelector';
import type { PartySelectorOption } from '../api/partySelector';
import { buildMerchOrdersCsv, formatMerchMoney, merchLanguage, merchStatusLabel, type MerchOrderExportRow } from '../utils/merch';

const optionalTrimmed = (value?: string) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  return trimmed;
};

type SellerTab = 'catalog' | 'orders' | 'team' | 'settings';

const allPermissions: MerchPermissions = { catalog: true, stock: true, orders: true, fulfillment: true, finance: false, settings: false };

const initialProduct = (): MerchProductRequest => ({
  slug: '', name: '', description: '', category: 'apparel', visibility: 'public', availabilityMode: 'in_stock',
  preorderReleaseAt: null, publishAt: null, unpublishAt: null, buyerLimit: null, policyId: null,
  variants: [{ sku: '', name: 'Única', optionValues: {}, priceMinor: 1000, compareAtPriceMinor: null, currency: 'USD', weightGrams: 200, customsDescription: null, stockMode: 'finite', stockOnHand: 0, reorderThreshold: 0, active: true }],
});

function ProductImageUpload({ storeId, productId, onUploaded, language }: { storeId: string; productId: string; onUploaded: () => void; language: 'es' | 'en' }) {
  const [file, setFile] = useState<File | null>(null);
  const [altText, setAltText] = useState('');
  const upload = useMutation({ mutationFn: () => Merch.uploadProductImage(storeId, productId, file!, altText.trim(), 0), onSuccess: () => { setFile(null); setAltText(''); onUploaded(); } });
  return <Stack component="form" direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }} onSubmit={(event) => { event.preventDefault(); upload.mutate(); }}>
    <Button component="label" size="small" variant="outlined">{file?.name ?? (language === 'en' ? 'Choose JPEG/PNG' : 'Elegir JPEG/PNG')}<input hidden type="file" accept="image/jpeg,image/png" onChange={(event) => setFile(event.target.files?.[0] ?? null)} /></Button>
    <TextField size="small" label={language === 'en' ? 'Image description' : 'Descripción de la imagen'} value={altText} onChange={(event) => setAltText(event.target.value)} inputProps={{ maxLength: 500 }} />
    <Button type="submit" size="small" disabled={!file || !altText.trim() || upload.isPending}>{language === 'en' ? 'Upload safely' : 'Subir de forma segura'}</Button>
    {upload.isError && <Alert severity="error">{language === 'en' ? 'The image was rejected or could not be processed.' : 'La imagen fue rechazada o no pudo procesarse.'}</Alert>}
  </Stack>;
}

type ManagedVariant = NonNullable<MerchProduct['variants']>[number];

function VariantStockEditor({ storeId, variant, onSaved, language }: { storeId: string; variant: ManagedVariant; onSaved: () => void; language: 'es' | 'en' }) {
  const [stockOnHand, setStockOnHand] = useState(variant.stockOnHand ?? 0);
  const [reorderThreshold, setReorderThreshold] = useState(variant.reorderThreshold ?? 0);
  const [active, setActive] = useState(variant.active);
  const update = useMutation({
    mutationFn: () => Merch.updateVariantStock(storeId, variant.id, { stockOnHand, reorderThreshold, active, version: variant.version }),
    onSuccess: onSaved,
  });
  return <Stack component="form" direction={{ xs: 'column', md: 'row' }} spacing={1} alignItems={{ md: 'center' }} onSubmit={(event) => { event.preventDefault(); update.mutate(); }}>
    <Box flex={1}><Typography fontWeight={700}>{variant.name}</Typography><Typography variant="caption">{variant.sku} · {language === 'en' ? 'reserved' : 'reservado'} {variant.stockReserved ?? 0} · {language === 'en' ? 'sold' : 'vendido'} {variant.stockSold ?? 0}</Typography></Box>
    <TextField required size="small" type="number" inputProps={{ min: 0 }} label={language === 'en' ? 'On hand' : 'Existencia'} value={stockOnHand} onChange={(event) => setStockOnHand(Number(event.target.value))} />
    <TextField required size="small" type="number" inputProps={{ min: 0 }} label={language === 'en' ? 'Low-stock alert' : 'Alerta de stock'} value={reorderThreshold} onChange={(event) => setReorderThreshold(Number(event.target.value))} />
    <FormControlLabel control={<Checkbox checked={active} onChange={(event) => setActive(event.target.checked)} />} label={language === 'en' ? 'Active' : 'Activa'} />
    <Button type="submit" disabled={update.isPending}>{language === 'en' ? 'Update stock' : 'Actualizar stock'}</Button>
    {update.isError && <Alert severity="error">{language === 'en' ? 'Stock changed elsewhere or is below reserved/sold units. Reload and retry.' : 'El stock cambió en otro lugar o es menor que las unidades reservadas/vendidas. Recarga e intenta otra vez.'}</Alert>}
  </Stack>;
}

export default function MerchSellerPage() {
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const client = useQueryClient();
  const [tab, setTab] = useState<SellerTab>('catalog');
  const [selectedStoreId, setSelectedStoreId] = useState('');
  const [application, setApplication] = useState({ profileId: '', slug: '', displayName: '', description: '', applicationNote: '' });
  const [product, setProduct] = useState<MerchProductRequest>(initialProduct);
  const [selectedMember, setSelectedMember] = useState<PartySelectorOption | null>(null);
  const [memberPermissions, setMemberPermissions] = useState<MerchPermissions>(allPermissions);
  const [policy, setPolicy] = useState({ shippingPolicy: '', returnPolicy: '', preorderPolicy: '', supportEmail: '' });
  const [zone, setZone] = useState({ name: 'Envío nacional', deliveryMethod: 'national_shipping' as 'national_shipping' | 'coordinated_pickup', rateMinor: 500, freeShippingMinMinor: '' });
  const [tracking, setTracking] = useState<Record<string, { carrier: string; number: string; url: string }>>({});
  const [orderStatus, setOrderStatus] = useState('');
  const [issueResponses, setIssueResponses] = useState<Record<string, string>>({});
  const [applicationKey, setApplicationKey] = useState(() => createMerchIdempotencyKey('store-application'));
  const [productKey, setProductKey] = useState(() => createMerchIdempotencyKey('product'));
  const [inviteKey, setInviteKey] = useState(() => createMerchIdempotencyKey('member'));
  const capabilities = useQuery({ queryKey: ['merch-capabilities'], queryFn: Merch.capabilities, retry: false });
  const stores = useQuery({ queryKey: ['merch-seller-stores'], queryFn: Merch.sellerStores, retry: false });
  const profiles = useQuery({ queryKey: ['directory', 'managed-profiles'], queryFn: Directory.managedProfiles, retry: false });
  const selectedStore = stores.data?.find((store) => store.id === selectedStoreId) ?? stores.data?.[0];

  useEffect(() => { if (!selectedStoreId && stores.data?.[0]) setSelectedStoreId(stores.data[0].id); }, [selectedStoreId, stores.data]);

  const products = useQuery({ queryKey: ['merch-seller-products', selectedStore?.id], queryFn: () => Merch.sellerProducts(selectedStore!.id), enabled: [selectedStore?.permissions?.catalog, selectedStore?.permissions?.stock].includes(true), retry: false });
  const orders = useQuery({ queryKey: ['merch-seller-orders', selectedStore?.id, orderStatus], queryFn: () => Merch.sellerOrders(selectedStore!.id, orderStatus || undefined), enabled: Boolean(selectedStore?.permissions?.orders), retry: false });
  const issues = useQuery({ queryKey: ['merch-seller-issues', selectedStore?.id], queryFn: () => Merch.sellerIssues(selectedStore!.id), enabled: Boolean(selectedStore?.permissions?.orders), retry: false });
  const members = useQuery({ queryKey: ['merch-seller-members', selectedStore?.id], queryFn: () => Merch.members(selectedStore!.id), enabled: Boolean(selectedStore?.permissions?.settings), retry: false });

  const eligibleProfiles = useMemo(() => (profiles.data ?? []).filter((profile) => ['artist', 'band', 'project'].includes(profile.kind) && profile.status === 'published' && profile.moderationStatus === 'allowed'), [profiles.data]);
  const orderSummary = useMemo(() => (orders.data ?? []).reduce((summary, order) => ({
    count: summary.count + 1,
    totalMinor: summary.totalMinor + order.totalMinor,
    commissionMinor: summary.commissionMinor + (order.tdfCommissionMinor ?? 0),
    sellerNetMinor: summary.sellerNetMinor + (order.sellerNetMinor ?? 0),
  }), { count: 0, totalMinor: 0, commissionMinor: 0, sellerNetMinor: 0 }), [orders.data]);

  const downloadOrders = () => {
    if (!orders.data?.length || !selectedStore) return;
    const exportRows = orders.data.map((order) => ({
      ...order,
      createdAt: typeof order['createdAt'] === 'string' ? order['createdAt'] : undefined,
    })) as MerchOrderExportRow[];
    const blob = new Blob([buildMerchOrdersCsv(exportRows, language)], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${selectedStore.slug}-orders.csv`;
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.setTimeout(() => URL.revokeObjectURL(url), 0);
  };

  const applyMutation = useMutation({
    mutationFn: () => Merch.applyForStore({ profileId: application.profileId, slug: application.slug, displayName: application.displayName, description: application.description || null, applicationNote: application.applicationNote }, applicationKey),
    onSuccess: () => { setApplicationKey(createMerchIdempotencyKey('store-application')); void client.invalidateQueries({ queryKey: ['merch-seller-stores'] }); },
  });
  const productMutation = useMutation({ mutationFn: () => Merch.createProduct(selectedStore!.id, product, productKey), onSuccess: () => { setProduct(initialProduct()); setProductKey(createMerchIdempotencyKey('product')); void client.invalidateQueries({ queryKey: ['merch-seller-products', selectedStore?.id] }); } });
  const statusMutation = useMutation({ mutationFn: ({ productId, status }: { productId: string; status: string }) => Merch.updateProductStatus(selectedStore!.id, productId, status), onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-seller-products', selectedStore?.id] }) });
  const inviteMutation = useMutation({ mutationFn: () => Merch.inviteMember(selectedStore!.id, { partyId: selectedMember!.partyId, permissions: memberPermissions }, inviteKey), onSuccess: () => { setSelectedMember(null); setInviteKey(createMerchIdempotencyKey('member')); void client.invalidateQueries({ queryKey: ['merch-seller-members', selectedStore?.id] }); } });
  const policyMutation = useMutation({ mutationFn: () => Merch.createPolicy(selectedStore!.id, { shippingPolicy: policy.shippingPolicy, returnPolicy: policy.returnPolicy, preorderPolicy: policy.preorderPolicy || null, supportEmail: policy.supportEmail || null }) });
  const zoneMutation = useMutation({ mutationFn: () => Merch.createShippingZone(selectedStore!.id, { name: zone.name, countryCode: 'EC', subdivisionCodes: [], deliveryMethod: zone.deliveryMethod, rateMinor: zone.rateMinor, freeShippingMinMinor: zone.freeShippingMinMinor ? Number(zone.freeShippingMinMinor) : null, estimatedMinDays: zone.deliveryMethod === 'national_shipping' ? 2 : 0, estimatedMaxDays: zone.deliveryMethod === 'national_shipping' ? 7 : 0, active: true }) });
  const fulfillmentMutation = useMutation({ mutationFn: ({ orderId, payload }: { orderId: string; payload: Parameters<typeof Merch.updateFulfillment>[2] }) => Merch.updateFulfillment(selectedStore!.id, orderId, payload), onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-seller-orders', selectedStore?.id] }) });
  const issueMutation = useMutation({ mutationFn: ({ issueId, status }: { issueId: string; status: Parameters<typeof Merch.updateSellerIssue>[2]['status'] }) => Merch.updateSellerIssue(selectedStore!.id, issueId, { status, publicResponse: optionalTrimmed(issueResponses[issueId]), internalNotes: null }), onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-seller-issues', selectedStore?.id] }) });

  if (capabilities.isLoading || stores.isLoading || profiles.isLoading) return <Box py={8} textAlign="center"><CircularProgress aria-label="Cargando panel de merch" /></Box>;
  if (capabilities.isError || stores.isError) return <Box py={4}><Alert severity="error">{language === 'en' ? 'The merch pilot status could not be loaded.' : 'No se pudo cargar el estado del piloto de merch.'}</Alert></Box>;

  if (!selectedStore) {
    return <Box component="main" py={4} maxWidth="sm" mx="auto"><Stack spacing={3}><Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Artist merch pilot' : 'Piloto de merch para artistas'}</Typography><Alert severity="info">{language === 'en' ? 'Applications are reviewed before any store becomes public.' : 'Cada solicitud se revisa antes de que una tienda pueda ser pública.'}</Alert>{!capabilities.data?.features.sellerApplications ? <Alert severity="warning">{language === 'en' ? 'Applications are currently closed. Your profile and data were not changed.' : 'Las solicitudes están cerradas por ahora. Tu perfil y tus datos no fueron modificados.'}</Alert> : <Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); applyMutation.mutate(); }}><Stack spacing={2}><TextField select required label={language === 'en' ? 'Claimed or verified artist profile' : 'Perfil de artista reclamado o verificado'} value={application.profileId} onChange={(event) => { const profile = eligibleProfiles.find((item) => item.id === event.target.value); setApplication({ ...application, profileId: event.target.value, slug: profile?.slug ?? application.slug, displayName: profile?.name ?? application.displayName }); }}>{eligibleProfiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name} · @{profile.slug}</MenuItem>)}</TextField><TextField required label="Slug" value={application.slug} onChange={(event) => setApplication({ ...application, slug: event.target.value.toLowerCase().replace(/[^a-z0-9-]/g, '-') })} helperText="ejemplo-banda" /><TextField required label={language === 'en' ? 'Store name' : 'Nombre de la tienda'} value={application.displayName} onChange={(event) => setApplication({ ...application, displayName: event.target.value })} /><TextField multiline minRows={2} label={language === 'en' ? 'Short description' : 'Descripción breve'} value={application.description} onChange={(event) => setApplication({ ...application, description: event.target.value })} /><TextField required multiline minRows={3} inputProps={{ minLength: 10, maxLength: 2000 }} label={language === 'en' ? 'Why do you want to join the pilot?' : '¿Por qué quieren participar en el piloto?'} value={application.applicationNote} onChange={(event) => setApplication({ ...application, applicationNote: event.target.value })} />{eligibleProfiles.length === 0 && <Alert severity="info">{language === 'en' ? 'Publish and claim or verify an artist/band profile first.' : 'Primero publica y reclama o verifica un perfil de artista o banda.'}</Alert>}{applyMutation.isError && <Alert severity="error">{applyMutation.error instanceof Error ? applyMutation.error.message : 'No se pudo enviar.'}</Alert>}{applyMutation.isSuccess && <Alert severity="success">{language === 'en' ? 'Application submitted for review. The store is not active yet.' : 'Solicitud enviada a revisión. La tienda todavía no está activa.'}</Alert>}<Button type="submit" variant="contained" disabled={eligibleProfiles.length === 0 || applyMutation.isPending}>{language === 'en' ? 'Submit application' : 'Enviar solicitud'}</Button></Stack></Paper>}</Stack></Box>;
  }

  return (
    <Box component="main" py={{ xs: 2, md: 4 }}>
      <Stack spacing={3}>
        <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" spacing={2}><Box><Typography component="h1" variant="h3" fontWeight={900}>{selectedStore.displayName}</Typography><Stack direction="row" spacing={1}><Chip label={merchStatusLabel(selectedStore.applicationStatus ?? '', language)} /><Chip label={merchStatusLabel(selectedStore.operationalStatus ?? '', language)} /></Stack></Box>{(stores.data?.length ?? 0) > 1 && <TextField select label={language === 'en' ? 'Store' : 'Tienda'} value={selectedStore.id} onChange={(event) => setSelectedStoreId(event.target.value)}>{stores.data?.map((store) => <MenuItem key={store.id} value={store.id}>{store.displayName}</MenuItem>)}</TextField>}</Stack>
        {selectedStore.operationalStatus !== 'active' && <Alert severity="warning">{language === 'en' ? 'This store is not active. You can prepare drafts, but it will not appear publicly.' : 'Esta tienda no está activa. Puedes preparar borradores, pero no aparecerá públicamente.'}</Alert>}
        <Tabs value={tab} onChange={(_event, value: SellerTab) => setTab(value)} variant="scrollable" allowScrollButtonsMobile aria-label={language === 'en' ? 'Store management' : 'Gestión de tienda'}><Tab value="catalog" label={language === 'en' ? 'Catalog' : 'Catálogo'} /><Tab value="orders" label={language === 'en' ? 'Orders' : 'Pedidos'} /><Tab value="team" label={language === 'en' ? 'Team' : 'Equipo'} /><Tab value="settings" label={language === 'en' ? 'Policies & delivery' : 'Políticas y entrega'} /></Tabs>

        {tab === 'catalog' && <Stack spacing={3}><Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Products' : 'Productos'}</Typography><Stack spacing={2}>{products.data?.map((item) => <Paper key={item.id} variant="outlined" sx={{ p: 2 }}><Stack spacing={1}><Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between" alignItems={{ sm: 'center' }}><Box><Typography fontWeight={800}>{item.name}</Typography><Typography variant="body2">{merchStatusLabel(item.status, language)}</Typography></Box><Stack direction="row" spacing={1}>{item.status === 'draft' && <Button onClick={() => statusMutation.mutate({ productId: item.id, status: 'pending_review' })}>{language === 'en' ? 'Submit for review' : 'Enviar a revisión'}</Button>}{item.status === 'published' && <Button onClick={() => statusMutation.mutate({ productId: item.id, status: 'paused' })}>{language === 'en' ? 'Pause' : 'Pausar'}</Button>}</Stack></Stack>{item.status === 'draft' && item.images?.length === 0 && <ProductImageUpload storeId={selectedStore.id} productId={item.id} language={language} onUploaded={() => void client.invalidateQueries({ queryKey: ['merch-seller-products', selectedStore.id] })} />}{item.images && item.images.length > 0 && <Typography variant="caption" color="text.secondary">{language === 'en' ? `${item.images.length} image(s) uploaded; moderation remains separate.` : `${item.images.length} imagen(es) subida(s); la moderación se mantiene separada.`}</Typography>}</Stack></Paper>)}{products.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No products yet.' : 'Todavía no hay productos.'}</Typography>}</Stack></Paper>
          {selectedStore.permissions?.catalog && <Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); productMutation.mutate(); }}><Stack spacing={2}><Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'New product draft' : 'Nuevo borrador de producto'}</Typography><TextField required label={language === 'en' ? 'Product name' : 'Nombre del producto'} value={product.name} onChange={(event) => setProduct({ ...product, name: event.target.value })} /><TextField required label="Slug" value={product.slug} onChange={(event) => setProduct({ ...product, slug: event.target.value.toLowerCase().replace(/[^a-z0-9-]/g, '-') })} /><TextField required multiline minRows={3} label={language === 'en' ? 'Description' : 'Descripción'} value={product.description} onChange={(event) => setProduct({ ...product, description: event.target.value })} /><Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}><TextField select fullWidth label={language === 'en' ? 'Category' : 'Categoría'} value={product.category} onChange={(event) => setProduct({ ...product, category: event.target.value as MerchProductRequest['category'] })}>{['apparel','vinyl','cd','cassette','poster','accessory','limited_edition','bundle'].map((category) => <MenuItem key={category} value={category}>{category.replace(/_/g, ' ')}</MenuItem>)}</TextField><TextField select fullWidth label={language === 'en' ? 'Availability' : 'Disponibilidad'} value={product.availabilityMode} onChange={(event) => setProduct({ ...product, availabilityMode: event.target.value as MerchProductRequest['availabilityMode'] })}><MenuItem value="in_stock">{language === 'en' ? 'In stock' : 'En stock'}</MenuItem><MenuItem value="preorder">{language === 'en' ? 'Preorder' : 'Preventa'}</MenuItem><MenuItem value="made_to_order">{language === 'en' ? 'Made to order' : 'Bajo pedido'}</MenuItem></TextField></Stack>{product.variants.map((variant, index) => <Paper key={index} variant="outlined" sx={{ p: 2 }}><Typography fontWeight={800}>{language === 'en' ? `Variant ${index + 1}` : `Variante ${index + 1}`}</Typography><Stack spacing={2} mt={1}><TextField required label="SKU" value={variant.sku} onChange={(event) => { const variants = [...product.variants]; variants[index] = { ...variant, sku: event.target.value }; setProduct({ ...product, variants }); }} /><TextField required label={language === 'en' ? 'Variant name' : 'Nombre de variante'} value={variant.name} onChange={(event) => { const variants = [...product.variants]; variants[index] = { ...variant, name: event.target.value }; setProduct({ ...product, variants }); }} /><Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}><TextField required type="number" fullWidth label={language === 'en' ? 'Price (cents)' : 'Precio (centavos)'} value={variant.priceMinor} onChange={(event) => { const variants = [...product.variants]; variants[index] = { ...variant, priceMinor: Number(event.target.value) }; setProduct({ ...product, variants }); }} /><TextField required type="number" fullWidth label={language === 'en' ? 'Weight (grams)' : 'Peso (gramos)'} value={variant.weightGrams} onChange={(event) => { const variants = [...product.variants]; variants[index] = { ...variant, weightGrams: Number(event.target.value) }; setProduct({ ...product, variants }); }} /><TextField required type="number" fullWidth label="Stock" value={variant.stockOnHand} onChange={(event) => { const variants = [...product.variants]; variants[index] = { ...variant, stockOnHand: Number(event.target.value) }; setProduct({ ...product, variants }); }} /></Stack></Stack></Paper>)}<Button startIcon={<AddIcon />} onClick={() => setProduct({ ...product, variants: [...product.variants, { ...product.variants[0]!, id: null, sku: '', name: '' }] })}>{language === 'en' ? 'Add variant' : 'Agregar variante'}</Button>{productMutation.isError && <Alert severity="error">{productMutation.error instanceof Error ? productMutation.error.message : 'Error'}</Alert>}<Button type="submit" variant="contained" disabled={productMutation.isPending}>{language === 'en' ? 'Save draft' : 'Guardar borrador'}</Button><Typography variant="caption" color="text.secondary">{language === 'en' ? 'After saving, upload a validated image before submitting for review.' : 'Después de guardar, sube una imagen validada antes de enviarlo a revisión.'}</Typography></Stack></Paper>}</Stack>}

        {tab === 'orders' && <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ md: 'center' }}>
              <Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Orders' : 'Pedidos'}</Typography>
              {selectedStore.permissions?.orders && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField select size="small" label={language === 'en' ? 'Fulfillment status' : 'Estado de entrega'} value={orderStatus} onChange={(event) => setOrderStatus(event.target.value)} sx={{ minWidth: 210 }}>
                  <MenuItem value="">{language === 'en' ? 'All statuses' : 'Todos los estados'}</MenuItem>
                  {['pending', 'preparing', 'ready_for_pickup', 'shipped', 'delivered', 'problem', 'return_requested', 'returned', 'cancelled'].map((status) => <MenuItem key={status} value={status}>{merchStatusLabel(status, language)}</MenuItem>)}
                </TextField>
                <Button startIcon={<DownloadIcon />} variant="outlined" disabled={!orders.data?.length || orders.isFetching} onClick={downloadOrders}>
                  {language === 'en' ? 'Export CSV' : 'Exportar CSV'}
                </Button>
              </Stack>}
            </Stack>
            {!selectedStore.permissions?.orders && <Alert severity="warning">{language === 'en' ? 'You do not have order access.' : 'No tienes acceso a pedidos.'}</Alert>}
            {selectedStore.permissions?.orders && <>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} aria-label={language === 'en' ? 'Order totals for current filter' : 'Totales de pedidos del filtro actual'}>
                <Chip label={`${orderSummary.count} ${language === 'en' ? 'orders' : 'pedidos'}`} />
                <Chip label={`${language === 'en' ? 'Total' : 'Total'}: ${formatMerchMoney(orderSummary.totalMinor, 'USD')}`} />
                {selectedStore.permissions?.finance && <Chip label={`${language === 'en' ? 'TDF commission' : 'Comisión TDF'}: ${formatMerchMoney(orderSummary.commissionMinor, 'USD')}`} />}
                {selectedStore.permissions?.finance && <Chip label={`${language === 'en' ? 'Seller net' : 'Neto vendedor'}: ${formatMerchMoney(orderSummary.sellerNetMinor, 'USD')}`} />}
              </Stack>
              <Typography variant="caption" color="text.secondary">
                {language === 'en' ? 'The export reflects this filter and excludes customer personal data. Finance columns appear only with finance permission.' : 'La exportación respeta este filtro y excluye datos personales del cliente. Las columnas financieras aparecen solo con permiso de finanzas.'}
              </Typography>
            </>}
            {orders.isFetching && <Box textAlign="center"><CircularProgress size={24} aria-label={language === 'en' ? 'Loading filtered orders' : 'Cargando pedidos filtrados'} /></Box>}
            {orders.isError && <Alert severity="error">{language === 'en' ? 'Orders could not be loaded. Check the filter and your permission.' : 'No se pudieron cargar los pedidos. Comprueba el filtro y tu permiso.'}</Alert>}
            <Stack spacing={2}>{orders.data?.map((order) => {
              const sellerOrder = order as typeof order & { shippingMethod?: string };
              const trackingValue = tracking[order.id] ?? { carrier: '', number: '', url: '' };
              const payloadBase = { publicNote: null, privateNote: null, carrier: null, trackingNumber: null, trackingUrl: null };
              return <Paper key={order.id} variant="outlined" sx={{ p: 2 }}><Stack spacing={2}>
                <Box><Typography fontWeight={800}>{order.orderNumber}</Typography><Typography>{formatMerchMoney(order.totalMinor, order.currency)} · {merchStatusLabel(order.paymentStatus, language)} · {merchStatusLabel(order.fulfillmentStatus, language)}</Typography></Box>
                {selectedStore.permissions?.fulfillment && order.paymentStatus === 'paid' && <Stack spacing={1}>
                  {order.fulfillmentStatus === 'pending' && <Button sx={{ alignSelf: 'flex-start' }} onClick={() => fulfillmentMutation.mutate({ orderId: order.id, payload: { ...payloadBase, status: 'preparing' } })}>{language === 'en' ? 'Start preparing' : 'Preparar'}</Button>}
                  {order.fulfillmentStatus === 'preparing' && sellerOrder.shippingMethod === 'coordinated_pickup' && <Button sx={{ alignSelf: 'flex-start' }} onClick={() => fulfillmentMutation.mutate({ orderId: order.id, payload: { ...payloadBase, status: 'ready_for_pickup' } })}>{language === 'en' ? 'Pickup ready' : 'Listo para retirar'}</Button>}
                  {order.fulfillmentStatus === 'preparing' && sellerOrder.shippingMethod === 'national_shipping' && <Stack component="form" direction={{ xs: 'column', md: 'row' }} spacing={1} onSubmit={(event) => { event.preventDefault(); fulfillmentMutation.mutate({ orderId: order.id, payload: { status: 'shipped', publicNote: language === 'en' ? 'Your order was shipped.' : 'Tu pedido fue enviado.', privateNote: null, carrier: trackingValue.carrier, trackingNumber: trackingValue.number, trackingUrl: trackingValue.url.trim().length > 0 ? trackingValue.url.trim() : null } }); }}>
                    <TextField required size="small" label={language === 'en' ? 'Carrier' : 'Transportista'} value={trackingValue.carrier} onChange={(event) => setTracking({ ...tracking, [order.id]: { ...trackingValue, carrier: event.target.value } })} />
                    <TextField required size="small" label="Tracking" value={trackingValue.number} onChange={(event) => setTracking({ ...tracking, [order.id]: { ...trackingValue, number: event.target.value } })} />
                    <TextField size="small" type="url" label="Tracking URL" value={trackingValue.url} onChange={(event) => setTracking({ ...tracking, [order.id]: { ...trackingValue, url: event.target.value } })} />
                    <Button type="submit">{language === 'en' ? 'Mark shipped' : 'Marcar enviado'}</Button>
                  </Stack>}
                </Stack>}
              </Stack></Paper>;
            })}{orders.data?.length === 0 && !orders.isFetching && <Typography color="text.secondary">{language === 'en' ? 'No orders match this filter.' : 'No hay pedidos que coincidan con este filtro.'}</Typography>}</Stack>
          </Stack>
        </Paper>}

        {tab === 'orders' && selectedStore.permissions?.orders && <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Typography component="h2" variant="h5" fontWeight={800} mb={1}>{language === 'en' ? 'Requests and issues' : 'Solicitudes e incidencias'}</Typography><Alert severity="info" sx={{ mb: 2 }}>{language === 'en' ? 'Refunds, disputes, fraud, and paid cancellations require independent staff review. Updating a case never changes payment or refund state.' : 'Los reembolsos, disputas, fraude y cancelaciones pagadas requieren revisión independiente. Actualizar un caso nunca cambia por sí solo el pago ni el reembolso.'}</Alert>{issues.isError && <Alert severity="error">{language === 'en' ? 'The issue queue could not be loaded.' : 'No se pudo cargar la cola de incidencias.'}</Alert>}<Stack spacing={2}>{issues.data?.map((item) => { const response = issueResponses[item.id] ?? ''; const terminal = ['resolved','rejected','cancelled'].includes(item.status); const financial = ['cancellation','refund','dispute','fraud'].includes(item.issueType); return <Paper key={item.id} variant="outlined" sx={{ p: 2 }}><Stack spacing={1.5}><Box><Typography fontWeight={800}>{item.orderNumber} · {merchStatusLabel(item.issueType, language)}</Typography><Typography variant="body2">{merchStatusLabel(item.status, language)} · {item.customerName}</Typography></Box><Typography>{item.message}</Typography>{item.resolution && <Alert severity="success">{item.resolution}</Alert>}{!terminal && <><TextField multiline minRows={2} inputProps={{ maxLength: 5000 }} label={language === 'en' ? 'Public response to the buyer' : 'Respuesta pública para el comprador'} value={response} onChange={(event) => setIssueResponses({ ...issueResponses, [item.id]: event.target.value })} /><Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>{item.status === 'open' && <Button onClick={() => issueMutation.mutate({ issueId: item.id, status: 'seller_review' })}>{language === 'en' ? 'Start review' : 'Iniciar revisión'}</Button>}<Button disabled={response.trim().length < 1} onClick={() => issueMutation.mutate({ issueId: item.id, status: 'awaiting_buyer' })}>{language === 'en' ? 'Ask buyer' : 'Consultar al comprador'}</Button><Button onClick={() => issueMutation.mutate({ issueId: item.id, status: 'staff_review' })}>{language === 'en' ? 'Escalate to TDF' : 'Escalar a TDF'}</Button>{!financial && <Button disabled={response.trim().length < 10} onClick={() => issueMutation.mutate({ issueId: item.id, status: 'resolved' })}>{language === 'en' ? 'Resolve' : 'Resolver'}</Button>}</Stack></>}</Stack></Paper>; })}{issues.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No issues reported.' : 'No hay incidencias reportadas.'}</Typography>}</Stack>{issueMutation.isError && <Alert severity="error" sx={{ mt: 2 }}>{language === 'en' ? 'The case changed or this action requires staff review. Reload and retry.' : 'El caso cambió o esta acción requiere revisión de TDF. Recarga e intenta otra vez.'}</Alert>}</Paper>}

        {tab === 'team' && <Stack spacing={3}>
          <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
            <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Store team' : 'Equipo de la tienda'}</Typography>
            <Stack spacing={1}>{members.data?.map((member) => <Stack key={member.id} direction="row" justifyContent="space-between"><Box><Typography fontWeight={800}>{member.displayName}</Typography><Typography variant="body2">{member.username ? `@${member.username} · ` : ''}{member.role} · {merchStatusLabel(member.status, language)}</Typography></Box></Stack>)}</Stack>
          </Paper>
          {selectedStore.permissions?.settings && <Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); inviteMutation.mutate(); }}>
            <Stack spacing={2}>
              <Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Invite collaborator' : 'Invitar colaborador'}</Typography>
              <PartySelector value={selectedMember} onChange={setSelectedMember} field={{ label: language === 'en' ? 'Search name, artist name or @username' : 'Buscar nombre, nombre artístico o @username', required: true }} search={{ context: 'crm_assignment', kind: 'any', accountOnly: true, excludedPartyIds: members.data?.map((member) => member.partyId) ?? [] }} />
              <Stack direction="row" flexWrap="wrap">{(Object.keys(memberPermissions) as (keyof MerchPermissions)[]).map((permission) => <FormControlLabel key={permission} control={<Checkbox checked={memberPermissions[permission]} onChange={(event) => setMemberPermissions({ ...memberPermissions, [permission]: event.target.checked })} />} label={permission} />)}</Stack>
              {inviteMutation.isError && <Alert severity="error">{inviteMutation.error instanceof Error ? inviteMutation.error.message : 'Error'}</Alert>}
              <Button type="submit" variant="contained" disabled={!selectedMember || inviteMutation.isPending}>{language === 'en' ? 'Send invitation' : 'Enviar invitación'}</Button>
            </Stack>
          </Paper>}
        </Stack>}

        {tab === 'settings' && <Stack spacing={3}><Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); policyMutation.mutate(); }}><Stack spacing={2}><Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Versioned policies' : 'Políticas versionadas'}</Typography><Alert severity="info">{language === 'en' ? 'Changes apply only to future orders; existing order snapshots never change.' : 'Los cambios aplican solo a pedidos futuros; los snapshots existentes nunca cambian.'}</Alert><TextField required multiline minRows={3} inputProps={{ minLength: 20 }} label={language === 'en' ? 'Shipping policy' : 'Política de envío'} value={policy.shippingPolicy} onChange={(event) => setPolicy({ ...policy, shippingPolicy: event.target.value })} /><TextField required multiline minRows={3} inputProps={{ minLength: 20 }} label={language === 'en' ? 'Return policy' : 'Política de devoluciones'} value={policy.returnPolicy} onChange={(event) => setPolicy({ ...policy, returnPolicy: event.target.value })} /><TextField type="email" label={language === 'en' ? 'Support email (optional)' : 'Email de soporte (opcional)'} value={policy.supportEmail} onChange={(event) => setPolicy({ ...policy, supportEmail: event.target.value })} />{policyMutation.isSuccess && <Alert severity="success">{language === 'en' ? 'Policy version saved for future orders.' : 'Versión de políticas guardada para pedidos futuros.'}</Alert>}<Button type="submit" variant="contained" disabled={!selectedStore.permissions?.settings || policyMutation.isPending}>{language === 'en' ? 'Save new version' : 'Guardar nueva versión'}</Button></Stack></Paper><Paper component="form" variant="outlined" sx={{ p: 3, borderRadius: 3 }} onSubmit={(event) => { event.preventDefault(); zoneMutation.mutate(); }}><Stack spacing={2}><Typography component="h2" variant="h5" fontWeight={800}>{language === 'en' ? 'Delivery zone' : 'Zona de entrega'}</Typography><TextField required label={language === 'en' ? 'Name' : 'Nombre'} value={zone.name} onChange={(event) => setZone({ ...zone, name: event.target.value })} /><TextField select label={language === 'en' ? 'Method' : 'Método'} value={zone.deliveryMethod} onChange={(event) => setZone({ ...zone, deliveryMethod: event.target.value as typeof zone.deliveryMethod })}><MenuItem value="national_shipping">{language === 'en' ? 'National shipping' : 'Envío nacional'}</MenuItem><MenuItem value="coordinated_pickup">{language === 'en' ? 'Coordinated pickup' : 'Retiro coordinado'}</MenuItem></TextField><TextField required type="number" label={language === 'en' ? 'Rate (cents)' : 'Tarifa (centavos)'} value={zone.rateMinor} onChange={(event) => setZone({ ...zone, rateMinor: Number(event.target.value) })} />{zoneMutation.isSuccess && <Alert severity="success">{language === 'en' ? 'Delivery zone saved.' : 'Zona de entrega guardada.'}</Alert>}<Button type="submit" variant="contained" disabled={!selectedStore.permissions?.settings || zoneMutation.isPending}>{language === 'en' ? 'Add delivery zone' : 'Agregar zona'}</Button></Stack></Paper></Stack>}
        {tab === 'catalog' && selectedStore.permissions?.stock && <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Stock control' : 'Control de stock'}</Typography><Stack spacing={2}>{products.data?.flatMap((item) => (item.variants ?? []).map((variant) => <VariantStockEditor key={`${variant.id}:${variant.version}`} storeId={selectedStore.id} variant={variant} language={language} onSaved={() => void client.invalidateQueries({ queryKey: ['merch-seller-products', selectedStore.id] })} />))}{products.data?.every((item) => (item.variants?.length ?? 0) === 0) && <Typography color="text.secondary">{language === 'en' ? 'No variants to manage.' : 'No hay variantes para gestionar.'}</Typography>}</Stack></Paper>}
      </Stack>
    </Box>
  );
}
