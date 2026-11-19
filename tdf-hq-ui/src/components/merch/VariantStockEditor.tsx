import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation } from '@tanstack/react-query';
import { Alert, Box, Button, Checkbox, FormControlLabel, Stack, TextField, Typography } from '@mui/material';
import { Merch, type MerchProduct, type MerchStockRequest } from '../../api/merch';
import { readMerchIntegerInput } from '../../utils/merch';

type ManagedVariant = NonNullable<MerchProduct['variants']>[number];
interface Props {
  storeId: string;
  variant: ManagedVariant;
  onRefresh: () => void;
  language: 'es' | 'en';
}
interface Snapshot {
  version: number;
  stockOnHand: number;
  reorderThreshold: number;
  active: boolean;
  reserved: number;
  sold: number;
}
interface Draft {
  stockOnHand: string;
  reorderThreshold: string;
  active: boolean;
}
const snapshot = (variant: ManagedVariant): Snapshot => ({
  version: variant.version,
  stockOnHand: variant.stockOnHand ?? 0,
  reorderThreshold: variant.reorderThreshold ?? 0,
  active: variant.active,
  reserved: variant.stockReserved ?? 0,
  sold: variant.stockSold ?? 0,
});
const draftFrom = (base: Snapshot): Draft => ({
  stockOnHand: String(base.stockOnHand), reorderThreshold: String(base.reorderThreshold), active: base.active,
});

function StockEditor({ storeId, variant, onRefresh, language }: Props) {
  const latest = useMemo(() => snapshot(variant), [variant]);
  const [editor, setEditor] = useState(() => ({ base: latest, draft: draftFrom(latest) }));
  const input = useRef<HTMLInputElement>(null);
  const en = language === 'en';
  const dirty = editor.draft.stockOnHand !== String(editor.base.stockOnHand)
    || editor.draft.reorderThreshold !== String(editor.base.reorderThreshold) || editor.draft.active !== editor.base.active;
  // Ignore older query responses after an accepted write. Same-version values
  // are compared as well, so inconsistent incoming metadata never rebases edits.
  const changed = latest.version > editor.base.version || (latest.version === editor.base.version && (
    latest.stockOnHand !== editor.base.stockOnHand || latest.reorderThreshold !== editor.base.reorderThreshold
    || latest.active !== editor.base.active
  ));
  const stock = readMerchIntegerInput(editor.draft.stockOnHand, language);
  const threshold = readMerchIntegerInput(editor.draft.reorderThreshold, language);
  const update = useMutation({
    mutationFn: (body: MerchStockRequest) => Merch.updateVariantStock(storeId, variant.id, body),
    onError: () => onRefresh(),
    onSuccess: (result) => {
      const committed = snapshot(result);
      setEditor({ base: committed, draft: draftFrom(committed) });
      onRefresh();
    },
  });
  const pending = update.isPending;
  const resetUpdate = update.reset;
  useEffect(() => {
    if (changed && !dirty && !pending) {
      setEditor({ base: latest, draft: draftFrom(latest) });
      resetUpdate();
    }
  }, [changed, dirty, pending, latest, resetUpdate]);

  const reviewLatest = (keepDraft: boolean) => {
    if (pending || !changed) return;
    setEditor((current) => ({ base: latest, draft: keepDraft ? current.draft : draftFrom(latest) }));
    resetUpdate();
    input.current?.focus();
  };
  const server = latest.version >= editor.base.version ? latest : editor.base;
  return <Stack component="form" aria-label={en ? `Stock for ${variant.name}` : `Stock de ${variant.name}`} spacing={1.5}
    onSubmit={(event) => {
      event.preventDefault();
      if (pending || changed || stock.value === null || threshold.value === null) return;
      update.mutate({ stockOnHand: stock.value, reorderThreshold: threshold.value, active: editor.draft.active, version: editor.base.version });
    }}>
    <Box><Typography fontWeight={700}>{variant.name}</Typography><Typography variant="caption">{variant.sku} · {en ? 'reserved' : 'reservado'} {server.reserved} · {en ? 'sold' : 'vendido'} {server.sold}</Typography></Box>
    <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} alignItems={{ md: 'flex-start' }}>
      <TextField required size="small" inputRef={input} inputProps={{ inputMode: 'numeric' }} label={en ? 'On hand' : 'Existencia'} value={editor.draft.stockOnHand}
        onChange={(event) => setEditor((current) => ({ ...current, draft: { ...current.draft, stockOnHand: event.target.value } }))}
        error={Boolean(stock.error)} helperText={stock.error ?? (en ? 'Whole units; enter 0 for none.' : 'Unidades enteras; usa 0 si no hay existencias.')} disabled={pending} />
      <TextField required size="small" inputProps={{ inputMode: 'numeric' }} label={en ? 'Low-stock alert' : 'Alerta de stock'} value={editor.draft.reorderThreshold}
        onChange={(event) => setEditor((current) => ({ ...current, draft: { ...current.draft, reorderThreshold: event.target.value } }))}
        error={Boolean(threshold.error)} helperText={threshold.error ?? (en ? 'Whole units; enter 0 for no alert.' : 'Unidades enteras; usa 0 para no alertar.')} disabled={pending} />
      <FormControlLabel control={<Checkbox checked={editor.draft.active} disabled={pending}
        onChange={(event) => setEditor((current) => ({ ...current, draft: { ...current.draft, active: event.target.checked } }))} />} label={en ? 'Active' : 'Activa'} />
      <Button type="submit" disabled={pending || changed || stock.value === null || threshold.value === null}>{en ? 'Update stock' : 'Actualizar stock'}</Button>
    </Stack>
    {changed && (dirty || pending) && <Alert severity="warning">
      <Stack spacing={1}>
        <Typography>{en ? 'Stock changed while you were editing. Your edits are still here and have not been saved.' : 'El stock cambió mientras editabas. Conservamos tu edición sin guardarla.'}</Typography>
        <Typography variant="body2">{en ? 'Current values' : 'Valores actuales'}: {en ? 'on hand' : 'existencia'} {latest.stockOnHand} · {en ? 'low-stock alert' : 'alerta'} {latest.reorderThreshold} · {en ? 'active' : 'activa'} {latest.active ? (en ? 'yes' : 'sí') : 'no'}.</Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Button type="button" disabled={pending} onClick={() => reviewLatest(true)}>{en ? 'Keep my edits' : 'Conservar mi edición'}</Button>
          <Button type="button" disabled={pending} onClick={() => reviewLatest(false)}>{en ? 'Use current values' : 'Usar valores actuales'}</Button>
        </Stack>
      </Stack>
    </Alert>}
    {update.isError && !changed && <Alert severity="error">{en ? 'Stock changed elsewhere or is below reserved/sold units. Your edits were kept. Review the current values and reserved/sold units before trying again.' : 'El stock cambió en otro lugar o es menor que las unidades reservadas/vendidas. Conservamos tu edición. Revisa los valores actuales y las unidades reservadas/vendidas antes de volver a intentar.'}</Alert>}
  </Stack>;
}

// Switching store/variant resets this editor; changing only the server version
// must not unmount a dirty draft. SessionQueryProvider owns principal isolation.
export default function VariantStockEditor(props: Props) {
  return <StockEditor key={`${props.storeId}:${props.variant.id}`} {...props} />;
}
