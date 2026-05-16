import { useMemo, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { Alert, Button, Chip, Divider, Grid, Paper, Stack, TextField, Typography } from "@mui/material";
import { useSearchParams } from "react-router-dom";
import {
  getMenu,
  postSale,
  type MenuItem,
  type PaymentMethod,
  type SaleItem,
  type SalePayload,
} from "../../api/bar";

const PAYMENT_OPTIONS: PaymentMethod[] = ["PayCash", "PayCard", "PayTransfer", "PayComp"];

function formatCurrency(cents: number) {
  return `$${(cents / 100).toFixed(2)}`;
}

export default function SellPage() {
  const [searchParams] = useSearchParams();
  const bookingId = Number(searchParams.get("bookingId") ?? "0");
  const stationId = Number(searchParams.get("stationId") ?? "0");
  const shiftId = Number(searchParams.get("shiftId") ?? "0");

  const { data: menu, isLoading, error: menuError } = useQuery({
    queryKey: ["bar-menu", bookingId],
    queryFn: () => getMenu(bookingId),
    enabled: bookingId > 0,
  });

  const [items, setItems] = useState<SaleItem[]>([]);
  const [tipCents, setTipCents] = useState(0);
  const [discountCents, setDiscountCents] = useState(0);
  const [paymentMethod, setPaymentMethod] = useState<PaymentMethod>("PayCash");

  const addItem = (mi: MenuItem) => {
    setItems((prev) => {
      const idx = prev.findIndex((entry) => entry.productId === Number(mi.miProductId));
      if (idx >= 0) {
        const next = [...prev];
        next[idx] = { ...next[idx], qty: next[idx].qty + 1 };
        return next;
      }
      return [
        ...prev,
        {
          productId: Number(mi.miProductId),
          qty: 1,
          unitPriceCents: mi.miUnitPriceCents,
        },
      ];
    });
  };

  const subtotal = useMemo(
    () => items.reduce((sum, item) => sum + item.qty * item.unitPriceCents, 0),
    [items],
  );

  const taxCents = 0; // taxes handled server-side for now

  const total = useMemo(
    () => subtotal - discountCents + taxCents + tipCents,
    [subtotal, discountCents, taxCents, tipCents],
  );

  const canSubmit = bookingId > 0 && stationId > 0 && shiftId > 0 && items.length > 0;

  const {
    mutate: submitSale,
    isPending: isSubmitting,
    error: saleError,
  } = useMutation({
    mutationFn: (): Promise<unknown> => {
      const payload: SalePayload = {
        bookingId,
        stationId,
        shiftId,
        items,
        discountCents,
        taxCents,
        tipCents,
        paymentMethod,
      };
      return postSale(payload);
    },
    onSuccess: () => {
      setItems([]);
      setTipCents(0);
      setDiscountCents(0);
    },
  });

  const missingParams = bookingId <= 0 || stationId <= 0 || shiftId <= 0;

  return (
    <Grid container spacing={2} padding={2}>
      <Grid item xs={12}>
        {missingParams && (
          <Alert severity="info">
            Proporciona `bookingId`, `stationId` y `shiftId` en la URL (?bookingId=...&stationId=...&shiftId=...) para operar el punto de venta.
          </Alert>
        )}
        {menuError && (
          <Alert severity="error">
            No se pudo cargar el menú: {menuError instanceof Error ? menuError.message : String(menuError ?? "")}
          </Alert>
        )}
        {saleError && (
          <Alert severity="error">
            No se pudo registrar la venta: {saleError instanceof Error ? saleError.message : String(saleError ?? "")}
          </Alert>
        )}
      </Grid>

      <Grid item xs={12} md={8}>
        <Typography variant="h5" gutterBottom>
          Menú
        </Typography>
        <Divider sx={{ mb: 2 }} />
        {isLoading && <Typography>Cargando productos…</Typography>}
        {!isLoading && !menu?.length && (
          <Typography color="text.secondary">No hay productos configurados para este evento.</Typography>
        )}
        <Grid container spacing={1}>
          {menu?.map((mi) => (
            <Grid item xs={12} sm={6} lg={4} key={mi.miProductId}>
              <Paper
                role="button"
                tabIndex={0}
                sx={{ p: 2, cursor: "pointer" }}
                onClick={() => addItem(mi)}
                onKeyDown={(ev) => {
                  if (ev.key === "Enter" || ev.key === " ") {
                    ev.preventDefault();
                    addItem(mi);
                  }
                }}
              >
                <Typography fontWeight={600}>{mi.miName}</Typography>
                <Typography variant="body2" color="text.secondary">
                  {formatCurrency(mi.miUnitPriceCents)}
                </Typography>
                <Chip label={mi.miCategory} size="small" sx={{ mt: 1 }} />
              </Paper>
            </Grid>
          ))}
        </Grid>
      </Grid>

      <Grid item xs={12} md={4}>
        <Typography variant="h5" gutterBottom>
          Orden
        </Typography>
        <Divider sx={{ mb: 2 }} />
        <Stack spacing={1}>
          {items.map((item, idx) => (
            <Paper key={`${item.productId}-${idx}`} sx={{ p: 1.5, display: "flex", justifyContent: "space-between" }}>
              <Typography>{item.qty} × {formatCurrency(item.unitPriceCents)}</Typography>
              <Typography fontWeight={600}>{formatCurrency(item.qty * item.unitPriceCents)}</Typography>
            </Paper>
          ))}
          {items.length === 0 && (
            <Typography color="text.secondary">Agrega productos del menú para crear la orden.</Typography>
          )}

          <Divider />
          <TextField
            label="Propina ($)"
            type="number"
            inputProps={{ min: 0, step: "0.01" }}
            value={tipCents / 100}
            onChange={(event) => setTipCents(Math.max(0, Math.round(Number(event.target.value || 0) * 100)))}
          />
          <TextField
            label="Descuento ($)"
            type="number"
            inputProps={{ min: 0, step: "0.01" }}
            value={discountCents / 100}
            onChange={(event) =>
              setDiscountCents(Math.max(0, Math.round(Number(event.target.value || 0) * 100)))
            }
          />
          <Divider />
          <Typography>Subtotal: {formatCurrency(subtotal)}</Typography>
          <Typography>Impuesto: {formatCurrency(taxCents)}</Typography>
          <Typography variant="h6">Total: {formatCurrency(total)}</Typography>

          <Stack direction="row" spacing={1} flexWrap="wrap">
            {PAYMENT_OPTIONS.map((method) => (
              <Button
                key={method}
                variant={paymentMethod === method ? "contained" : "outlined"}
                onClick={() => setPaymentMethod(method)}
              >
                {method.replace("Pay", "")}
              </Button>
            ))}
          </Stack>

          <Button
            disabled={!canSubmit || isSubmitting}
            variant="contained"
            onClick={() => submitSale()}
          >
            {isSubmitting ? "Procesando…" : "Cobrar"}
          </Button>
        </Stack>
      </Grid>
    </Grid>
  );
}
