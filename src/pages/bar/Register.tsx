import { useMemo, useState } from "react";
import { useMutation } from "@tanstack/react-query";
import {
  Alert,
  Button,
  Divider,
  Grid,
  Paper,
  Stack,
  TextField,
  Typography,
} from "@mui/material";
import { useSearchParams } from "react-router-dom";
import {
  cashDrop,
  closeRegister,
  countRegister,
  openRegister,
  type RegisterCountPayload,
  type RegisterCountResult,
} from "../../api/bar";

const USD_DENOMS = [10000, 5000, 2000, 1000, 500, 100, 25, 10, 5, 1] as const; // cents

function extractShiftId(value: unknown): number {
  if (!value || typeof value !== "object") {
    return 0;
  }
  const candidate = value as Record<string, unknown>;
  for (const key of ["shiftId", "id", "key"]) {
    const raw = candidate[key];
    if (typeof raw === "number") {
      return raw;
    }
    if (typeof raw === "string" && raw.trim()) {
      const parsed = Number(raw);
      if (!Number.isNaN(parsed)) {
        return parsed;
      }
    }
  }
  return 0;
}

export default function RegisterPage() {
  const [searchParams] = useSearchParams();
  const bookingId = Number(searchParams.get("bookingId") ?? "0");
  const stationId = Number(searchParams.get("stationId") ?? "0");
  const initialShiftId = Number(searchParams.get("shiftId") ?? "0");

  const [openingFloat, setOpeningFloat] = useState(0);
  const [shift, setShift] = useState<unknown>(null);
  const [shiftId, setShiftId] = useState(initialShiftId);
  const [dropAmount, setDropAmount] = useState(0);
  const [counts, setCounts] = useState<Record<number, number>>({});
  const [countResult, setCountResult] = useState<RegisterCountResult | null>(null);

  const effectiveShiftId = shiftId || extractShiftId(shift);

  const sumDeclared = useMemo(
    () => Object.entries(counts).reduce((total, [den, qty]) => total + Number(den) * Number(qty), 0),
    [counts],
  );

  const openMutation = useMutation({
    mutationFn: (floatCents: number) =>
      openRegister({
        barEventId: bookingId > 0 ? bookingId : undefined,
        stationId,
        openingFloatCents: floatCents,
      }),
    onSuccess: (data) => {
      setShift(data);
      setShiftId(extractShiftId(data));
    },
  });

  const dropMutation = useMutation({
    mutationFn: () =>
      cashDrop({
        shiftId: effectiveShiftId,
        amountCents: dropAmount,
      }),
    onSuccess: () => {
      setDropAmount(0);
    },
  });

  const countMutation = useMutation({
    mutationFn: (): Promise<RegisterCountResult> => {
      const payload: RegisterCountPayload = {
        shiftId: effectiveShiftId,
        counts: Object.entries(counts).map(([den, qty]) => [Number(den), Number(qty)] as [number, number]),
      };
      return countRegister(payload);
    },
    onSuccess: (result) => {
      setCountResult(result);
    },
  });

  const closeMutation = useMutation({
    mutationFn: () =>
      closeRegister({
        shiftIdClose: effectiveShiftId,
        declaredCloseCents: sumDeclared,
      }),
    onSuccess: () => {
      setCountResult(null);
    },
  });

  const disableRegisterActions = bookingId <= 0 || stationId <= 0;

  return (
    <Grid container spacing={2} padding={2}>
      <Grid item xs={12}>
        {disableRegisterActions && (
          <Alert severity="info">
            Proporciona `bookingId` y `stationId` en la URL (?bookingId=...&stationId=...) para operar la caja.
          </Alert>
        )}
        {openMutation.error && (
          <Alert severity="error">
            Error al abrir caja:{" "}
            {openMutation.error instanceof Error ? openMutation.error.message : String(openMutation.error ?? "")}
          </Alert>
        )}
        {dropMutation.error && (
          <Alert severity="error">
            Error al registrar depósito:{" "}
            {dropMutation.error instanceof Error ? dropMutation.error.message : String(dropMutation.error ?? "")}
          </Alert>
        )}
        {countMutation.error && (
          <Alert severity="error">
            Error al contar caja:{" "}
            {countMutation.error instanceof Error ? countMutation.error.message : String(countMutation.error ?? "")}
          </Alert>
        )}
        {closeMutation.error && (
          <Alert severity="error">
            Error al cerrar turno:{" "}
            {closeMutation.error instanceof Error ? closeMutation.error.message : String(closeMutation.error ?? "")}
          </Alert>
        )}
      </Grid>

      <Grid item xs={12} md={6}>
        <Paper sx={{ p: 2 }}>
          <Typography variant="h6">Abrir caja</Typography>
          <Stack direction="row" spacing={1} mt={2}>
            <TextField
              label="Fondo inicial ($)"
              type="number"
              inputProps={{ min: 0, step: "0.01" }}
              value={openingFloat / 100}
              onChange={(event) => setOpeningFloat(Math.max(0, Math.round(Number(event.target.value || 0) * 100)))}
              fullWidth
            />
            <Button
              variant="contained"
              disabled={disableRegisterActions || openMutation.isPending}
              onClick={() => openMutation.mutate(openingFloat)}
            >
              {openMutation.isPending ? "Abriendo…" : "Abrir"}
            </Button>
          </Stack>
          <TextField
            sx={{ mt: 2 }}
            label="ID de turno (manual)"
            type="number"
            value={shiftId || ""}
            onChange={(event) => setShiftId(Number(event.target.value))}
            helperText="Completa manualmente si ya tienes un turno abierto."
          />
          {effectiveShiftId > 0 && (
            <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
              Turno activo: #{effectiveShiftId}
            </Typography>
          )}
        </Paper>

        <Paper sx={{ p: 2, mt: 2 }}>
          <Typography variant="h6">Depósito de efectivo</Typography>
          <Stack direction="row" spacing={1} mt={2}>
            <TextField
              label="Depósito ($)"
              type="number"
              inputProps={{ min: 0, step: "0.01" }}
              value={dropAmount / 100}
              onChange={(event) => setDropAmount(Math.max(0, Math.round(Number(event.target.value || 0) * 100)))}
              fullWidth
            />
            <Button
              variant="outlined"
              disabled={effectiveShiftId <= 0 || dropMutation.isPending}
              onClick={() => dropMutation.mutate()}
            >
              {dropMutation.isPending ? "Registrando…" : "Registrar"}
            </Button>
          </Stack>
        </Paper>
      </Grid>

      <Grid item xs={12} md={6}>
        <Paper sx={{ p: 2, height: "100%" }}>
          <Typography variant="h6">Conteo de caja</Typography>
          <Divider sx={{ my: 2 }} />
          <Grid container spacing={1}>
            {USD_DENOMS.map((den) => (
              <Grid item xs={6} key={den}>
                <Stack direction="row" spacing={1} alignItems="center">
                  <Typography sx={{ minWidth: 90 }}>${(den / 100).toFixed(2)}</Typography>
                  <TextField
                    type="number"
                    size="small"
                    inputProps={{ min: 0, step: 1 }}
                    value={counts[den] ?? ''}
                    onChange={(event) => {
                      const raw = event.target.value;
                      setCounts((prev) => {
                        const next = { ...prev };
                        if (!raw.trim()) {
                          delete next[den];
                          return next;
                        }
                        const parsed = Number(raw);
                        if (Number.isNaN(parsed)) {
                          delete next[den];
                          return next;
                        }
                        next[den] = Math.max(0, Math.round(parsed));
                        return next;
                      });
                    }}
                  />
                </Stack>
              </Grid>
            ))}
          </Grid>
          <Divider sx={{ my: 2 }} />
          <Typography>Declarado: ${(sumDeclared / 100).toFixed(2)}</Typography>
          {countResult && (
            <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
              Esperado: ${(countResult.expectedCents / 100).toFixed(2)} · Diferencia:{" "}
              {(countResult.overShortCents / 100).toFixed(2)}
            </Typography>
          )}
          <Stack direction="row" spacing={1} mt={2}>
            <Button
              variant="outlined"
              disabled={effectiveShiftId <= 0 || countMutation.isPending}
              onClick={() => countMutation.mutate()}
            >
              {countMutation.isPending ? "Calculando…" : "Calcular diferencia"}
            </Button>
            <Button
              variant="contained"
              color="primary"
              disabled={effectiveShiftId <= 0 || closeMutation.isPending}
              onClick={() => closeMutation.mutate()}
            >
              {closeMutation.isPending ? "Cerrando…" : "Cerrar turno"}
            </Button>
          </Stack>
        </Paper>
      </Grid>
    </Grid>
  );
}
