import { Box, Typography } from "@mui/material";

export default function BarInventoryPage() {
  return (
    <Box p={2}>
      <Typography variant="h5" gutterBottom>
        Inventario del bar
      </Typography>
      <Typography variant="body1" color="text.secondary">
        Administra transferencias, existencias y ajustes del bar desde este módulo (en desarrollo).
      </Typography>
    </Box>
  );
}
