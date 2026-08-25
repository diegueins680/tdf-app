import { Box, Typography } from "@mui/material";

export default function BarStaffPage() {
  return (
    <Box p={2}>
      <Typography variant="h5" gutterBottom>
        Personal del bar
      </Typography>
      <Typography variant="body1" color="text.secondary">
        Gestiona al personal de la barra y los turnos de cada evento (en desarrollo).
      </Typography>
    </Box>
  );
}
