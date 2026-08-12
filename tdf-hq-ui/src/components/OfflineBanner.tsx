import { Alert, Collapse } from '@mui/material';
import WifiOffIcon from '@mui/icons-material/WifiOff';
import { useOffline } from '../hooks/useOffline';

export function OfflineBanner() {
  const isOffline = useOffline();
  return (
    <Collapse in={isOffline}>
      <Alert
        severity="warning"
        icon={<WifiOffIcon />}
        sx={{ borderRadius: 0, py: 0.5, '& .MuiAlert-message': { fontSize: '0.8125rem' } }}
      >
        Sin conexión — algunas funciones no estarán disponibles
      </Alert>
    </Collapse>
  );
}
