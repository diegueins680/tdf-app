import { Button, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle } from '@mui/material';

interface EnrollmentSuccessDialogProps {
  open: boolean;
  onClose: () => void;
  message?: string;
}

const defaultMessage = 'Felicitaciones, tu inscripción fue recibida exitosamente. Bienvenido a TDF Records!';

export default function EnrollmentSuccessDialog({ open, onClose, message = defaultMessage }: EnrollmentSuccessDialogProps) {
  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="xs">
      <DialogTitle>¡Inscripción confirmada!</DialogTitle>
      <DialogContent>
        <DialogContentText>{message}</DialogContentText>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} variant="contained">
          Entendido
        </Button>
      </DialogActions>
    </Dialog>
  );
}
