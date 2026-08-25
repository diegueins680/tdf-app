import { Button, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle } from '@mui/material';

interface EnrollmentSuccessDialogProps {
  open: boolean;
  onClose: () => void;
  message?: string;
  title?: string;
}

const defaultMessage = 'Recibimos tu solicitud. Revisa el estado de pago y del cupo antes de considerarlo confirmado.';

export default function EnrollmentSuccessDialog({
  open,
  onClose,
  message = defaultMessage,
  title = 'Solicitud recibida',
}: EnrollmentSuccessDialogProps) {
  const titleId = 'enrollment-success-dialog-title';
  const descriptionId = 'enrollment-success-dialog-description';

  return (
    <Dialog
      open={open}
      onClose={onClose}
      fullWidth
      maxWidth="xs"
      aria-labelledby={titleId}
      aria-describedby={descriptionId}
    >
      <DialogTitle id={titleId}>{title}</DialogTitle>
      <DialogContent>
        <DialogContentText id={descriptionId}>{message}</DialogContentText>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} variant="contained">
          Entendido
        </Button>
      </DialogActions>
    </Dialog>
  );
}
