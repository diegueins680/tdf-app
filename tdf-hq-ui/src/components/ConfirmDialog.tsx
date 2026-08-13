import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Typography,
  CircularProgress,
} from '@mui/material';

interface ConfirmDialogProps {
  open: boolean;
  onClose: () => void;
  onConfirm: () => void;
  title: string;
  description: string;
  confirmLabel?: string;
  severity?: 'danger' | 'warning' | 'info';
  confirming?: boolean;
}

export default function ConfirmDialog({
  open,
  onClose,
  onConfirm,
  title,
  description,
  confirmLabel = 'Confirmar',
  severity = 'warning',
  confirming = false,
}: ConfirmDialogProps) {
  const titleId = 'confirm-dialog-title';
  const descriptionId = 'confirm-dialog-description';

  const confirmButtonColor: 'error' | 'warning' | 'info' =
    severity === 'danger' ? 'error' : severity;

  return (
    <Dialog
      open={open}
      onClose={onClose}
      aria-labelledby={titleId}
      aria-describedby={descriptionId}
    >
      <DialogTitle id={titleId}>{title}</DialogTitle>
      <DialogContent>
        <Typography id={descriptionId}>{description}</Typography>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} disabled={confirming}>
          Cancelar
        </Button>
        <Button
          onClick={onConfirm}
          variant="contained"
          color={confirmButtonColor}
          disabled={confirming}
          startIcon={confirming ? <CircularProgress size={16} /> : undefined}
        >
          {confirming ? 'Confirmando…' : confirmLabel}
        </Button>
      </DialogActions>
    </Dialog>
  );
}
