import { getContrastRatio } from '@mui/material/styles';
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
  confirmDisabled?: boolean;
  cancelLabel?: string;
  confirmingLabel?: string;
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
  confirmDisabled = false,
  cancelLabel = 'Cancelar',
  confirmingLabel = 'Confirmando…',
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
          {cancelLabel}
        </Button>
        <Button
          onClick={onConfirm}
          variant="contained"
          color={confirmButtonColor}
          sx={severity === 'danger' ? (theme) => {
            const background = theme.palette.error.dark;
            const foreground = getContrastRatio(background, theme.palette.common.white) >= 4.5
              ? theme.palette.common.white : theme.palette.common.black;
            return {
              backgroundColor: background, color: foreground,
              '&:hover': { backgroundColor: background, color: foreground },
              '&.Mui-disabled': { backgroundColor: theme.palette.action.disabledBackground, color: theme.palette.action.disabled },
            };
          } : undefined}
          disabled={confirming || confirmDisabled}
          startIcon={confirming ? <CircularProgress size={16} /> : undefined}
        >
          {confirming ? confirmingLabel : confirmLabel}
        </Button>
      </DialogActions>
    </Dialog>
  );
}
