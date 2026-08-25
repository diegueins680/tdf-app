import { useEffect, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  TextField,
  Typography,
  Box,
  CircularProgress,
  Alert,
} from '@mui/material';
import NotificationsActiveIcon from '@mui/icons-material/NotificationsActive';
import { SocialEventsAPI } from '../api/socialEvents';
import {
  WAITLIST_DEFAULT_QUANTITY,
  WAITLIST_MAX_QUANTITY,
  WAITLIST_MIN_QUANTITY,
  parseWaitlistQuantity,
} from './WaitlistJoinDialog.logic';

interface WaitlistJoinDialogProps {
  open: boolean;
  onClose: () => void;
  eventId: string;
  eventTitle: string;
  tierName?: string;
  onSuccess: () => void;
}

const WAITLIST_PURCHASE_WINDOW_HOURS = 2 * 10 + 4;
const WAITLIST_ACTION_SPINNER_SIZE_PX = 2 * 10 + 4;

/**
 * Contract:
 * @precondition eventId identifies the event waitlist being joined.
 * @precondition onSuccess can be called after the join mutation commits successfully.
 * @invariant submitted quantities are validated inside the visible 1-10 ticket range before mutation.
 * @postcondition successful joins invalidate the event waitlist query, notify the parent, and reset local form state.
 */
export function WaitlistJoinDialog({ open, onClose, eventId, eventTitle, tierName, onSuccess }: WaitlistJoinDialogProps) {
  /*
   * precondition: eventId identifies one waitlist.
   * invariant: submitted quantity is validated against visible bounds.
   * postcondition: success resets form state.
   */
  const { t } = useTranslation();
  const qc = useQueryClient();
  const [email, setEmail] = useState('');
  const [quantity, setQuantity] = useState(WAITLIST_DEFAULT_QUANTITY);
  const [error, setError] = useState<string | null>(null);
  const previousFocusRef = useRef<Element | null>(null);

  const joinMutation = useMutation({
    mutationFn: () =>
      SocialEventsAPI.joinWaitlist(eventId, {
        wjEmail: email,
        wjQuantity: quantity,
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['waitlist', eventId] });
      onSuccess();
      handleClose();
    },
    onError: (err) => {
      setError(err instanceof Error ? err.message : t('waitlist.errors.joinFailed'));
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!email) {
      setError(t('waitlist.errors.emailRequired'));
      return;
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError(t('waitlist.errors.invalidEmail'));
      return;
    }

    if (quantity < WAITLIST_MIN_QUANTITY || quantity > WAITLIST_MAX_QUANTITY) {
      setError(t('waitlist.errors.invalidQuantity'));
      return;
    }

    setError(null);
    joinMutation.mutate();
  };

  const handleClose = () => {
    setEmail('');
    setQuantity(WAITLIST_DEFAULT_QUANTITY);
    setError(null);
    onClose();
    // Restore focus to the element that triggered the dialog
    if (previousFocusRef.current instanceof HTMLElement) {
      previousFocusRef.current.focus();
    }
  };

  const handleOpen = () => {
    previousFocusRef.current = document.activeElement;
  };

  useEffect(() => {
    if (open) {
      handleOpen();
    }
  }, [open]);

  const dialogContent = (
    <Dialog open={open} onClose={handleClose} maxWidth="sm" fullWidth aria-labelledby="waitlist-title">
      <DialogTitle id="waitlist-title">
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <NotificationsActiveIcon />
          {t('waitlist.title')}
        </Box>
      </DialogTitle>

      <DialogContent>
        <Typography variant="body1" gutterBottom>
          {eventTitle}
        </Typography>
        {tierName && (
          <Typography variant="body2" color="text.secondary" gutterBottom>
            {t('waitlist.tier')} {tierName}
          </Typography>
        )}

        <Alert severity="info" sx={{ my: 2 }}>
          <Typography variant="body2">
            {t('waitlist.info', { hours: WAITLIST_PURCHASE_WINDOW_HOURS })}
          </Typography>
        </Alert>

        <Box component="form" id="waitlist-form" onSubmit={handleSubmit}>
          <TextField
            label={t('waitlist.emailLabel')}
            type="email"
            fullWidth
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            margin="normal"
            placeholder={t('waitlist.emailPlaceholder')}
            helperText={t('waitlist.emailHelper')}
          />
          <TextField
            label={t('waitlist.quantityLabel')}
            type="number"
            fullWidth
            required
            value={quantity}
            onChange={(e) => setQuantity(parseWaitlistQuantity(e.target.value))}
            margin="normal"
            InputProps={{ inputProps: { min: WAITLIST_MIN_QUANTITY, max: WAITLIST_MAX_QUANTITY } }}
            helperText={t('waitlist.quantityHelper')}
          />

          {error && (
            <Alert severity="error" role="alert" sx={{ mt: 2 }}>
              {error}
            </Alert>
          )}
        </Box>
      </DialogContent>

      <DialogActions>
        <Button onClick={handleClose} disabled={joinMutation.isPending}>
          {t('waitlist.cancel')}
        </Button>
        <Button
          type="submit"
          form="waitlist-form"
          variant="contained"
          disabled={joinMutation.isPending}
        >
          {joinMutation.isPending ? <CircularProgress size={WAITLIST_ACTION_SPINNER_SIZE_PX} /> : t('waitlist.joinButton')}
        </Button>
      </DialogActions>
    </Dialog>
  );

  return dialogContent;
}
