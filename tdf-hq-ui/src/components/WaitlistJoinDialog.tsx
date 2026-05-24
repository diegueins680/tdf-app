import { useState } from 'react';
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
  const qc = useQueryClient();
  const [email, setEmail] = useState('');
  const [quantity, setQuantity] = useState(WAITLIST_DEFAULT_QUANTITY);
  const [error, setError] = useState<string | null>(null);

  const joinMutation = useMutation({
    mutationFn: () =>
      SocialEventsAPI.joinWaitlist(eventId, {
        wjEmail: email,
        wjQuantity: quantity,
      }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['waitlist', eventId] });
      onSuccess();
      handleClose();
    },
    onError: (err) => {
      setError(err instanceof Error ? err.message : 'Failed to join waitlist');
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!email) {
      setError('Please enter your email');
      return;
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError('Please enter a valid email address');
      return;
    }

    if (quantity < WAITLIST_MIN_QUANTITY || quantity > WAITLIST_MAX_QUANTITY) {
      setError('Quantity must be between 1 and 10');
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
  };

  const dialogContent = (
    <Dialog open={open} onClose={handleClose} maxWidth="sm" fullWidth>
      <DialogTitle>
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <NotificationsActiveIcon />
          Join Waitlist
        </Box>
      </DialogTitle>

      <DialogContent>
        <Typography variant="body1" gutterBottom>
          {eventTitle}
        </Typography>
        {tierName && (
          <Typography variant="body2" color="text.secondary" gutterBottom>
            Tier: {tierName}
          </Typography>
        )}

        <Alert severity="info" sx={{ my: 2 }}>
          <Typography variant="body2">
            We'll notify you by email when tickets become available. You'll have {WAITLIST_PURCHASE_WINDOW_HOURS} hours to purchase your tickets.
          </Typography>
        </Alert>

        <Box component="form" onSubmit={handleSubmit}>
          <TextField
            label="Email Address"
            type="email"
            fullWidth
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            margin="normal"
            placeholder="your@email.com"
            helperText="We'll send notifications to this email"
          />
          <TextField
            label="Number of Tickets"
            type="number"
            fullWidth
            required
            value={quantity}
            onChange={(e) => setQuantity(parseWaitlistQuantity(e.target.value))}
            margin="normal"
            InputProps={{ inputProps: { min: WAITLIST_MIN_QUANTITY, max: WAITLIST_MAX_QUANTITY } }}
            helperText="Maximum 10 tickets per request"
          />

          {error && (
            <Alert severity="error" sx={{ mt: 2 }}>
              {error}
            </Alert>
          )}
        </Box>
      </DialogContent>

      <DialogActions>
        <Button onClick={handleClose} disabled={joinMutation.isPending}>
          Cancel
        </Button>
        <Button
          onClick={handleSubmit}
          variant="contained"
          disabled={joinMutation.isPending}
        >
          {joinMutation.isPending ? <CircularProgress size={WAITLIST_ACTION_SPINNER_SIZE_PX} /> : 'Join Waitlist'}
        </Button>
      </DialogActions>
    </Dialog>
  );

  return dialogContent;
}
