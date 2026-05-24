import { useEffect, useCallback, useReducer, useRef } from 'react';
import {
  TextField,
  Box,
  Typography,
  CircularProgress,
  Alert,
  InputAdornment,
  IconButton,
} from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ClearIcon from '@mui/icons-material/Clear';
import { SocialEventsAPI, type PromoCodeDTO } from '../api/socialEvents';
import { initialPromoCodeState, promoCodeReducer } from './PromoCodeField.logic';

interface PromoCodeFieldProps {
  eventId: string;
  tierId: string;
  onPromoApplied: (code: string | null) => void;
}

const PROMO_COPY = {
  label: 'Promo Code (Optional)',
  placeholder: 'ENTER-CODE-HERE',
  clear: 'Clear promo code',
  inactive: 'Promo code is not active',
  invalid: 'Invalid promo code',
  applied: 'Code applied',
  validUntil: 'Valid until',
  usesRemaining: 'uses remaining',
};

const PROMO_VALIDATION_SPINNER_SIZE_PX = 20;

/**
 * Contract:
 * @precondition eventId and tierId identify the event/tier whose promo policy is being checked.
 * @precondition onPromoApplied accepts either the canonical validated promo code or null for no valid promo.
 * @invariant only the most recent async validation request may publish validation state.
 * @postcondition inactive, invalid, empty, or stale codes report null to the parent.
 */
export function PromoCodeField({ eventId, tierId, onPromoApplied }: PromoCodeFieldProps) {
  const [{ code, debouncedCode, validating, validPromo, error }, dispatch] = useReducer(
    promoCodeReducer,
    initialPromoCodeState,
  );
  const inputRef = useRef<HTMLInputElement | null>(null);
  const validationRequestRef = useRef(0);

  useEffect(
    () => {
      const timer = setTimeout(() => {
        dispatch({ type: 'debouncedCodeChanged', code: code.trim().toUpperCase() });
      }, 500);

      const clearDebounceTimer = () => {
        window.clearTimeout(timer);
      };

      return clearDebounceTimer;
    },
    [code],
  );

  const validatePromo = useCallback(async (promoCode: string) => {
    if (!promoCode) {
      dispatch({ type: 'validationReset' });
      onPromoApplied(null);
      return;
    }

    const requestId = validationRequestRef.current + 1;
    validationRequestRef.current = requestId;
    dispatch({ type: 'validationStarted' });

    try {
      const promo = await SocialEventsAPI.validatePromoCode(eventId, promoCode, promoCode, tierId);
      if (validationRequestRef.current !== requestId) {
        return;
      }

      if (promo.promoCodeIsActive) {
        dispatch({ type: 'validationSucceeded', promo });
        onPromoApplied(promo.promoCodeCode ?? promoCode);
      } else {
        dispatch({ type: 'validationFailed', error: PROMO_COPY.inactive });
        onPromoApplied(null);
      }
    } catch (err) {
      if (validationRequestRef.current !== requestId) {
        return;
      }

      dispatch({ type: 'validationFailed', error: err instanceof Error ? err.message : PROMO_COPY.invalid });
      onPromoApplied(null);
    }
  }, [eventId, tierId, onPromoApplied]);

  useEffect(() => {
    if (debouncedCode) {
      void validatePromo(debouncedCode);
    } else {
      validationRequestRef.current += 1;
      dispatch({ type: 'validationReset' });
      onPromoApplied(null);
    }
  }, [debouncedCode, validatePromo, onPromoApplied]);

  const handleClear = () => {
    validationRequestRef.current += 1;
    dispatch({ type: 'cleared' });
    onPromoApplied(null);
  };

  const handleClearKeyDown = (event: React.KeyboardEvent) => {
    if (event.key !== 'Escape') return;

    event.preventDefault();
    handleClear();
    inputRef.current?.focus();
  };

  const formatDiscount = (promo: PromoCodeDTO): string => {
    if (promo.promoCodeDiscountType === 'percentage') {
      return `${promo.promoCodeDiscountValue / 100}% off`;
    }

    const currency = (promo.promoCodeCurrency ?? 'USD').toUpperCase();
    return `${currency} ${(promo.promoCodeDiscountValue / 100).toFixed(2)} off`;
  };

  const fieldContent = (
    <Box>
      <TextField
        label={PROMO_COPY.label}
        fullWidth
        inputRef={inputRef}
        value={code}
        onChange={(e) => dispatch({ type: 'codeChanged', code: e.target.value.toUpperCase() })}
        placeholder={PROMO_COPY.placeholder}
        margin="normal"
        InputProps={{
          endAdornment: (
            <InputAdornment position="end">
              {validating && <CircularProgress size={PROMO_VALIDATION_SPINNER_SIZE_PX} />}
              {validPromo && <CheckCircleIcon color="success" />}
              {code && !validating && (
                <IconButton
                  aria-label={PROMO_COPY.clear}
                  size="small"
                  onKeyDown={handleClearKeyDown}
                  onClick={() => {
                    handleClear();
                    inputRef.current?.focus();
                  }}
                >
                  <ClearIcon />
                </IconButton>
              )}
            </InputAdornment>
          ),
        }}
        error={Boolean(error)}
        helperText={error ?? (validPromo ? `${PROMO_COPY.applied}: ${formatDiscount(validPromo)}` : undefined)}
      />

      {validPromo && (
        <Alert role="status" severity="success" sx={{ mt: 1 }}>
          <Typography variant="body2">
            <strong>{validPromo.promoCodeCode}</strong> - {formatDiscount(validPromo)}
          </Typography>
          {validPromo.promoCodeValidUntil && (
            <Typography variant="caption" display="block">
              {PROMO_COPY.validUntil}: {new Date(validPromo.promoCodeValidUntil).toLocaleDateString()}
            </Typography>
          )}
          {validPromo.promoCodeMaxRedemptions && (
            <Typography variant="caption" display="block">
              {Math.max(0, validPromo.promoCodeMaxRedemptions - validPromo.promoCodeCurrentRedemptions)}{' '}
              {PROMO_COPY.usesRemaining}
            </Typography>
          )}
        </Alert>
      )}
    </Box>
  );

  return fieldContent;
}
