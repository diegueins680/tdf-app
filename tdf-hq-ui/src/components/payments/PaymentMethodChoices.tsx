import { Button, CircularProgress, Stack, Typography, type ButtonProps } from '@mui/material';
import type { ReactNode } from 'react';

/** Presentation only: the caller keeps method availability and payment dispatch. */
export function PaymentMethodButton({ children, busy = false, ...props }: Omit<ButtonProps, 'children' | 'variant'> & {
  children: string;
  busy?: boolean;
}) {
  const [method, processor] = children.split(' · ');
  return (
    <Button {...props} variant="outlined" aria-label={children}
      sx={[{ justifyContent: 'flex-start', minHeight: 56, textTransform: 'none' },
        ...(Array.isArray(props.sx) ? props.sx : props.sx ? [props.sx] : [])]}>
      {busy && <CircularProgress size={20} color="inherit" sx={{ mr: 1 }} />}
      <Stack component="span" alignItems="flex-start">
        <Typography component="span" fontWeight={700}>{method}</Typography>
        {processor && <Typography component="span" variant="caption" color="text.secondary"> · {processor}</Typography>}
      </Stack>
    </Button>
  );
}

export default function PaymentMethodChoices({ methods, children, english = false }: {
  methods: ReactNode;
  children?: ReactNode;
  english?: boolean;
}) {
  return (
    <Stack spacing={1.5} component="section" aria-label={english ? 'Payment methods' : 'Métodos de pago'}>
      <Typography variant="subtitle2" fontWeight={800}>{english ? 'Choose how to pay' : 'Elige cómo pagar'}</Typography>
      <Stack role="group" aria-label={english ? 'Available payment methods' : 'Métodos de pago disponibles'}
        direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
        {methods}
      </Stack>
      {children}
    </Stack>
  );
}
