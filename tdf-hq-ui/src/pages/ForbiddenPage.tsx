import { useEffect, useRef } from 'react';
import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { Link as RouterLink } from 'react-router-dom';
import {
  accessRequestPath,
  featureLabel,
  type FeatureAccessDecision,
} from '../features/featureRegistry';
import appI18n from '../i18n';

interface ForbiddenPageProps {
  decision: FeatureAccessDecision;
}

export default function ForbiddenPage({ decision }: ForbiddenPageProps) {
  const headingRef = useRef<HTMLHeadingElement | null>(null);
  const canNameFeature = decision.feature.safeLockedDisclosure;
  const canRequest = decision.state === 'locked' && decision.feature.accessRequestEligible;
  const locale = appI18n.resolvedLanguage ?? appI18n.language ?? 'es';
  const missingModule = decision.missingModules[0];
  const missingRole = decision.missingRoles[0];
  const missingExplanation = missingModule
    ? `Te falta acceso al módulo ${missingModule}.`
    : missingRole === 'strict-admin'
      ? 'Esta acción requiere una cuenta administradora de emergencia sin roles operativos adicionales.'
      : missingRole
        ? `Tu rol actual no incluye la categoría ${missingRole}.`
        : 'Tu acceso actual no permite abrir esta sección.';

  useEffect(() => {
    headingRef.current?.focus();
  }, []);

  return (
    <Box sx={{ minHeight: '55vh', display: 'grid', placeItems: 'center', px: 2 }}>
      <Stack spacing={2.5} alignItems="center" textAlign="center" maxWidth={560}>
        <LockOutlinedIcon aria-hidden="true" sx={{ fontSize: 56, color: 'warning.main' }} />
        <Typography variant="overline" color="text.secondary">ERROR 403</Typography>
        <Typography ref={headingRef} tabIndex={-1} variant="h4" component="h1" fontWeight={700}>
          No tienes permiso para abrir esta sección
        </Typography>
        {canNameFeature ? (
          <Typography variant="body1" color="text.secondary">
            {featureLabel(decision.feature, locale)} es relevante para tu flujo, pero no forma parte de tu acceso efectivo.
          </Typography>
        ) : null}
        <Alert severity="warning" role="status" sx={{ width: '100%', textAlign: 'left' }}>
          {missingExplanation} No se mostró ningún dato protegido.
        </Alert>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} sx={{ width: { xs: '100%', sm: 'auto' } }}>
          {canRequest ? (
            <Button
              variant="contained"
              component={RouterLink}
              to={accessRequestPath(decision.feature, 'view')}
              sx={{ minHeight: 44 }}
            >
              Solicitar acceso
            </Button>
          ) : null}
          <Button variant="outlined" component={RouterLink} to="/inicio" sx={{ minHeight: 44 }}>
            Volver al inicio
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}
