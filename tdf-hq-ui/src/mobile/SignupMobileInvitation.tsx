import { Box, Button } from '@mui/material';
import { useLocation, useNavigate } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { useSession } from '../session/SessionContext';
import MobilePromo from './MobilePromo';
export default function SignupMobileInvitation() {
  const location = useLocation(); const navigate = useNavigate();
  const { session } = useSession(); const { t } = useTranslation();
  const state = location.state as { mobileInvitation?: boolean } | null;
  if (!session || !state?.mobileInvitation) return null;
  return <Box sx={{ maxWidth: 760, mx: 'auto', p: 2 }}>
    <MobilePromo surface="signup_complete" />
    <Button sx={{ minHeight: 44 }} onClick={() => navigate(`${location.pathname}${location.search}${location.hash}`, { replace: true, state: { ...state, mobileInvitation: false } })}>{t('app.dismiss')}</Button>
  </Box>;
}
