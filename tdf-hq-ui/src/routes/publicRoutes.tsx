import { lazy } from 'react';
import { Navigate, Route } from 'react-router-dom';

import PublicBranding from '../components/PublicBranding';

const ArtistOnboardingPage = lazy(() => import('../pages/ArtistOnboardingPage'));
const ArtistPublicPage = lazy(() => import('../pages/ArtistPublicPage'));
const CourseProductionLandingPage = lazy(() => import('../pages/CourseProductionLandingPage'));
const DatafastReturnPage = lazy(() => import('../pages/DatafastReturnPage'));
const DonationPage = lazy(() => import('../pages/DonationPage'));
const FanHubPage = lazy(() => import('../pages/FanHubPage'));
const FeedbackPage = lazy(() => import('../pages/FeedbackPage'));
const GoogleDriveCallbackPage = lazy(() => import('../pages/GoogleDriveCallbackPage'));
const InscripcionPage = lazy(() => import('../pages/inscripcion/InscripcionPage'));
const InstagramCallbackPage = lazy(() => import('../pages/InstagramCallbackPage'));
const InventoryScanPage = lazy(() => import('../pages/InventoryScanPage'));
const LiveSessionPublicPage = lazy(() => import('../pages/LiveSessionPublicPage'));
const LoginPage = lazy(() => import('../pages/LoginPage'));
const MarketplaceOrderTrackingPage = lazy(() => import('../pages/MarketplaceOrderTrackingPage'));
const MarketplacePage = lazy(() => import('../pages/MarketplacePage'));
const MusicMakerPage = lazy(() => import('../pages/MusicMakerPage'));
const NotFoundPage = lazy(() => import('../pages/NotFoundPage'));
const PublicBookingPage = lazy(() => import('../pages/PublicBookingPage'));
const PublicWhatsAppConsentPage = lazy(() => import('../pages/PublicWhatsAppConsentPage'));
const PublicWhatsAppConsentSuccessPage = lazy(() => import('../pages/PublicWhatsAppConsentSuccessPage'));
const RecordsPublicPage = lazy(() => import('../pages/RecordsPublicPage'));
const TrialsPage = lazy(() => import('../pages/TrialsPage'));

export function renderPublicRoutes() {
  return (
    <>
      <Route path="/fans" element={<PublicBranding><FanHubPage /></PublicBranding>} />
      <Route path="/mi-artista" element={<PublicBranding><FanHubPage focusArtist /></PublicBranding>} />
      <Route path="/artista/crear" element={<PublicBranding><ArtistOnboardingPage /></PublicBranding>} />
      <Route path="/artista/:slugOrId" element={<PublicBranding><ArtistPublicPage /></PublicBranding>} />
      <Route path="/marketplace" element={<PublicBranding><MarketplacePage /></PublicBranding>} />
      <Route path="/marketplace/orden/:orderId" element={<PublicBranding><MarketplaceOrderTrackingPage /></PublicBranding>} />
      <Route path="/marketplace/pago-datafast" element={<PublicBranding><DatafastReturnPage /></PublicBranding>} />
      <Route path="/oauth/google-drive/callback" element={<PublicBranding><GoogleDriveCallbackPage /></PublicBranding>} />
      <Route path="/oauth/instagram/callback" element={<PublicBranding><InstagramCallbackPage /></PublicBranding>} />
      <Route path="/curso/produccion-musical" element={<PublicBranding><CourseProductionLandingPage /></PublicBranding>} />
      <Route path="/curso/produccion-musical-abr-2026" element={<PublicBranding><CourseProductionLandingPage /></PublicBranding>} />
      <Route path="/curso/produccion-musical-feb-2026" element={<PublicBranding><CourseProductionLandingPage /></PublicBranding>} />
      <Route path="/curso/produccion-musical-dic-2025" element={<Navigate to="/curso/produccion-musical-abr-2026" replace />} />
      <Route path="/inscripcion/:slug" element={<PublicBranding><InscripcionPage /></PublicBranding>} />
      <Route path="/trials" element={<PublicBranding><TrialsPage /></PublicBranding>} />
      <Route path="/live-sessions/registro" element={<PublicBranding><LiveSessionPublicPage /></PublicBranding>} />
      <Route path="/feedback" element={<PublicBranding><FeedbackPage /></PublicBranding>} />
      <Route path="/whatsapp/consentimiento" element={<PublicBranding><PublicWhatsAppConsentPage /></PublicBranding>} />
      <Route path="/whatsapp/ok" element={<PublicBranding><PublicWhatsAppConsentSuccessPage /></PublicBranding>} />
      <Route path="/records" element={<PublicBranding><RecordsPublicPage /></PublicBranding>} />
      <Route path="/inventario/scan/:token" element={<PublicBranding><InventoryScanPage /></PublicBranding>} />
      <Route path="/donar" element={<PublicBranding><DonationPage /></PublicBranding>} />
      <Route path="/reservar" element={<PublicBranding><PublicBookingPage /></PublicBranding>} />
      <Route path="/login" element={<PublicBranding showHeader showLoginButton={false}><LoginPage /></PublicBranding>} />
      <Route path="/herramientas/creador-musical" element={<PublicBranding><MusicMakerPage /></PublicBranding>} />
      <Route path="*" element={<PublicBranding><NotFoundPage /></PublicBranding>} />
    </>
  );
}
