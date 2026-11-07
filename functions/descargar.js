import { redirectToAppLanding } from './_shared/app-smart-link.mjs';

export function onRequest(context) {
  return redirectToAppLanding(context);
}
