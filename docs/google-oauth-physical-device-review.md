# Google OAuth Physical Device Review — 2026-05-21

## Objective
Verify Google OAuth flow works correctly on physical iOS device using the preview `.ipa` build.

## Prerequisites

- [ ] iPhone/iPad with iOS 16+ available for testing
- [ ] Preview build installed: https://expo.dev/accounts/cuco.saa/projects/tdf-mobile/builds/2d8b5544-4304-4a19-a018-42c83930bce9
- [ ] Google account credentials ready for testing
- [ ] Device connected to internet (WiFi/cellular)

## Test Steps

### 1. Fresh Install
- [ ] Delete any existing TDF Records app from device
- [ ] Install preview build via Expo Go or direct install
- [ ] Launch app — verify onboarding screen appears

### 2. Google OAuth Flow
- [ ] Tap "Continuar con Google" button
- [ ] Verify system auth dialog appears (ASWebAuthenticationSession)
- [ ] Tap "Continue" on system dialog
- [ ] Verify Google sign-in page loads
- [ ] Enter valid Google credentials
- [ ] Complete 2FA if prompted
- [ ] Verify redirect back to app succeeds
- [ ] Verify app reaches parties/main screen

### 3. Edge Cases
- [ ] Test with Google account that has 2FA enabled
- [ ] Test canceling OAuth flow mid-way — verify graceful return to login
- [ ] Test with poor network connection
- [ ] Test backgrounding app during OAuth flow

### 4. Regression
- [ ] Verify username/password login still works
- [ ] Verify logout clears session
- [ ] Verify re-login works after logout

## Expected Behavior

- ASWebAuthenticationSession should present native system dialog
- Google sign-in should complete without errors
- Deep link redirect should return to app automatically
- User should land on parties screen after successful auth

## Known Limitations

- Simulator testing confirms OAuth flow starts but cannot complete (system dialog blocks interaction)
- Physical device is required for full end-to-end verification
- Apple may require app review for production OAuth consent screen

## Sign-off

| Role | Name | Date | Status |
|------|------|------|--------|
| Tester | | 2026-05-21 | ⬜ PASS / ⬜ FAIL |
| Reviewer | | 2026-05-21 | ⬜ APPROVED / ⬜ BLOCKS |

## Notes

- Build `2d8b5544-4304-4a19-a018-42c83930bce9` includes A/B experiment infrastructure
- If OAuth fails, check Google Cloud Console for `com.tdfrecords.app` bundle ID configuration
- Verify `GOOGLE_IOS_URL_SCHEME` matches Google Cloud Console settings
