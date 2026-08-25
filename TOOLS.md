# TOOLS.md - Local Notes

Skills define _how_ tools work. This file is for _your_ specifics — the stuff that's unique to your setup.

## What Goes Here

Things like:

- Camera names and locations
- SSH hosts and aliases
- Preferred voices for TTS
- Speaker/room names
- Device nicknames
- Anything environment-specific

## Examples

```markdown
### Cameras

- living-room → Main area, 180° wide angle
- front-door → Entrance, motion-triggered

### SSH

- home-server → 192.168.1.100, user: admin

### TTS

- Preferred voice: "Nova" (warm, slightly British)
- Default speaker: Kitchen HomePod
```

## Why Separate?

Skills are shared. Your setup is yours. Keeping them apart means you can update skills without losing your notes, and share skills without leaking your infrastructure.

---

Add whatever helps you do your job. This is your cheat sheet.

## TDF Label Project Notes

### Project Structure
- Main repo: /Users/diegosaa/GitHub/tdf-app
- Mobile app: tdf-mobile/ (React Native / Expo)
- iOS workspace: tdf-mobile/ios/TDFRecords.xcworkspace

### Release Infrastructure
- Detox e2e tests: e2e/firstTest.e2e.js
- Detox config: .detoxrc.js
- Release build derived data: tdf-mobile/ios/build
- Primary simulator: 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B (iPhone 16)
- Old corrupted simulator: 3C3D5759-6E10-480D-B768-2747B9B0D02A (DO NOT USE)

### Active Blockers (as of 2026-05-18)
- EAS_IOS_CREDENTIALS_MISSING: blocks .ipa physical device distribution
- Physical device Google OAuth: waived until 2026-05-21 review

### Key Commands
```bash
# Detox regression test
cd tdf-mobile && npx detox test --configuration ios.sim.release e2e/firstTest.e2e.js

# Release build
cd tdf-mobile/ios && xcodebuild -workspace TDFRecords.xcworkspace -scheme TDFRecords -configuration Release -sdk iphonesimulator -derivedDataPath ../ios/build

# Simulator management
xcrun simctl list devices
xcrun simctl boot <udid>
xcrun simctl shutdown <udid>
xcrun simctl erase <udid>
```

### Organization
- Org path: /Users/diegosaa/.openclaw/orgs/tdf-label
- Release reports: /Users/diegosaa/.openclaw/orgs/tdf-label/reports/
- Evidence/screenshots: /Users/diegosaa/.openclaw/orgs/tdf-label/evidence/
