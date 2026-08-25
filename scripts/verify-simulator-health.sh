#!/bin/bash
# Post-restart simulator health verification for tdf-label-release
# Run from tdf-mobile/ directory after host restart clears SIMCTL_DAEMON_DEADLOCK
#
# Usage: cd tdf-mobile && ../scripts/verify-simulator-health.sh

set -e
DEVICE="8DB9DCE0-2F80-49C9-A614-F21DA3876B7B"
APP_PATH="ios/build/Build/Products/Release-iphonesimulator/TDFRecords.app"
BUNDLE_ID="com.tdfrecords.mobile"

echo "=== Simulator Health Check ==="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"

echo ""
echo "1. Testing simctl list devices (<5s)..."
timeout 5 xcrun simctl list devices || { echo "FAIL: simctl list devices hung or timed out"; exit 1; }
echo "PASS: simctl responsive"

echo ""
echo "2. Erasing primary simulator (${DEVICE})..."
xcrun simctl erase "${DEVICE}" || true

echo ""
echo "3. Booting primary simulator..."
xcrun simctl boot "${DEVICE}"

echo ""
echo "4. Testing app install..."
if [ ! -d "${APP_PATH}" ]; then
  echo "FAIL: ${APP_PATH} not found. Rebuild required."
  exit 1
fi
xcrun simctl install "${DEVICE}" "${APP_PATH}"
echo "PASS: app installed"

echo ""
echo "5. Testing app launch (<30s)..."
timeout 30 xcrun simctl launch "${DEVICE}" "${BUNDLE_ID}" || { echo "FAIL: app launch timed out"; exit 1; }
echo "PASS: app launched"

echo ""
echo "=== ALL CHECKS PASSED ==="
echo "Simulator infrastructure is healthy. Proceed to Detox test:"
echo "  npx detox test --configuration ios.sim.release e2e/firstTest.e2e.js"
