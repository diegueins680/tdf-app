'use strict';
// electron-builder configuration. Signing and notarization are driven by the
// standard electron-builder environment variables; when they are absent the
// build is explicitly unsigned and its artifacts carry the UNSIGNED-TEST
// marker so a test package is never mistaken for a distributable one.
//
//   macOS:   CSC_LINK + CSC_KEY_PASSWORD (Developer ID Application .p12)
//            APPLE_ID + APPLE_APP_SPECIFIC_PASSWORD + APPLE_TEAM_ID (notarization)
//   Windows: WIN_CSC_LINK + WIN_CSC_KEY_PASSWORD (Authenticode .pfx)
//            or AZURE_TENANT_ID + AZURE_CLIENT_ID + AZURE_CLIENT_SECRET + TDF_WIN_SIGN_ENDPOINT
//            + TDF_WIN_SIGN_ACCOUNT + TDF_WIN_SIGN_PROFILE (Azure Trusted Signing)
const hasMacCertificate = Boolean(process.env.CSC_LINK || process.env.CSC_NAME);
const hasNotarization = Boolean(process.env.APPLE_ID && process.env.APPLE_APP_SPECIFIC_PASSWORD && process.env.APPLE_TEAM_ID);
const hasWinCertificate = Boolean(process.env.WIN_CSC_LINK);
const hasAzureSigning = Boolean(process.env.AZURE_TENANT_ID && process.env.AZURE_CLIENT_ID && process.env.AZURE_CLIENT_SECRET
  && process.env.TDF_WIN_SIGN_ENDPOINT && process.env.TDF_WIN_SIGN_ACCOUNT && process.env.TDF_WIN_SIGN_PROFILE);
const explicitUnsigned = process.env.TDF_DESKTOP_UNSIGNED === '1';
const macSigned = hasMacCertificate && !explicitUnsigned;
const winSigned = (hasWinCertificate || hasAzureSigning) && !explicitUnsigned;
const platformSigned = process.platform === 'darwin' ? macSigned : process.platform === 'win32' ? winSigned : false;
const marker = platformSigned ? '' : '-UNSIGNED-TEST';

/** @type {import('electron-builder').Configuration} */
module.exports = {
  appId: 'net.tdfrecords.desktop',
  productName: 'TDF Records',
  copyright: 'Copyright © 2026 TDF Records',
  artifactName: `TDF-Records-\${version}-\${os}-\${arch}${marker}.\${ext}`,
  directories: { output: 'dist', buildResources: 'build' },
  files: ['src/**/*', 'internal/**/*', 'package.json', '!test/**', '!**/*.test.cjs'],
  extraResources: [{ from: 'generated/channel.json', to: 'channel.json' }],
  icon: 'build/icon.png',
  asar: true,
  npmRebuild: false,
  protocols: [{ name: 'TDF Records', schemes: ['tdf-desktop'] }],
  // electron-updater reads latest*.yml from this generic feed. CI always runs
  // with --publish never; the release workflow uploads the files explicitly.
  publish: [{ provider: 'generic', url: 'https://github.com/diegueins680/tdf-app/releases/download/desktop-latest', channel: 'latest' }],
  mac: {
    category: 'public.app-category.music',
    target: [{ target: 'dmg', arch: ['x64', 'arm64'] }, { target: 'zip', arch: ['x64', 'arm64'] }],
    hardenedRuntime: true,
    gatekeeperAssess: false,
    entitlements: 'build/entitlements.mac.plist',
    entitlementsInherit: 'build/entitlements.mac.plist',
    // `identity: null` disables signing explicitly instead of letting the
    // builder pick any certificate from the keychain.
    ...(macSigned ? {} : { identity: null }),
    notarize: macSigned && hasNotarization,
    extendInfo: {
      NSMicrophoneUsageDescription: 'TDF Records usa el micrófono solo cuando transmites en la radio desde esta app.',
      NSCameraUsageDescription: 'TDF Records usa la cámara solo cuando escaneas códigos de inventario o entradas.',
    },
  },
  dmg: { sign: false, writeUpdateInfo: false },
  win: {
    target: [{ target: 'nsis', arch: ['x64', 'arm64'] }],
    // The publisher shown by Windows comes from the signing certificate itself.
    verifyUpdateCodeSignature: winSigned,
    ...(hasAzureSigning && !explicitUnsigned ? {
      azureSignOptions: {
        endpoint: process.env.TDF_WIN_SIGN_ENDPOINT,
        codeSigningAccountName: process.env.TDF_WIN_SIGN_ACCOUNT,
        certificateProfileName: process.env.TDF_WIN_SIGN_PROFILE,
      },
    } : {}),
  },
  nsis: {
    oneClick: false,
    perMachine: false,
    allowToChangeInstallationDirectory: true,
    deleteAppDataOnUninstall: false,
    shortcutName: 'TDF Records',
    uninstallDisplayName: 'TDF Records',
    artifactName: `TDF-Records-\${version}-win-\${arch}${marker}.\${ext}`,
  },
  linux: {
    // Linux is a verification target for the Electron end-to-end suite, not a
    // distribution platform for this stage.
    target: [{ target: 'AppImage', arch: ['x64'] }],
    category: 'Audio',
    artifactName: `TDF-Records-\${version}-linux-\${arch}${marker}.\${ext}`,
  },
};
