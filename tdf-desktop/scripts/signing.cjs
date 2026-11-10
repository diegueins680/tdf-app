'use strict';
// The single signing decision shared by channel generation
// (scripts/prepare-channel.cjs) and packaging (electron-builder.config.cjs).
// Both read the same environment, so an installer can never embed a
// channel.json that claims a signed build while its file name carries
// UNSIGNED-TEST, or the reverse.
//
//   macOS:   CSC_LINK or CSC_NAME (Developer ID Application)
//            APPLE_ID + APPLE_APP_SPECIFIC_PASSWORD + APPLE_TEAM_ID (notarization)
//   Windows: WIN_CSC_LINK (Authenticode .pfx), or Azure Trusted Signing:
//            AZURE_TENANT_ID + AZURE_CLIENT_ID + AZURE_CLIENT_SECRET
//            + TDF_WIN_SIGN_ENDPOINT + TDF_WIN_SIGN_ACCOUNT + TDF_WIN_SIGN_PROFILE
//   Any:     TDF_DESKTOP_UNSIGNED=1 forces an unsigned test package.
const UNSIGNED_MARKER = '-UNSIGNED-TEST';

const present = (value) => typeof value === 'string' && value.trim().length > 0;

/**
 * @param {NodeJS.ProcessEnv} env
 * @param {string} platform build host; electron-builder signs its own platform's targets
 */
function resolveSigning(env = process.env, platform = process.platform) {
  const hasMacCertificate = present(env.CSC_LINK) || present(env.CSC_NAME);
  const hasNotarization = present(env.APPLE_ID) && present(env.APPLE_APP_SPECIFIC_PASSWORD) && present(env.APPLE_TEAM_ID);
  const hasWinCertificate = present(env.WIN_CSC_LINK);
  const hasAzureSigning = present(env.AZURE_TENANT_ID) && present(env.AZURE_CLIENT_ID) && present(env.AZURE_CLIENT_SECRET)
    && present(env.TDF_WIN_SIGN_ENDPOINT) && present(env.TDF_WIN_SIGN_ACCOUNT) && present(env.TDF_WIN_SIGN_PROFILE);
  const explicitUnsigned = env.TDF_DESKTOP_UNSIGNED === '1';
  const macSigned = hasMacCertificate && !explicitUnsigned;
  const winSigned = (hasWinCertificate || hasAzureSigning) && !explicitUnsigned;
  const platformSigned = platform === 'darwin' ? macSigned : platform === 'win32' ? winSigned : false;
  let reason = null;
  if (!platformSigned) {
    if (explicitUnsigned) reason = 'explicit';
    else if (platform !== 'darwin' && platform !== 'win32') reason = 'unsupported-platform';
    else reason = 'no-certificate';
  }
  return Object.freeze({
    platform,
    explicitUnsigned,
    hasMacCertificate,
    hasNotarization,
    hasWinCertificate,
    hasAzureSigning,
    macSigned,
    winSigned,
    platformSigned,
    unsigned: !platformSigned,
    reason,
    marker: platformSigned ? '' : UNSIGNED_MARKER,
  });
}

/** Refuses to package a channel whose unsigned flag disagrees with the signing environment. */
function assertChannelMatchesSigning(channel, signing) {
  if (!channel || typeof channel !== 'object') throw new Error('generated/channel.json is missing or malformed; run npm run prepare:channel');
  if (channel.unsigned !== signing.unsigned) {
    throw new Error(`generated/channel.json marks unsigned=${channel.unsigned} but this environment resolves unsigned=${signing.unsigned} (${signing.reason ?? 'signed'}); run npm run prepare:channel in the same environment as electron-builder`);
  }
  return channel;
}

module.exports = { UNSIGNED_MARKER, assertChannelMatchesSigning, resolveSigning };
