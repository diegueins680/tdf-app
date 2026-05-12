/** @type {Detox.DetoxConfig} */
module.exports = {
  testRunner: {
    args: {
      $0: 'jest',
      config: 'tdf-mobile/e2e/jest.config.js',
    },
    jest: {
      setupTimeout: 120000,
    },
  },
  apps: {
    'ios.debug': {
      type: 'ios.app',
      binaryPath: 'tdf-mobile/ios/build/Build/Products/Debug-iphonesimulator/TDFRecords.app',
      build: 'cd tdf-mobile/ios && xcodebuild -workspace TDFRecords.xcworkspace -scheme TDFRecords -configuration Debug -sdk iphonesimulator -derivedDataPath build -jobs 1',
    },
  },
  devices: {
    simulator: {
      type: 'ios.simulator',
      device: {
        type: 'iPhone 16',
        udid: '3C3D5759-6E10-480D-B768-2747B9B0D02A', // healthy device — 8DB9DCE0 corrupted (simctl hangs)
      },
    },
  },
  configurations: {
    'ios.sim.debug': {
      device: 'simulator',
      app: 'ios.debug',
    },
  },
};
