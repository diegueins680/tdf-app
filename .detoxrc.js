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
        udid: '8DB9DCE0-2F80-49C9-A614-F21DA3876B7B', // regression-verified device — do not use 3C3D5759 due to persistent URL-open dialog
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
