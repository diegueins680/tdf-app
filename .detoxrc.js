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
    'ios.release': {
      type: 'ios.app',
      binaryPath: 'tdf-mobile/ios/build/Build/Products/Release-iphonesimulator/TDFRecords.app',
      build: 'cd tdf-mobile/ios && xcodebuild -workspace TDFRecords.xcworkspace -scheme TDFRecords -configuration Release -sdk iphonesimulator -derivedDataPath build -jobs 1',
    },
  },
  devices: {
    simulator: {
      type: 'ios.simulator',
      device: {
        type: 'iPhone 16',
        id: '8DB9DCE0-2F80-49C9-A614-F21DA3876B7B', // fresh simulator (2026-05-18); old 3C3D5759 corrupted/hung
      },
    },
  },
  configurations: {
    'ios.sim.debug': {
      device: 'simulator',
      app: 'ios.debug',
    },
    'ios.sim.release': {
      device: 'simulator',
      app: 'ios.release',
    },
  },
};
