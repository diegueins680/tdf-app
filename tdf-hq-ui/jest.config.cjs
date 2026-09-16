const path = require('path');

/** @type {import('jest').Config} */
module.exports = {
  preset: 'ts-jest/presets/default-esm',
  testEnvironment: 'jsdom',
  testEnvironmentOptions: {
    customExportConditions: ['node', 'node-addons'],
  },
  extensionsToTreatAsEsm: ['.ts', '.tsx'],
  transform: {
    '^.+\\.(ts|tsx)$': ['ts-jest', { tsconfig: path.join(__dirname, 'tsconfig.jest.json'), useESM: true }],
  },
  modulePaths: ['<rootDir>/node_modules', '<rootDir>/../node_modules'],
  setupFiles: ['<rootDir>/jest.polyfills.cjs'],
  moduleNameMapper: {
    '^@mui/icons-material/(.*)$': '<rootDir>/src/__mocks__/muiIconMock.tsx',
    '^react$': require.resolve('react'),
    '^react-dom$': require.resolve('react-dom'),
    '^react/jsx-runtime$': require.resolve('react/jsx-runtime'),
    '^react/jsx-dev-runtime$': require.resolve('react/jsx-dev-runtime'),
    '^webtorrent/dist/webtorrent\\.min\\.js(?:\\?url)?$': '<rootDir>/src/__mocks__/assetUrlMock.ts',
    '\\.(svg|png|jpe?g|gif|webp)$': '<rootDir>/src/__mocks__/assetUrlMock.ts',
    '\\.(css|less|scss|sass)$': 'identity-obj-proxy',
  },
  setupFilesAfterEnv: [],
  reporters: [
    'default',
    [require.resolve('@testomatio/reporter/jest'), { apiKey: process.env.TESTOMATIO }],
  ],
};
