/** @type {import('jest').Config} */
module.exports = {
  preset: 'ts-jest/presets/default-esm',
  testEnvironment: 'jsdom',
  testEnvironmentOptions: {
    customExportConditions: ['node', 'node-addons'],
  },
  extensionsToTreatAsEsm: ['.ts', '.tsx'],
  transform: {
    '^.+\\.(ts|tsx)$': ['ts-jest', { useESM: true }],
  },
  modulePaths: ['<rootDir>/node_modules'],
  moduleNameMapper: {
    '^@mui/icons-material/(.*)$': '<rootDir>/src/__mocks__/muiIconMock.tsx',
    '\\.(css|less|scss|sass)$': 'identity-obj-proxy',
  },
  setupFilesAfterEnv: [],
  reporters: [
    'default',
    ['@testomatio/reporter/jest', { apiKey: process.env.TESTOMATIO }],
  ],
};
