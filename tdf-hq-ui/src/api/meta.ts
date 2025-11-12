import { get } from './client';
import type { VersionInfo, HealthStatus } from './types';

export const Meta = {
  version: () => get<VersionInfo>('/version'),
  health: () => get<HealthStatus>('/health'),
};
