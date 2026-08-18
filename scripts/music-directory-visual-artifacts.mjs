import { writeFileSync } from 'node:fs';
import { join } from 'node:path';

export const MUSIC_DIRECTORY_VISUAL_SCOPES = ['all', 'web-managed', 'mobile-managed'];

export function musicDirectoryVisualArtifactPaths(outputDir, captureScope) {
  if (!MUSIC_DIRECTORY_VISUAL_SCOPES.includes(captureScope)) {
    throw new Error(`Unsupported directory visual scope: ${captureScope}`);
  }

  const suffix = captureScope === 'all' ? '' : `-${captureScope}`;
  return {
    accessibility: join(outputDir, `accessibility-results${suffix}.json`),
    browserErrors: join(outputDir, `browser-errors${suffix}.json`),
  };
}

export function writeMusicDirectoryVisualArtifacts({
  outputDir,
  captureScope,
  accessibility,
  browserErrors,
}) {
  const paths = musicDirectoryVisualArtifactPaths(outputDir, captureScope);
  writeFileSync(paths.accessibility, `${JSON.stringify(accessibility, null, 2)}\n`);
  writeFileSync(paths.browserErrors, `${JSON.stringify(browserErrors, null, 2)}\n`);
  return paths;
}
