import { readFile, writeFile } from 'node:fs/promises';
const source = await readFile(new URL('../tdf-hq-ui/src/components/notificationTarget.ts', import.meta.url), 'utf8');
const destination = new URL('../tdf-mobile/src/navigation/notificationTarget.ts', import.meta.url);
const output = '// Generated from tdf-hq-ui/src/components/notificationTarget.ts.\n' + source.replace("'../api/types'", "'../api/notifications'");
if (process.argv.includes('--check')) {
  if (await readFile(destination, 'utf8') !== output) throw new Error('Mobile notification destinations drifted; run node scripts/generate-notification-navigation.mjs');
} else {
  await writeFile(destination, output);
}
