import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { copyFileSync, mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

export const tlaSha256 = 'fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22';
export const translatorArgs = ['pcal.trans', '-nocfg', '-unixEOL', '-lineWidth', '120'];
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const algorithm = /--(?:fair\s+)?algorithm\b/;

export function pinnedToolchain(env = process.env) {
  if (!env.TLA2TOOLS_JAR) throw new Error('TLA2TOOLS_JAR must point to the pinned official JAR.');
  const jarPath = path.resolve(env.TLA2TOOLS_JAR);
  const digest = createHash('sha256').update(readFileSync(jarPath)).digest('hex');
  if (digest !== tlaSha256) throw new Error(`Unexpected TLA+ tools checksum: ${digest}`);
  return { javaBin: env.JAVA_BIN || 'java', jarPath };
}

// This is an integrity check, not a translator-correctness or refinement proof.
// The original is never passed to the translator, which writes .tla/.old files.
export function verifyTranslation(sourcePath, { javaBin, jarPath }) {
  const original = readFileSync(sourcePath);
  const source = original.toString('utf8');
  if (!algorithm.test(source)
      || (source.match(/^\\\* BEGIN TRANSLATION .+$/gm) || []).length !== 1
      || (source.match(/^\\\* END TRANSLATION\s*$/gm) || []).length !== 1) {
    throw new Error(`Missing or ambiguous PlusCal/translation pair: ${path.basename(sourcePath)}`);
  }
  const scratch = mkdtempSync(path.join(tmpdir(), 'tdf-pluscal-check-'));
  try {
    const name = path.basename(sourcePath);
    const copied = path.join(scratch, name);
    copyFileSync(sourcePath, copied);
    const result = spawnSync(javaBin, ['-cp', jarPath, ...translatorArgs, name], {
      cwd: scratch, encoding: 'utf8', input: '', timeout: 30_000, maxBuffer: 1024 * 1024,
    });
    if (result.error || result.status !== 0
        || !result.stdout.includes('Translation completed.')) {
      throw new Error(`PlusCal translation failed: ${name}\n${result.error?.message || ''}\n${result.stdout || ''}${result.stderr || ''}`);
    }
    // Compare ALL bytes, including checksums and whitespace. Never normalize a
    // mismatch away: normalization caused the original reservation warning.
    if (!original.equals(readFileSync(copied))) {
      throw new Error(`PlusCal translation drift: ${name}. Regenerate with the pinned translator and documented options; review the diff.`);
    }
    // Fail if a concurrent edit changed the source during verification.
    if (!original.equals(readFileSync(sourcePath))) {
      throw new Error(`PlusCal source changed during verification: ${name}`);
    }
    return name;
  } finally {
    // Only our freshly-created directory, never a caller-supplied cleanup target.
    rmSync(scratch, { recursive: true, force: true });
  }
}

export function verifyDirectory(modelDir, toolchain) {
  // ReservationRace cannot silently disappear or be converted to handwritten TLA+.
  const models = new Set(['ReservationRace.tla']);
  for (const name of readdirSync(modelDir, { recursive: true }).sort()) {
    if (!name.endsWith('.tla')) continue;
    const source = readFileSync(path.join(modelDir, name), 'utf8');
    if (algorithm.test(source) || source.includes('BEGIN TRANSLATION')) models.add(name);
  }
  return [...models].sort().map(name => verifyTranslation(path.join(modelDir, name), toolchain));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    if (process.argv.length !== 2) throw new Error('This check accepts no command-line arguments.');
    const models = verifyDirectory(path.join(root, 'formal/event-operations'), pinnedToolchain());
    for (const name of models) console.log(`PlusCal regeneration matches exactly: ${name}`);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
