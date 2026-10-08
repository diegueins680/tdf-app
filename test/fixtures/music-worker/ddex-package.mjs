#!/usr/bin/env node
// Deliberate package double: no schema/profile validation is asserted here.
import { writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { spawnSync } from 'node:child_process';
const [, , , output] = process.argv.slice(2);
const manifest = join(dirname(output), 'manifest.json');
writeFileSync(manifest, '{"synthetic":true,"notASchemaValidation":true}');
const result = spawnSync('zip', ['-Xj', output, manifest], { encoding: 'utf8' });
if (result.status !== 0) throw new Error('Synthetic package creation failed');
