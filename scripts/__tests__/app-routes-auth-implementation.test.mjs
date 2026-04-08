import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const source = readFileSync(path.resolve(__dirname, '../../src/routes/AppRoutes.tsx'), 'utf8');

test('AppRoutes uses AuthProvider state instead of a demo admin user', () => {
  assert.match(source, /import\s+\{\s*useAuth\s*\}\s+from\s+'\.\.\/auth\/AuthProvider';/);
  assert.match(source, /const\s+\{\s*user,\s*isAuthenticated\s*\}\s*=\s*useAuth\(\);/);
  assert.match(source, /if\s*\(!isAuthenticated\s*\|\|\s*!user\)\s*return null;/);
  assert.match(source, /if\s*\(!rawRoles\s*\|\|\s*rawRoles\.length\s*===\s*0\)\s*return \[\];/);
  assert.doesNotMatch(source, /id:\s*'demo'/);
  assert.doesNotMatch(source, /roles:\s*\[\s*'admin'\s*\]/);
  assert.doesNotMatch(source, /TODO:\s*Reemplazar por hook\/auth real/);
});
