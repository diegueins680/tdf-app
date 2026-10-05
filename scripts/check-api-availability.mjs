import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { verifyApiAvailability, verifyApiResponseStatus } from './lib/compiled-api-surface.mjs';

const read = name => JSON.parse(readFileSync(fileURLToPath(new URL(`../formal/system/${name}.json`, import.meta.url))));
// This always-selected local lane validates actual committed inputs. The backend
// lane independently binds this declaration snapshot to the just-built service.
const result = verifyApiAvailability(read('compiled-api-surface').surface,
  read('traceability').apiOperations, read('api-availability'));
const responseStatus = verifyApiResponseStatus(read('compiled-api-surface').surface,
  read('traceability').apiOperations, read('api-response-status'));
console.log(JSON.stringify({ status: 'declaration-availability-checked', ...result, responseStatus }));
