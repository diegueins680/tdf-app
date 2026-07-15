#!/usr/bin/env node
import { buildSchemaVerificationSql } from './lib/production-release.mjs';

process.stdout.write(`${buildSchemaVerificationSql()}\n`);
