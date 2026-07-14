#!/usr/bin/env node
import { buildSchemaPreflightSql } from './lib/production-release.mjs';

process.stdout.write(`${buildSchemaPreflightSql()}\n`);
