import path from 'node:path';

const FULL_SHA = /^[0-9a-f]{40}$/i;
const SAFE_NAME = /^[a-zA-Z0-9][a-zA-Z0-9_-]*$/;
const MIGRATION_PATH = /^tdf-hq\/sql\/[a-zA-Z0-9][a-zA-Z0-9._-]*\.sql$/;

export function normalizeFullSha(value) {
  const sha = String(value ?? '').trim().toLowerCase();
  if (!FULL_SHA.test(sha)) {
    throw new Error('Release SHA must be a full 40-character hexadecimal commit.');
  }
  return sha;
}

export function validateSafeName(value, label = 'name') {
  const normalized = String(value ?? '').trim();
  if (!SAFE_NAME.test(normalized)) {
    throw new Error(`${label} contains unsupported characters.`);
  }
  return normalized;
}

export function validateMigrationRelativePath(value) {
  const normalized = String(value ?? '').replaceAll('\\', '/');
  if (!MIGRATION_PATH.test(normalized) || path.posix.normalize(normalized) !== normalized) {
    throw new Error(`Migration path must stay inside tdf-hq/sql: ${value}`);
  }
  return normalized;
}

export async function expandMigrationIncludes(migration, readFile, ancestors = new Set()) {
  const relativePath = validateMigrationRelativePath(migration?.path);
  const content = String(migration?.content ?? '');
  if (!content.trim()) throw new Error(`Migration ${relativePath} has no SQL content.`);
  if (typeof readFile !== 'function') throw new Error('Migration include expansion requires a reader.');
  if (ancestors.has(relativePath)) {
    throw new Error(`Recursive migration include detected at ${relativePath}.`);
  }

  const nextAncestors = new Set(ancestors).add(relativePath);
  const output = [];
  for (const line of content.split(/\r?\n/u)) {
    const include = line.match(/^\s*\\ir\s+([a-zA-Z0-9][a-zA-Z0-9._-]*\.sql)\s*$/u);
    if (!include) {
      if (/^\s*\\i(?:r)?\s+/u.test(line)) {
        throw new Error(`Unsupported migration include syntax in ${relativePath}: ${line.trim()}`);
      }
      output.push(line);
      continue;
    }

    const includedPath = validateMigrationRelativePath(
      path.posix.join(path.posix.dirname(relativePath), include[1]),
    );
    const includedContent = await readFile(includedPath);
    const expanded = await expandMigrationIncludes(
      { path: includedPath, content: includedContent },
      readFile,
      nextAncestors,
    );
    output.push(`-- begin inlined migration include: ${includedPath}`);
    output.push(expanded);
    output.push(`-- end inlined migration include: ${includedPath}`);
  }
  return output.join('\n');
}

export function parseSecurityEmergencyReadinessOutput(output) {
  const records = String(output ?? '')
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.startsWith('{'))
    .map((line) => {
      try {
        return JSON.parse(line);
      } catch {
        return undefined;
      }
    })
    .filter(Boolean);
  const report = [...records]
    .reverse()
    .find(({ kind }) => kind === 'security-emergency-readiness');
  if (!report) throw new Error('Security emergency readiness preflight returned no report.');
  if (!['legacy', 'canonical', 'missing'].includes(report.schemaMode)) {
    throw new Error('Security emergency readiness preflight returned an unknown schema mode.');
  }
  if (report.transactionReadOnly !== 'on') {
    throw new Error('Security emergency readiness preflight was not read-only.');
  }
  if (report.requiredIndependentPaths !== 2
      || typeof report.preMigrationReady !== 'boolean'
      || typeof report.databaseReady !== 'boolean') {
    throw new Error('Security emergency readiness preflight returned an invalid gate payload.');
  }
  for (const field of [
    'activeEmergencyAssignments',
    'distinctAssignedParties',
    'authenticatableParties',
    'databaseCoherentPaths',
  ]) {
    if (report[field] !== undefined
        && report[field] !== null
        && (!Number.isInteger(report[field]) || report[field] < 0)) {
      throw new Error(`Security emergency readiness preflight returned an invalid ${field}.`);
    }
  }
  return report;
}

export function securityEmergencyReadinessBlocker(report, options = {}) {
  if (options.requireCanonical === true) {
    if (report.schemaMode !== 'canonical') {
      return `post-migration emergency readiness remained in ${report.schemaMode} schema mode`;
    }
    if (!report.databaseReady) {
      return `canonical emergency recovery has ${report.databaseCoherentPaths ?? 0} coherent paths; 2 are required`;
    }
    return undefined;
  }
  if (!report.preMigrationReady) {
    return `emergency recovery has ${report.authenticatableParties ?? 0} independently authenticatable parties; 2 are required before migration`;
  }
  return undefined;
}

function stripTomlComment(line) {
  let quote;
  let escaped = false;
  for (let index = 0; index < line.length; index += 1) {
    const character = line[index];
    if (escaped) {
      escaped = false;
      continue;
    }
    if (quote === '"' && character === '\\') {
      escaped = true;
      continue;
    }
    if (character === '"' || character === "'") {
      quote = quote === character ? undefined : (quote ?? character);
      continue;
    }
    if (character === '#' && !quote) return line.slice(0, index);
  }
  return line;
}

function parseTomlScalar(rawValue) {
  const value = rawValue.trim();
  if (value.startsWith('"') && value.endsWith('"')) {
    try {
      return JSON.parse(value);
    } catch {
      throw new Error(`Unsupported TOML string: ${value}`);
    }
  }
  if (value.startsWith("'") && value.endsWith("'")) return value.slice(1, -1);
  if (/^-?[0-9]+$/.test(value)) return Number(value);
  if (value === 'true' || value === 'false') return value === 'true';
  return value;
}

function parseTomlTables(toml) {
  const tables = new Map([['', [new Map()]]]);
  let current = tables.get('')[0];

  for (const rawLine of String(toml).split(/\r?\n/)) {
    const line = stripTomlComment(rawLine).trim();
    if (!line) continue;
    const arrayHeader = line.match(/^\[\[([A-Za-z0-9_.-]+)\]\]$/);
    const tableHeader = line.match(/^\[([A-Za-z0-9_.-]+)\]$/);
    if (arrayHeader || tableHeader) {
      const name = (arrayHeader ?? tableHeader)[1];
      if (!tables.has(name)) tables.set(name, []);
      if (!arrayHeader && tables.get(name).length > 0) {
        throw new Error(`Duplicate TOML table [${name}].`);
      }
      current = new Map();
      tables.get(name).push(current);
      continue;
    }
    const assignment = line.match(/^([A-Za-z0-9_-]+)\s*=\s*(.+)$/);
    if (!assignment) continue;
    const [, key, rawValue] = assignment;
    if (current.has(key)) throw new Error(`Duplicate TOML key: ${key}.`);
    current.set(key, parseTomlScalar(rawValue));
  }
  return tables;
}

function exactlyOneTable(tables, name) {
  const matches = tables.get(name) ?? [];
  if (matches.length !== 1) throw new Error(`fly.toml must define exactly one [${name}] table.`);
  return matches[0];
}

export function validateFlyConfig(toml) {
  const tables = parseTomlTables(toml);
  const root = exactlyOneTable(tables, '');
  const env = exactlyOneTable(tables, 'env');
  const deploy = exactlyOneTable(tables, 'deploy');
  const services = tables.get('services') ?? [];
  const healthChecks = tables.get('services.http_checks') ?? [];
  const runMigrations = String(env.get('RUN_MIGRATIONS') ?? '').trim().toLowerCase();
  const eventDiscovery = String(env.get('EVENT_DISCOVERY_ENABLED') ?? '').trim().toLowerCase();
  const hasHealthCheck = services.length === 1 && healthChecks.some((check) => (
    String(check.get('path') ?? '').trim() === '/health'
      && String(check.get('protocol') ?? '').trim().toLowerCase() === 'http'
      && String(check.get('method') ?? '').trim().toLowerCase() === 'get'
  ));
  const rollingStrategy = String(deploy.get('strategy') ?? '').trim().toLowerCase() === 'rolling';
  const maxUnavailableOne = deploy.get('max_unavailable') === 1;

  if (root.get('app') !== 'tdf-hq') {
    throw new Error('fly.toml must target the production tdf-hq app.');
  }

  if (runMigrations !== 'false') {
    throw new Error('fly.toml must set RUN_MIGRATIONS="false" for production.');
  }
  if (eventDiscovery !== 'false') {
    throw new Error('fly.toml must stage EVENT_DISCOVERY_ENABLED="false" during rollout.');
  }
  if (!hasHealthCheck) {
    throw new Error('fly.toml must define an HTTP /health readiness check.');
  }
  if (!rollingStrategy || !maxUnavailableOne) {
    throw new Error('fly.toml must use rolling deploys with max_unavailable = 1.');
  }

  return {
    runMigrations: false,
    eventDiscoveryEnabled: false,
    healthCheckPath: '/health',
    strategy: 'rolling',
    maxUnavailable: 1,
  };
}

function sqlLiteral(value) {
  return `'${String(value).replaceAll("'", "''")}'`;
}

function migrationSource(migration) {
  if (typeof migration === 'string') {
    return `\\ir ${validateMigrationRelativePath(migration)}`;
  }
  if (!migration || typeof migration !== 'object') {
    throw new Error('Migration entries must be paths or migration objects.');
  }
  if (typeof migration.content !== 'string' || !migration.content.trim()) {
    throw new Error(`Migration ${migration.path ?? migration.id ?? '<unknown>'} has no SQL content.`);
  }
  if (/^\s*\\i(?:r)?\s+/mu.test(migration.content)) {
    throw new Error(`Migration ${migration.path ?? migration.id ?? '<unknown>'} contains an unexpanded include.`);
  }
  return migration.content.trim();
}

export function buildMigrationBatchSql(migrations, options = {}) {
  const sourceCommit = options.sourceCommit
    ? normalizeFullSha(options.sourceCommit)
    : '0000000000000000000000000000000000000000';
  const entries = migrations.map((migration) => {
    if (typeof migration === 'string') {
      const relativePath = validateMigrationRelativePath(migration);
      return {
        id: path.posix.basename(relativePath, '.sql'),
        path: relativePath,
        checksum: '0'.repeat(64),
        source: migrationSource(migration),
      };
    }
    const relativePath = validateMigrationRelativePath(migration.path);
    const checksum = String(migration.checksum ?? '').toLowerCase();
    if (!/^[0-9a-f]{64}$/.test(checksum)) {
      throw new Error(`Migration ${relativePath} requires a SHA-256 checksum.`);
    }
    return {
      id: String(migration.id ?? path.posix.basename(relativePath, '.sql')),
      path: relativePath,
      checksum,
      source: migrationSource(migration),
    };
  });

  const body = entries.map((entry, index) => {
    const applyVariable = `apply_migration_${index + 1}`;
    return [
      '\\unset run_code',
      '\\unset safety_threshold',
      '\\unset batch_size',
      '\\unset backfill_run_id',
      '\\unset records_backfill_run_id',
      `\\echo Checking ${entry.id}`,
      'DO $checksum$',
      'BEGIN',
      '  IF EXISTS (',
      '    SELECT 1 FROM public.tdf_schema_migration',
      `    WHERE migration_id = ${sqlLiteral(entry.id)}`,
      `      AND checksum <> ${sqlLiteral(entry.checksum)}`,
      '  ) THEN',
      `    RAISE EXCEPTION 'Checksum mismatch for migration ${entry.id}';`,
      '  END IF;',
      'END',
      '$checksum$;',
      `SELECT NOT EXISTS (SELECT 1 FROM public.tdf_schema_migration WHERE migration_id = ${sqlLiteral(entry.id)}) AS ${applyVariable} \\gset`,
      `\\if :${applyVariable}`,
      `\\echo Applying migration ${entry.id}`,
      entry.source,
      'INSERT INTO public.tdf_schema_migration (migration_id, checksum, source_commit)',
      `VALUES (${sqlLiteral(entry.id)}, ${sqlLiteral(entry.checksum)}, ${sqlLiteral(sourceCommit)});`,
      '\\else',
      `\\echo Already applied ${entry.id}`,
      '\\endif',
    ].join('\n');
  }).join('\n\n');

  return [
    '\\set ON_ERROR_STOP on',
    `\\set candidate_revision ${sourceCommit}`,
    "SELECT pg_try_advisory_lock(hashtextextended('tdf-production-schema-migrations', 0)) AS migration_lock_acquired \\gset",
    '\\if :migration_lock_acquired',
    'CREATE TABLE IF NOT EXISTS public.tdf_schema_migration (',
    '  migration_id TEXT PRIMARY KEY,',
    '  checksum TEXT NOT NULL CHECK (checksum ~ \'^[0-9a-f]{64}$\'),',
    '  source_commit TEXT NOT NULL CHECK (source_commit ~ \'^[0-9a-f]{40}$\'),',
    '  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()',
    ');',
    body,
    buildSchemaVerificationSql({ includePsqlHeader: false }),
    "SELECT pg_advisory_unlock(hashtextextended('tdf-production-schema-migrations', 0));",
    '\\else',
    '\\echo Another production schema migration is active; refusing to wait',
    '\\quit 3',
    '\\endif',
    '',
  ].join('\n\n');
}

export function buildSchemaPreflightSql() {
  return `
\\set ON_ERROR_STOP on
BEGIN READ ONLY;
DO $preflight$
BEGIN
  IF pg_is_in_recovery() THEN
    RAISE EXCEPTION 'Production migration target is not the PostgreSQL primary';
  END IF;
  IF current_setting('default_transaction_read_only') <> 'off' THEN
    RAISE EXCEPTION 'Production migration target is read-only';
  END IF;
  IF to_regclass('public.party') IS NULL
     OR to_regclass('public.campaign') IS NULL
     OR to_regclass('public.event_ticket_order') IS NULL
     OR to_regclass('public.venue') IS NULL
     OR to_regclass('public.social_artist_profile') IS NULL
     OR to_regclass('public.artist_profile') IS NULL
     OR to_regclass('public.social_event') IS NULL THEN
    RAISE EXCEPTION 'Required ticketing/event/social base tables are missing';
  END IF;
  IF to_regclass('public.notification') IS NULL OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'notification'
  ) <> 9 OR to_regclass('public.idx_notification_recipient') IS NULL THEN
    RAISE EXCEPTION 'The repaired notification schema is missing or incomplete';
  END IF;
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'event_ticket_order'
      AND column_name = 'stripe_payment_intent_id'
  ) THEN
    IF EXISTS (
      SELECT 1 FROM public.event_ticket_order
      WHERE stripe_payment_intent_id IS NOT NULL
      GROUP BY stripe_payment_intent_id HAVING COUNT(*) > 1
    ) THEN
      RAISE EXCEPTION 'Duplicate ticket PaymentIntent ids require reconciliation';
    END IF;
  END IF;
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'event_ticket_order'
      AND column_name = 'checkout_idempotency_key'
  ) THEN
    IF EXISTS (
      SELECT 1 FROM public.event_ticket_order
      WHERE checkout_idempotency_key IS NOT NULL AND buyer_party_id IS NULL
    ) THEN
      RAISE EXCEPTION 'Keyed ticket orders without buyers require reconciliation';
    END IF;
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'promo_code', 'promo_code_redemption', 'ticket_refund_request',
        'ticket_transfer', 'event_waitlist', 'stripe_payment_intent',
        'stripe_webhook_event', 'ticket_qr_code'
      )
  ) NOT IN (0, 8) THEN
    RAISE EXCEPTION 'Ticketing runtime tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'external_venue_ref', 'external_artist_ref',
        'external_event_ref', 'external_event_discovery_run'
      )
  ) NOT IN (0, 4) THEN
    RAISE EXCEPTION 'Event discovery tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'event_city', 'event_city_subscription', 'event_discovery_source'
      )
  ) NOT IN (0, 3) THEN
    RAISE EXCEPTION 'Event city subscription tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'social_sync_account', 'social_sync_post', 'social_sync_run'
      )
  ) NOT IN (0, 3) THEN
    RAISE EXCEPTION 'Social-sync runtime tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'artist_profile_enrichment', 'artist_inventory_reference',
        'artist_research_source', 'artist_enrichment_suggestion',
        'artist_field_change', 'artist_enrichment_run',
        'artist_identity_candidate', 'artist_media_asset'
      )
  ) NOT IN (0, 8) THEN
    RAISE EXCEPTION 'Artist enrichment tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'campaign_automation', 'campaign_automation_step',
        'campaign_enrollment', 'campaign_delivery'
      )
  ) NOT IN (0, 4) THEN
    RAISE EXCEPTION 'Campaign automation tables are partially present';
  END IF;
  IF (
    SELECT COUNT(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'feature_access_requests', 'feature_access_request_history',
        'feature_navigation_preferences'
      )
  ) NOT IN (0, 3) THEN
    RAISE EXCEPTION 'Feature discovery tables are partially present';
  END IF;
END
$preflight$;
ROLLBACK;
`.trim();
}

export function buildSchemaVerificationSql(options = {}) {
  const header = options.includePsqlHeader === false ? '' : '\\set ON_ERROR_STOP on\n';
  return `${header}DO $verify$
DECLARE
  campaign_table TEXT;
  catalog_table TEXT;
  cutover_code TEXT;
  discovery_table TEXT;
  ddex_table TEXT;
  feature_table TEXT;
  social_table TEXT;
  ticketing_table TEXT;
  enrichment_table TEXT;
BEGIN
  FOREACH catalog_table IN ARRAY ARRAY[
    'workflow_definition',
    'workflow_state',
    'workflow_transition',
    'catalog_definition',
    'catalog_revision',
    'catalog_audit_event',
    'catalog_backfill_run',
    'catalog_migration_mapping',
    'catalog_slug_alias',
    'catalog_scoped_default',
    'security_module',
    'security_action',
    'security_permission',
    'security_role',
    'role_permission',
    'party_security_role',
    'country_reference',
    'locale_reference',
    'currency_reference',
    'language_reference',
    'genre',
    'instrument',
    'service_offering',
    'event_type',
    'content_type',
    'authored_content',
    'record_release',
    'recording',
    'recording_session',
    'editorial_collection',
    'ddex_standard_version',
    'ddex_message_type',
    'ddex_standard_support'
  ] LOOP
    IF to_regclass('public.' || catalog_table) IS NULL THEN
      RAISE EXCEPTION 'Canonical catalog relation public.% is missing', catalog_table;
    END IF;
  END LOOP;

  IF (SELECT count(*) FROM workflow_definition WHERE active) < 16
     OR (SELECT count(*) FROM workflow_state WHERE active) < 97
     OR (SELECT count(*) FROM workflow_transition WHERE active) < 295
     OR (SELECT count(*) FROM catalog_definition WHERE active) < 47
     OR (SELECT count(*) FROM country_reference WHERE active) < 249
     OR (SELECT count(*) FROM security_module WHERE active) < 8
     OR (SELECT count(*) FROM security_action WHERE active) < 16
     OR (SELECT count(*) FROM security_permission WHERE active) < 30
     OR (SELECT count(*) FROM security_role WHERE active) < 31
     OR (SELECT count(*) FROM role_permission WHERE active) < 116 THEN
    RAISE EXCEPTION 'Canonical catalog foundation seed is incomplete';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('party', 'country_id'),
        ('artist_profile', 'country_id'),
        ('user_locale_preferences', 'locale_id'),
        ('user_locale_preferences', 'currency_id'),
        ('user_locale_preferences', 'country_id'),
        ('service_order', 'service_offering_id'),
        ('booking', 'service_offering_id'),
        ('booking', 'booking_type_id'),
        ('booking', 'workflow_state_id'),
        ('pipeline_card', 'service_offering_id'),
        ('pipeline_card', 'workflow_state_id'),
        ('feedback', 'category_id'),
        ('feedback', 'severity_id'),
        ('input_row', 'instrument_id'),
        ('social_event', 'event_type_id'),
        ('social_event', 'workflow_state_id'),
        ('social_event', 'currency_id'),
        ('ddex_document', 'standard_version_id'),
        ('ddex_document', 'message_type_id'),
        ('ddex_document', 'workflow_state_id')
    ) AS expected(table_name, column_name)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL OR actual.data_type <> 'uuid'
  ) THEN
    RAISE EXCEPTION 'A canonical catalog consumer UUID reference is missing or invalid';
  END IF;

  IF EXISTS (
    SELECT 1 FROM catalog_backfill_run
    WHERE NOT dry_run AND status <> 'completed'
  ) THEN
    RAISE EXCEPTION 'A canonical catalog cutover did not complete';
  END IF;

  FOREACH cutover_code IN ARRAY ARRAY[
    'catalog-cutover-2026-08-07',
    'records-cms-cutover-2026-08-07',
    'instrument-input-cutover-2026-08-11',
    'feedback-catalog-cutover-2026-08-11',
    'pipeline-workflow-cutover-2026-08-11',
    'social-event-type-cutover-2026-08-11',
    'social-event-workflow-cutover-2026-08-11',
    'event-moment-reaction-cutover-2026-08-12',
    'content-reaction-cutover-2026-08-12',
    'creator-badge-cutover-2026-08-12',
    'ddex-reference-cutover-2026-08-12',
    'ddex-validation-reference-cutover-2026-08-12',
    'ddex-operational-cutover-2026-08-12'
  ] LOOP
    IF NOT EXISTS (
      SELECT 1 FROM catalog_backfill_run
      WHERE run_code = cutover_code AND NOT dry_run AND status = 'completed'
    ) THEN
      RAISE EXCEPTION 'Canonical catalog cutover % has no completed run', cutover_code;
    END IF;
  END LOOP;

  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'event_ticket_order'
      AND column_name = 'checkout_idempotency_key'
      AND data_type = 'character varying'
  ) THEN
    RAISE EXCEPTION 'event_ticket_order.checkout_idempotency_key is missing or invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.event_ticket_order'::regclass
      AND conname = 'unique_event_ticket_checkout'
      AND contype = 'u'
      AND convalidated
      AND pg_get_constraintdef(oid) ILIKE '%buyer_party_id, checkout_idempotency_key%'
  ) THEN
    RAISE EXCEPTION 'unique_event_ticket_checkout is missing or invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1
    FROM pg_indexes
    WHERE schemaname = 'public'
      AND tablename = 'event_ticket_order'
      AND indexname = 'uq_event_ticket_order_stripe_payment_intent'
      AND indexdef ILIKE '%UNIQUE%'
      AND indexdef ILIKE '%WHERE (stripe_payment_intent_id IS NOT NULL)%'
  ) THEN
    RAISE EXCEPTION 'ticket PaymentIntent uniqueness index is missing or invalid';
  END IF;

  FOREACH ticketing_table IN ARRAY ARRAY[
    'promo_code',
    'promo_code_redemption',
    'ticket_refund_request',
    'ticket_transfer',
    'event_waitlist',
    'stripe_payment_intent',
    'stripe_webhook_event',
    'ticket_qr_code'
  ] LOOP
    IF to_regclass('public.' || ticketing_table) IS NULL THEN
      RAISE EXCEPTION 'Ticketing relation public.% is missing', ticketing_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'promo_code'
  ) <> 17 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'promo_code_redemption'
  ) <> 5 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'ticket_refund_request'
  ) <> 13 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'ticket_transfer'
  ) <> 13 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_waitlist'
  ) <> 14 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'stripe_payment_intent'
  ) <> 10 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'stripe_webhook_event'
  ) <> 5 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'ticket_qr_code'
  ) <> 5 THEN
    RAISE EXCEPTION 'A ticketing relation has an unexpected column count';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('event_ticket_order', 'stripe_payment_intent_id', 'character varying', 'YES'),
        ('event_ticket_order', 'promo_code_id', 'bigint', 'YES'),
        ('event_ticket_order', 'original_amount_cents', 'bigint', 'YES'),
        ('event_ticket_order', 'payment_method', 'character varying', 'YES'),
        ('event_ticket', 'current_holder_party_id', 'character varying', 'YES'),
        ('event_ticket', 'current_holder_email', 'character varying', 'YES'),
        ('event_ticket', 'current_holder_name', 'character varying', 'YES'),
        ('event_ticket', 'original_holder_party_id', 'character varying', 'YES'),
        ('event_ticket', 'transfer_history', 'character varying', 'YES'),
        ('event_ticket_tier', 'enable_waitlist', 'boolean', 'NO'),
        ('event_ticket_tier', 'allow_transfers', 'boolean', 'NO'),
        ('event_ticket_tier', 'refund_policy', 'character varying', 'NO'),
        ('event_ticket_tier', 'refund_deadline', 'timestamp with time zone', 'YES'),
        ('promo_code', 'discount_value', 'bigint', 'NO'),
        ('promo_code', 'current_redemptions', 'bigint', 'NO'),
        ('promo_code', 'valid_until', 'timestamp with time zone', 'YES'),
        ('promo_code_redemption', 'discount_amount_cents', 'bigint', 'NO'),
        ('ticket_refund_request', 'processed_at', 'timestamp with time zone', 'YES'),
        ('ticket_transfer', 'transfer_code', 'character varying', 'NO'),
        ('event_waitlist', 'quantity', 'bigint', 'NO'),
        ('stripe_payment_intent', 'stripe_payment_intent_id', 'character varying', 'NO'),
        ('stripe_webhook_event', 'stripe_event_id', 'character varying', 'NO'),
        ('ticket_qr_code', 'ticket_id', 'bigint', 'NO')
    ) AS expected(table_name, column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'Ticketing columns do not match the runtime schema';
  END IF;

  IF (
    SELECT COUNT(*) FROM pg_constraint
    WHERE conrelid IN (
      'public.promo_code'::regclass,
      'public.ticket_transfer'::regclass,
      'public.stripe_payment_intent'::regclass,
      'public.stripe_webhook_event'::regclass,
      'public.ticket_qr_code'::regclass
    ) AND contype = 'u' AND convalidated
  ) <> 5 THEN
    RAISE EXCEPTION 'A ticketing uniqueness constraint is missing or invalid';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('promo_code', 'unique_promo_code', 'u', 'UNIQUE (code)'),
        ('promo_code', 'promo_code_event_id_fkey', 'f', 'FOREIGN KEY (event_id) REFERENCES social_event(id)'),
        ('promo_code_redemption', 'promo_code_redemption_promo_code_id_fkey', 'f', 'FOREIGN KEY (promo_code_id) REFERENCES promo_code(id)'),
        ('promo_code_redemption', 'promo_code_redemption_order_id_fkey', 'f', 'FOREIGN KEY (order_id) REFERENCES event_ticket_order(id)'),
        ('ticket_refund_request', 'ticket_refund_request_order_id_fkey', 'f', 'FOREIGN KEY (order_id) REFERENCES event_ticket_order(id)'),
        ('ticket_transfer', 'unique_ticket_transfer_code', 'u', 'UNIQUE (transfer_code)'),
        ('ticket_transfer', 'ticket_transfer_ticket_id_fkey', 'f', 'FOREIGN KEY (ticket_id) REFERENCES event_ticket(id)'),
        ('event_waitlist', 'event_waitlist_event_id_fkey', 'f', 'FOREIGN KEY (event_id) REFERENCES social_event(id)'),
        ('event_waitlist', 'event_waitlist_tier_id_fkey', 'f', 'FOREIGN KEY (tier_id) REFERENCES event_ticket_tier(id)'),
        ('event_waitlist', 'event_waitlist_converted_order_id_fkey', 'f', 'FOREIGN KEY (converted_order_id) REFERENCES event_ticket_order(id)'),
        ('stripe_payment_intent', 'unique_stripe_payment_intent', 'u', 'UNIQUE (stripe_payment_intent_id)'),
        ('stripe_payment_intent', 'stripe_payment_intent_order_id_fkey', 'f', 'FOREIGN KEY (order_id) REFERENCES event_ticket_order(id)'),
        ('stripe_webhook_event', 'unique_stripe_webhook_event', 'u', 'UNIQUE (stripe_event_id)'),
        ('ticket_qr_code', 'unique_ticket_qr_code', 'u', 'UNIQUE (ticket_id)'),
        ('ticket_qr_code', 'ticket_qr_code_ticket_id_fkey', 'f', 'FOREIGN KEY (ticket_id) REFERENCES event_ticket(id)'),
        ('event_ticket_order', 'event_ticket_order_promo_code_id_fkey', 'f', 'FOREIGN KEY (promo_code_id) REFERENCES promo_code(id)')
    ) AS expected(table_name, constraint_name, constraint_type, definition)
    LEFT JOIN pg_constraint AS actual
      ON actual.conrelid = ('public.' || expected.table_name)::regclass
     AND actual.conname = expected.constraint_name
     AND actual.contype = expected.constraint_type::"char"
    WHERE actual.oid IS NULL
       OR NOT actual.convalidated
       OR replace(pg_get_constraintdef(actual.oid), 'public.', '') <> expected.definition
  ) THEN
    RAISE EXCEPTION 'A ticketing constraint definition is missing or invalid';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('promo_code', 'idx_promo_code_event', FALSE, '(event_id) WHERE (event_id IS NOT NULL)'),
        ('promo_code', 'idx_promo_code_lookup', FALSE, '(code, is_active)'),
        ('promo_code_redemption', 'idx_promo_redemption_order', FALSE, '(order_id)'),
        ('ticket_refund_request', 'idx_refund_order', FALSE, '(order_id)'),
        ('ticket_refund_request', 'idx_refund_status', FALSE, '(status, created_at)'),
        ('ticket_transfer', 'idx_transfer_ticket', FALSE, '(ticket_id)'),
        ('event_waitlist', 'idx_waitlist_event', FALSE, '(event_id, status)'),
        ('event_ticket_order', 'idx_ticket_order_promo', FALSE, '(promo_code_id) WHERE (promo_code_id IS NOT NULL)'),
        ('event_ticket_order', 'uq_event_ticket_order_stripe_payment_intent', TRUE, '(stripe_payment_intent_id) WHERE (stripe_payment_intent_id IS NOT NULL)'),
        ('event_ticket', 'idx_ticket_current_holder', FALSE, '(current_holder_party_id) WHERE (current_holder_party_id IS NOT NULL)')
    ) AS expected(table_name, index_name, is_unique, definition_fragment)
    LEFT JOIN pg_class AS relation
      ON relation.oid = ('public.' || expected.table_name)::regclass
    LEFT JOIN pg_class AS index_relation
      ON index_relation.relnamespace = 'public'::regnamespace
     AND index_relation.relname = expected.index_name
     AND index_relation.relkind = 'i'
    LEFT JOIN pg_index AS actual
      ON actual.indrelid = relation.oid
     AND actual.indexrelid = index_relation.oid
    WHERE actual.indexrelid IS NULL
       OR actual.indisunique <> expected.is_unique
       OR NOT actual.indisvalid
       OR NOT actual.indisready
       OR position(lower(expected.definition_fragment) IN lower(pg_get_indexdef(actual.indexrelid))) = 0
  ) THEN
    RAISE EXCEPTION 'A ticketing index definition is missing or invalid';
  END IF;

  FOREACH discovery_table IN ARRAY ARRAY[
    'external_venue_ref',
    'external_artist_ref',
    'external_event_ref',
    'external_event_discovery_run',
    'event_city',
    'event_city_subscription',
    'event_discovery_source'
  ] LOOP
    IF to_regclass('public.' || discovery_table) IS NULL THEN
      RAISE EXCEPTION 'Discovery relation public.% is missing', discovery_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'external_venue_ref'
  ) <> 5 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'external_artist_ref'
  ) <> 5 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'external_event_ref'
  ) <> 12 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'external_event_discovery_run'
  ) <> 14 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_city'
  ) <> 7 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_city_subscription'
  ) <> 4 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'event_discovery_source'
  ) <> 16 THEN
    RAISE EXCEPTION 'A discovery relation has an unexpected column count';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('external_venue_ref', 'id', 'bigint', 'NO'),
        ('external_venue_ref', 'provider', 'text', 'NO'),
        ('external_venue_ref', 'external_id', 'text', 'NO'),
        ('external_venue_ref', 'venue_id', 'bigint', 'NO'),
        ('external_venue_ref', 'last_seen_at', 'timestamp with time zone', 'NO'),
        ('external_artist_ref', 'id', 'bigint', 'NO'),
        ('external_artist_ref', 'provider', 'text', 'NO'),
        ('external_artist_ref', 'external_id', 'text', 'NO'),
        ('external_artist_ref', 'artist_id', 'bigint', 'NO'),
        ('external_artist_ref', 'last_seen_at', 'timestamp with time zone', 'NO'),
        ('external_event_ref', 'id', 'bigint', 'NO'),
        ('external_event_ref', 'provider', 'text', 'NO'),
        ('external_event_ref', 'external_id', 'text', 'NO'),
        ('external_event_ref', 'event_id', 'bigint', 'NO'),
        ('external_event_ref', 'city', 'text', 'NO'),
        ('external_event_ref', 'country_code', 'text', 'YES'),
        ('external_event_ref', 'source_url', 'text', 'YES'),
        ('external_event_ref', 'price_cents', 'integer', 'YES'),
        ('external_event_ref', 'currency', 'text', 'YES'),
        ('external_event_ref', 'last_seen_at', 'timestamp with time zone', 'NO'),
        ('external_event_ref', 'missing_runs', 'integer', 'NO'),
        ('external_event_ref', 'source_status', 'text', 'NO'),
        ('external_event_discovery_run', 'id', 'bigint', 'NO'),
        ('external_event_discovery_run', 'provider', 'text', 'NO'),
        ('external_event_discovery_run', 'run_date', 'date', 'NO'),
        ('external_event_discovery_run', 'scheduled_for', 'timestamp with time zone', 'YES'),
        ('external_event_discovery_run', 'status', 'text', 'NO'),
        ('external_event_discovery_run', 'cities_count', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_seen', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_updated', 'integer', 'NO'),
        ('external_event_discovery_run', 'venues_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'artists_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'error_message', 'text', 'YES'),
        ('external_event_discovery_run', 'started_at', 'timestamp with time zone', 'NO'),
        ('external_event_discovery_run', 'finished_at', 'timestamp with time zone', 'YES'),
        ('event_city', 'id', 'bigint', 'NO'),
        ('event_city', 'name', 'text', 'NO'),
        ('event_city', 'normalized_name', 'text', 'NO'),
        ('event_city', 'country_code', 'text', 'NO'),
        ('event_city', 'time_zone', 'text', 'YES'),
        ('event_city', 'created_at', 'timestamp with time zone', 'NO'),
        ('event_city', 'updated_at', 'timestamp with time zone', 'NO'),
        ('event_city_subscription', 'id', 'bigint', 'NO'),
        ('event_city_subscription', 'party_id', 'text', 'NO'),
        ('event_city_subscription', 'city_id', 'bigint', 'NO'),
        ('event_city_subscription', 'created_at', 'timestamp with time zone', 'NO'),
        ('event_discovery_source', 'id', 'bigint', 'NO'),
        ('event_discovery_source', 'source_key', 'text', 'NO'),
        ('event_discovery_source', 'name', 'text', 'NO'),
        ('event_discovery_source', 'source_type', 'text', 'NO'),
        ('event_discovery_source', 'feed_url', 'text', 'YES'),
        ('event_discovery_source', 'city_id', 'bigint', 'YES'),
        ('event_discovery_source', 'enabled', 'boolean', 'NO'),
        ('event_discovery_source', 'priority', 'integer', 'NO'),
        ('event_discovery_source', 'configuration', 'text', 'YES'),
        ('event_discovery_source', 'etag', 'text', 'YES'),
        ('event_discovery_source', 'last_modified', 'text', 'YES'),
        ('event_discovery_source', 'consecutive_failures', 'integer', 'NO'),
        ('event_discovery_source', 'last_success_at', 'timestamp with time zone', 'YES'),
        ('event_discovery_source', 'last_error', 'text', 'YES'),
        ('event_discovery_source', 'created_at', 'timestamp with time zone', 'NO'),
        ('event_discovery_source', 'updated_at', 'timestamp with time zone', 'NO')
    ) AS expected(table_name, column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'A discovery relation has unexpected column types or nullability';
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.external_venue_ref'::regclass
      AND conname = 'unique_external_venue_ref' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.external_artist_ref'::regclass
      AND conname = 'unique_external_artist_ref' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.external_event_ref'::regclass
      AND conname = 'unique_external_event_ref' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.event_city'::regclass
      AND conname = 'unique_event_city' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.event_city_subscription'::regclass
      AND conname = 'unique_event_city_subscription' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.event_discovery_source'::regclass
      AND conname = 'unique_event_discovery_source' AND contype = 'u'
  ) THEN
    RAISE EXCEPTION 'A discovery uniqueness constraint is missing';
  END IF;
  IF (
    SELECT COUNT(*) FROM pg_constraint
    WHERE conrelid IN (
      'public.external_venue_ref'::regclass,
      'public.external_artist_ref'::regclass,
      'public.external_event_ref'::regclass,
      'public.event_city_subscription'::regclass,
      'public.event_discovery_source'::regclass
    ) AND contype = 'f' AND convalidated
  ) <> 5 THEN
    RAISE EXCEPTION 'A discovery foreign key is missing or invalid';
  END IF;

  -- Inventory handlers select the full custody/payment/evidence shape even when
  -- no checkout rows exist. Keep legacy databases from failing at request time.
  -- The inventory module is optional in the ticketing/discovery migration fixture,
  -- so only enforce this contract when either inventory relation is present.
  IF to_regclass('public.asset') IS NOT NULL OR to_regclass('public.asset_checkout') IS NOT NULL THEN
    IF to_regclass('public.asset') IS NULL OR to_regclass('public.asset_checkout') IS NULL THEN
      RAISE EXCEPTION 'Inventory relation public.asset or public.asset_checkout is missing';
    END IF;

    IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('asset_checkout', 'disposition', 'character varying', 'NO'),
        ('asset_checkout', 'terms_and_conditions', 'character varying', 'YES'),
        ('asset_checkout', 'holder_email', 'character varying', 'YES'),
        ('asset_checkout', 'holder_phone', 'character varying', 'YES'),
        ('asset_checkout', 'payment_type', 'character varying', 'YES'),
        ('asset_checkout', 'payment_installments', 'bigint', 'YES'),
        ('asset_checkout', 'payment_reference', 'character varying', 'YES'),
        ('asset_checkout', 'payment_amount_cents', 'bigint', 'YES'),
        ('asset_checkout', 'payment_currency', 'character varying', 'YES'),
        ('asset_checkout', 'payment_outstanding_cents', 'bigint', 'YES'),
        ('asset_checkout', 'photo_out_url', 'character varying', 'YES'),
        ('asset_checkout', 'photo_in_url', 'character varying', 'YES')
    ) AS expected(table_name, column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
    ) THEN
      RAISE EXCEPTION 'Inventory checkout schema is missing required custody/payment/evidence columns';
    END IF;
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('external_venue_ref', 'unique_external_venue_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_venue_ref', 'external_venue_ref_venue_id_fkey', 'f', 'FOREIGN KEY (venue_id) REFERENCES venue(id)'),
        ('external_artist_ref', 'unique_external_artist_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_artist_ref', 'external_artist_ref_artist_id_fkey', 'f', 'FOREIGN KEY (artist_id) REFERENCES social_artist_profile(id)'),
        ('external_event_ref', 'unique_external_event_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_event_ref', 'external_event_ref_event_id_fkey', 'f', 'FOREIGN KEY (event_id) REFERENCES social_event(id)'),
        ('event_city', 'unique_event_city', 'u', 'UNIQUE (normalized_name, country_code)'),
        ('event_city_subscription', 'unique_event_city_subscription', 'u', 'UNIQUE (party_id, city_id)'),
        ('event_city_subscription', 'event_city_subscription_city_id_fkey', 'f', 'FOREIGN KEY (city_id) REFERENCES event_city(id) ON DELETE CASCADE'),
        ('event_discovery_source', 'unique_event_discovery_source', 'u', 'UNIQUE (source_key)'),
        ('event_discovery_source', 'event_discovery_source_city_id_fkey', 'f', 'FOREIGN KEY (city_id) REFERENCES event_city(id)')
    ) AS expected(table_name, constraint_name, constraint_type, definition)
    LEFT JOIN pg_constraint AS actual
      ON actual.conrelid = ('public.' || expected.table_name)::regclass
     AND actual.conname = expected.constraint_name
     AND actual.contype = expected.constraint_type::"char"
    WHERE actual.oid IS NULL
       OR NOT actual.convalidated
       OR replace(pg_get_constraintdef(actual.oid), 'public.', '') <> expected.definition
  ) THEN
    RAISE EXCEPTION 'A discovery constraint definition is missing or invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_indexes
    WHERE schemaname = 'public'
      AND tablename = 'external_event_ref'
      AND indexname = 'idx_external_event_ref_city'
      AND indexdef ILIKE '%lower(city)%'
  ) THEN
    RAISE EXCEPTION 'idx_external_event_ref_city is missing or invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_indexes
    WHERE schemaname = 'public'
      AND tablename = 'external_event_ref'
      AND indexname = 'idx_external_event_ref_event_id'
      AND indexdef ILIKE '%(event_id)%'
  ) THEN
    RAISE EXCEPTION 'idx_external_event_ref_event_id is missing or invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_indexes
    WHERE schemaname = 'public'
      AND tablename = 'external_event_discovery_run'
      AND indexname = 'unique_external_event_discovery_slot'
      AND indexdef ILIKE '%UNIQUE%'
      AND indexdef ILIKE '%(provider, scheduled_for)%'
      AND indexdef ILIKE '%WHERE (scheduled_for IS NOT NULL)%'
  ) THEN
    RAISE EXCEPTION 'unique_external_event_discovery_slot is missing or invalid';
  END IF;

  FOREACH social_table IN ARRAY ARRAY[
    'social_sync_account',
    'social_sync_post',
    'social_sync_run',
    'social_discovery_review'
  ] LOOP
    IF to_regclass('public.' || social_table) IS NULL THEN
      RAISE EXCEPTION 'Social-sync relation public.% is missing', social_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'social_sync_account'
  ) <> 12 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'social_sync_post'
  ) <> 20 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'social_sync_run'
  ) <> 9 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'social_discovery_review'
  ) <> 8 THEN
    RAISE EXCEPTION 'A social-sync relation has an unexpected column count';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('social_sync_account', 'party_id', 'bigint', 'YES'),
        ('social_sync_account', 'artist_profile_id', 'bigint', 'YES'),
        ('social_sync_account', 'platform', 'character varying', 'NO'),
        ('social_sync_account', 'external_user_id', 'character varying', 'NO'),
        ('social_sync_account', 'created_at', 'timestamp with time zone', 'NO'),
        ('social_sync_post', 'account_id', 'bigint', 'YES'),
        ('social_sync_post', 'platform', 'character varying', 'NO'),
        ('social_sync_post', 'external_post_id', 'character varying', 'NO'),
        ('social_sync_post', 'artist_party_id', 'bigint', 'YES'),
        ('social_sync_post', 'artist_profile_id', 'bigint', 'YES'),
        ('social_sync_post', 'fetched_at', 'timestamp with time zone', 'NO'),
        ('social_sync_post', 'ingest_source', 'character varying', 'NO'),
        ('social_sync_post', 'like_count', 'bigint', 'YES'),
        ('social_sync_post', 'created_at', 'timestamp with time zone', 'NO'),
        ('social_sync_post', 'updated_at', 'timestamp with time zone', 'NO'),
        ('social_sync_run', 'platform', 'character varying', 'NO'),
        ('social_sync_run', 'new_posts', 'bigint', 'NO'),
        ('social_discovery_review', 'social_sync_post_id', 'bigint', 'NO'),
        ('social_discovery_review', 'status', 'text', 'NO'),
        ('social_discovery_review', 'reviewed_by_party_id', 'bigint', 'YES'),
        ('social_discovery_review', 'created_at', 'timestamp with time zone', 'NO')
    ) AS expected(table_name, column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'Social-sync columns do not match the runtime schema';
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.social_sync_account'::regclass
      AND conname = 'unique_social_sync_account'
      AND contype = 'u'
      AND convalidated
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.social_sync_post'::regclass
      AND conname = 'unique_social_sync_post'
      AND contype = 'u'
      AND convalidated
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.social_discovery_review'::regclass
      AND conname = 'unique_social_discovery_review'
      AND contype = 'u'
      AND convalidated
  ) THEN
    RAISE EXCEPTION 'A social-sync uniqueness constraint is missing or invalid';
  END IF;

  IF (
    SELECT COUNT(*) FROM pg_constraint
    WHERE conrelid IN (
      'public.social_sync_account'::regclass,
      'public.social_sync_post'::regclass,
      'public.social_discovery_review'::regclass
    ) AND contype = 'f' AND convalidated
  ) <> 7 THEN
    RAISE EXCEPTION 'A social-sync foreign key is missing or invalid';
  END IF;

  FOREACH enrichment_table IN ARRAY ARRAY[
    'artist_profile_enrichment',
    'artist_inventory_reference',
    'artist_research_source',
    'artist_enrichment_suggestion',
    'artist_field_change',
    'artist_enrichment_run',
    'artist_identity_candidate',
    'artist_media_asset'
  ] LOOP
    IF to_regclass('public.' || enrichment_table) IS NULL THEN
      RAISE EXCEPTION 'Artist enrichment relation public.% is missing', enrichment_table;
    END IF;
  END LOOP;

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('artist_profile_enrichment', 'last_verified_at', 'timestamp with time zone', 'YES'),
        ('artist_profile_enrichment', 'confidence', 'double precision', 'YES'),
        ('artist_research_source', 'supported_fields', 'text', 'NO'),
        ('artist_research_source', 'content_hash', 'text', 'YES'),
        ('artist_enrichment_suggestion', 'decided_at', 'timestamp with time zone', 'YES'),
        ('artist_enrichment_suggestion', 'decided_by', 'bigint', 'YES'),
        ('artist_enrichment_suggestion', 'decision_note', 'text', 'YES'),
        ('artist_identity_candidate', 'decided_at', 'timestamp with time zone', 'YES'),
        ('artist_identity_candidate', 'decided_by', 'bigint', 'YES'),
        ('artist_identity_candidate', 'decision_note', 'text', 'YES'),
        ('artist_media_asset', 'source_content_hash', 'text', 'NO'),
        ('artist_media_asset', 'source_attribution', 'text', 'NO'),
        ('artist_media_asset', 'source_width', 'integer', 'NO'),
        ('artist_media_asset', 'source_height', 'integer', 'NO'),
        ('artist_media_asset', 'source_mime_type', 'text', 'NO'),
        ('artist_media_asset', 'source_byte_size', 'bigint', 'NO'),
        ('artist_media_asset', 'drive_file_id', 'text', 'NO')
    ) AS expected(table_name, column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = expected.table_name
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'Artist enrichment columns do not match the runtime schema';
  END IF;

  IF to_regclass('public.uq_artist_profile_slug_ci') IS NULL
     OR to_regclass('public.uq_artist_enrichment_active_full_run') IS NULL
     OR to_regclass('public.idx_artist_suggestion_queue') IS NULL
     OR to_regclass('public.idx_artist_field_change_history') IS NULL
     OR to_regclass('public.idx_artist_media_asset_hash') IS NULL
     OR to_regclass('public.unique_artist_media_drive_file') IS NULL THEN
    RAISE EXCEPTION 'Artist enrichment indexes are incomplete';
  END IF;

  IF (
    SELECT COUNT(*) FROM pg_constraint
    WHERE conrelid IN (
      'public.artist_profile_enrichment'::regclass,
      'public.artist_inventory_reference'::regclass,
      'public.artist_research_source'::regclass,
      'public.artist_enrichment_suggestion'::regclass,
      'public.artist_field_change'::regclass,
      'public.artist_enrichment_run'::regclass,
      'public.artist_identity_candidate'::regclass,
      'public.artist_media_asset'::regclass
    ) AND contype = 'f' AND convalidated
  ) <> 16 THEN
    RAISE EXCEPTION 'An artist enrichment foreign key is missing or invalid';
  END IF;

  FOREACH campaign_table IN ARRAY ARRAY[
    'campaign_automation',
    'campaign_automation_step',
    'campaign_enrollment',
    'campaign_delivery'
  ] LOOP
    IF to_regclass('public.' || campaign_table) IS NULL THEN
      RAISE EXCEPTION 'Campaign automation relation public.% is missing', campaign_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'campaign_automation'
  ) <> 9 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'campaign_automation_step'
  ) <> 12 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'campaign_enrollment'
  ) <> 11 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'campaign_delivery'
  ) <> 15 THEN
    RAISE EXCEPTION 'A campaign automation relation has an unexpected column count';
  END IF;

  FOREACH feature_table IN ARRAY ARRAY[
    'feature_access_requests',
    'feature_access_request_history',
    'feature_navigation_preferences'
  ] LOOP
    IF to_regclass('public.' || feature_table) IS NULL THEN
      RAISE EXCEPTION 'Feature discovery relation public.% is missing', feature_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'feature_access_requests'
  ) <> 16 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'feature_access_request_history'
  ) <> 8 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'feature_navigation_preferences'
  ) <> 9 OR EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('party_id', 'bigint', 'NO'),
        ('feature_id', 'text', 'NO'),
        ('favorite', 'boolean', 'NO'),
        ('pinned', 'boolean', 'NO'),
        ('pin_order', 'integer', 'YES'),
        ('last_visited_at', 'timestamp with time zone', 'YES'),
        ('use_count', 'integer', 'NO'),
        ('updated_at', 'timestamp with time zone', 'NO')
    ) AS expected(column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = 'feature_navigation_preferences'
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'Feature discovery tables do not match the runtime schema';
  END IF;

  FOREACH ddex_table IN ARRAY ARRAY[
    'catalog_release',
    'catalog_resource',
    'catalog_release_resource',
    'catalog_identifier',
    'catalog_credit',
    'catalog_deal',
    'catalog_deal_territory',
    'catalog_asset',
    'catalog_source_link',
    'ddex_document',
    'ddex_message_header',
    'ddex_validation_run',
    'ddex_validation_issue',
    'ddex_import_plan',
    'ddex_import_run',
    'ddex_import_change',
    'ddex_export',
    'ddex_partner',
    'ddex_job'
  ] LOOP
    IF to_regclass('public.' || ddex_table) IS NULL THEN
      RAISE EXCEPTION 'DDEX/catalog relation public.% is missing', ddex_table;
    END IF;
  END LOOP;

  IF (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'ddex_document'
  ) <> 18 OR EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('id', 'integer', 'NO'),
        ('file_name', 'text', 'NO'),
        ('private_uri', 'text', 'NO'),
        ('sha256', 'text', 'NO'),
        ('size_bytes', 'bigint', 'NO'),
        ('family', 'text', 'YES'),
        ('version', 'text', 'YES'),
        ('namespace', 'text', 'YES'),
        ('message_type', 'text', 'YES'),
        ('status', 'text', 'YES'),
        ('uploaded_by', 'integer', 'NO'),
        ('message_id', 'text', 'YES'),
        ('sender_id', 'text', 'YES'),
        ('recipient_id', 'text', 'YES'),
        ('created_at', 'timestamp with time zone', 'NO'),
        ('standard_version_id', 'uuid', 'YES'),
        ('message_type_id', 'uuid', 'YES'),
        ('workflow_state_id', 'uuid', 'YES')
    ) AS expected(column_name, data_type, is_nullable)
    LEFT JOIN information_schema.columns AS actual
      ON actual.table_schema = 'public'
     AND actual.table_name = 'ddex_document'
     AND actual.column_name = expected.column_name
    WHERE actual.column_name IS NULL
       OR actual.data_type <> expected.data_type
       OR actual.is_nullable <> expected.is_nullable
  ) THEN
    RAISE EXCEPTION 'ddex_document does not match the inbox runtime schema';
  END IF;

  IF EXISTS (
    SELECT 1 FROM ddex_document
    WHERE standard_version_id IS NULL
       OR workflow_state_id IS NULL
       OR family IS NOT NULL
       OR version IS NOT NULL
       OR message_type IS NOT NULL
       OR status IS NOT NULL
  ) OR EXISTS (
    SELECT 1 FROM ddex_partner
    WHERE jsonb_array_length(COALESCE(to_jsonb(ddex_partner)->'allowed_versions', '[]'::jsonb)) <> 0
  ) THEN
    RAISE EXCEPTION 'DDEX canonical cutover retained legacy values or missing IDs';
  END IF;
END
$verify$;`;
}

function deployCommand({ app, image, sha, onlyMachine, excludeMachine }) {
  const args = [
    'flyctl', 'deploy', '.',
    '--app', app,
    '--config', 'fly.toml',
    '--image', image,
    '--env', `SOURCE_COMMIT=${sha}`,
    '--env', `GIT_SHA=${sha}`,
    '--env', 'RUN_MIGRATIONS=false',
    '--env', 'EVENT_DISCOVERY_ENABLED=false',
    '--strategy', 'rolling',
    '--max-unavailable', '1',
    '--wait-timeout', '10m',
    '--update-only',
    '--yes',
  ];
  if (onlyMachine) args.push('--only-machines', onlyMachine);
  if (excludeMachine) args.push('--exclude-machines', excludeMachine);
  return args;
}

export function buildReleaseSteps(options = {}) {
  if (options.flyConfig) validateFlyConfig(options.flyConfig);
  const app = validateSafeName(options.app ?? 'tdf-hq', 'Fly app');
  const sha = normalizeFullSha(options.sha);
  const image = String(options.image ?? `diegueins680/tdf-hq:${sha}`);
  const descriptiveOnly = options.dryRun === true && options.execute !== true;
  const selectedCanary = options.canaryMachineId ?? options.canaryMachine;
  if (!descriptiveOnly && !selectedCanary) {
    throw new Error('Executable release steps require an explicit canary Machine.');
  }
  const canary = selectedCanary
    ? validateSafeName(selectedCanary, 'canary Machine')
    : '<selected-after-preflight>';
  if (!descriptiveOnly && !Array.isArray(options.remainingMachineIds)) {
    throw new Error('Executable release steps require the captured remaining Machine ids.');
  }
  if (Array.isArray(options.remainingMachineIds) && options.remainingMachineIds.length === 0) {
    throw new Error('A canary release requires at least two Machines so one remains untouched.');
  }
  const remaining = Array.isArray(options.remainingMachineIds)
    ? options.remainingMachineIds.map((id) => validateSafeName(id, 'remaining Machine'))
    : ['<each-captured-machine>'];
  const rawPreviousImage = options.priorImages?.[canary] ?? options.previousImage;
  const rawPreviousSha = options.priorShas?.[canary] ?? options.previousSha;
  if (!descriptiveOnly && (!rawPreviousImage || !rawPreviousSha)) {
    throw new Error('Executable release steps require the canary rollback image and source commit.');
  }
  const previousImage = String(rawPreviousImage ?? '<captured-before-canary>');
  const previousSha = descriptiveOnly && !rawPreviousSha
    ? '<captured-before-canary>'
    : normalizeFullSha(rawPreviousSha);

  const rollbackCanary = {
    id: 'rollback-canary',
    mutating: true,
    beforeStep: 'deploy-remaining',
    command: deployCommand({
      app,
      image: previousImage,
      sha: previousSha,
      onlyMachine: canary,
    }),
  };

  const remainingSteps = remaining.flatMap((machineId, index) => [
    {
      id: `deploy-remaining-${index + 1}`,
      machineId,
      mutating: true,
      command: deployCommand({ app, image, sha, onlyMachine: machineId }),
    },
    { id: `smoke-remaining-${index + 1}`, machineId, mutating: false },
  ]);

  return [
    { id: 'local-preflight', mutating: false, sha, image },
    { id: 'remote-preflight', mutating: false },
    { id: 'apply-migrations', mutating: true, migrations: options.migrations ?? [] },
    { id: 'verify-schema', mutating: false },
    {
      id: 'deploy-canary',
      mutating: true,
      command: deployCommand({ app, image, sha, onlyMachine: canary }),
    },
    { id: 'smoke-canary', mutating: false, onFailure: [rollbackCanary] },
    ...remainingSteps,
    { id: 'verify-fleet', mutating: false },
  ];
}

export function buildDeployPlan(options = {}) {
  if (options.flyConfig) validateFlyConfig(options.flyConfig);
  const execute = options.execute === true || options.dryRun === false;
  const steps = buildReleaseSteps(options).map((step) => {
    if (execute) return step;
    const { command: _command, onFailure: _onFailure, ...description } = step;
    return { ...description, mutating: false };
  });
  return {
    dryRun: !execute,
    mode: execute ? 'execute' : 'dry-run',
    steps,
    commands: execute
      ? steps.flatMap((step) => (step.command ? [step.command] : []))
      : [],
  };
}
