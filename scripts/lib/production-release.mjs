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
  IF to_regclass('public.event_ticket_order') IS NULL
     OR to_regclass('public.venue') IS NULL
     OR to_regclass('public.social_artist_profile') IS NULL
     OR to_regclass('public.social_event') IS NULL THEN
    RAISE EXCEPTION 'Required ticketing/event base tables are missing';
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
END
$preflight$;
ROLLBACK;
`.trim();
}

export function buildSchemaVerificationSql(options = {}) {
  const header = options.includePsqlHeader === false ? '' : '\\set ON_ERROR_STOP on\n';
  return `${header}DO $verify$
DECLARE
  discovery_table TEXT;
  ticketing_table TEXT;
BEGIN
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
    'external_event_discovery_run'
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
  ) <> 7 OR (
    SELECT COUNT(*) FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = 'external_event_discovery_run'
  ) <> 13 THEN
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
        ('external_event_ref', 'source_url', 'text', 'YES'),
        ('external_event_ref', 'last_seen_at', 'timestamp with time zone', 'NO'),
        ('external_event_discovery_run', 'id', 'bigint', 'NO'),
        ('external_event_discovery_run', 'provider', 'text', 'NO'),
        ('external_event_discovery_run', 'run_date', 'date', 'NO'),
        ('external_event_discovery_run', 'status', 'text', 'NO'),
        ('external_event_discovery_run', 'cities_count', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_seen', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'events_updated', 'integer', 'NO'),
        ('external_event_discovery_run', 'venues_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'artists_created', 'integer', 'NO'),
        ('external_event_discovery_run', 'error_message', 'text', 'YES'),
        ('external_event_discovery_run', 'started_at', 'timestamp with time zone', 'NO'),
        ('external_event_discovery_run', 'finished_at', 'timestamp with time zone', 'YES')
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
    WHERE conrelid = 'public.external_event_ref'::regclass
      AND conname = 'unique_external_event_local' AND contype = 'u'
  ) OR NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'public.external_event_discovery_run'::regclass
      AND conname = 'unique_external_event_discovery_run' AND contype = 'u'
  ) THEN
    RAISE EXCEPTION 'A discovery uniqueness constraint is missing';
  END IF;
  IF (
    SELECT COUNT(*) FROM pg_constraint
    WHERE conrelid IN (
      'public.external_venue_ref'::regclass,
      'public.external_artist_ref'::regclass,
      'public.external_event_ref'::regclass
    ) AND contype = 'f' AND convalidated
  ) <> 3 THEN
    RAISE EXCEPTION 'A discovery foreign key is missing or invalid';
  END IF;

  -- Inventory handlers select the full custody/payment/evidence shape even when
  -- no checkout rows exist. Keep legacy databases from failing at request time.
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

  IF EXISTS (
    SELECT 1
    FROM (
      VALUES
        ('external_venue_ref', 'unique_external_venue_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_venue_ref', 'external_venue_ref_venue_id_fkey', 'f', 'FOREIGN KEY (venue_id) REFERENCES venue(id)'),
        ('external_artist_ref', 'unique_external_artist_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_artist_ref', 'external_artist_ref_artist_id_fkey', 'f', 'FOREIGN KEY (artist_id) REFERENCES social_artist_profile(id)'),
        ('external_event_ref', 'unique_external_event_ref', 'u', 'UNIQUE (provider, external_id)'),
        ('external_event_ref', 'unique_external_event_local', 'u', 'UNIQUE (event_id)'),
        ('external_event_ref', 'external_event_ref_event_id_fkey', 'f', 'FOREIGN KEY (event_id) REFERENCES social_event(id)'),
        ('external_event_discovery_run', 'unique_external_event_discovery_run', 'u', 'UNIQUE (provider, run_date)')
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
