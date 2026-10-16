#!/usr/bin/env python3
"""Full-schema checkout admission tests. No provider HTTP or merchant secrets.

The Haskell provider-retry gate covers authenticated query/ledger/receipt behavior;
this matrix independently exercises the four PostgreSQL admission triggers.
"""
import concurrent.futures
import os
from pathlib import Path
import subprocess
import time
import uuid

import psycopg2
from psycopg2 import sql

ROOT = Path(__file__).resolve().parent.parent
URL = os.environ['TDF_PROVIDER_RETRY_DATABASE_URL']
subprocess.run(['node', '--input-type=module', '-e', '''
import { disposablePostgresUrl } from './scripts/lib/disposable-postgres-url.mjs';
disposablePostgresUrl(process.argv[1], {ci: process.env.CI === 'true'});
''', URL], cwd=ROOT, check=True)
DOMAINS = {
    'service_booking': 'Service booking', 'event_ticket_order': 'Event ticket',
    'course_registration': 'Course', 'domo_event_quote': 'Domo deposit',
}
PROVIDERS = ('placetopay', 'payphone')
NEGATIVES = ('binding_account', 'binding_environment', 'binding_currency', 'binding_amount',
             'resource_type', 'resource_id', 'reference', 'internal_reference',
             'operation_account', 'operation_environment', 'operation_provider',
             'operation_kind', 'missing_operation', 'missing_binding', 'intent_checkout', 'intent_provider',
             'intent_currency', 'intent_amount', 'missing_intent', 'attempt_status',
             'attempt_amount', 'attempt_currency', 'attempt_environment')


def connect():
    db = psycopg2.connect(URL, options='-c statement_timeout=8000 -c lock_timeout=6000')
    return db


def one(db, query, params=()):
    with db.cursor() as c:
        c.execute(query, params)
        return c.fetchone()[0]


def execute(db, query, params=()):
    with db.cursor() as c:
        c.execute(query, params)


def evidence(db, domain, provider, variant=None):
    checkout = one(db, 'SELECT payment_test.checkout(%s)::text', (domain,))
    due = one(db, 'SELECT total_minor FROM commerce_checkout_session WHERE id=%s::uuid', (checkout,))
    order = one(db, 'SELECT domain_order_id FROM commerce_checkout_session WHERE id=%s::uuid', (checkout,))
    intent, attempt = str(uuid.uuid4()), str(uuid.uuid4())
    reference, resource = 'TDF-' + uuid.uuid4().hex, str(uuid.uuid4().int % 10**15 + 1)
    intent_checkout = checkout
    if variant == 'intent_checkout':
        intent_checkout = one(db, 'SELECT payment_test.checkout(%s)::text', (domain,))
    execute(db, '''INSERT INTO commerce_payment_intent(id,checkout_id,status,capture_method,provider,
        payment_method,amount_minor,captured_minor,currency,idempotency_key)
        VALUES (%s::uuid,%s::uuid,'processing','automatic',%s,%s,%s,0,%s,%s)''',
        (intent, intent_checkout, 'paypal' if variant == 'intent_provider' else provider,
         'manual_bank_transfer' if provider == 'bank_transfer' else
         'payphone_wallet' if provider == 'payphone' else 'paypal_wallet' if provider == 'paypal' else 'card',
         due + (variant == 'intent_amount'), 'EUR' if variant == 'intent_currency' else 'USD', intent))
    execute(db, '''INSERT INTO commerce_payment_attempt(id,checkout_id,payment_intent_id,provider,
        environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key)
        VALUES (%s::uuid,%s::uuid,%s::uuid,%s,%s,%s,%s,%s,%s,'synthetic-merchant',%s)''',
        (attempt, checkout, None if variant == 'missing_intent' else intent, provider,
         'production' if variant == 'attempt_environment' else 'sandbox',
         'manual_verify' if provider == 'bank_transfer' else 'create',
         'processing' if variant == 'attempt_status' else 'succeeded',
         due + (variant == 'attempt_amount'), 'EUR' if variant == 'attempt_currency' else 'USD', attempt))
    if provider in PROVIDERS and variant != 'missing_operation':
        execute(db, '''INSERT INTO commerce_provider_operation(payment_attempt_id,provider,environment,
            merchant_account_ref,provider_reference,provider_resource_id,operation,idempotency_key,
            request_sha256,status,outcome_certainty,started_at)
            VALUES (%s::uuid,%s,%s,%s,%s,%s,%s,%s,repeat('a',64),'processing','ambiguous',now())''',
            (attempt, ('payphone' if provider == 'placetopay' else 'placetopay')
             if variant == 'operation_provider' else provider,
             'production' if variant == 'operation_environment' else 'sandbox',
             'other-merchant' if variant == 'operation_account' else 'synthetic-merchant', reference, resource,
             'query' if variant == 'operation_kind' else 'create', attempt))
    kind = {'placetopay': 'session', 'payphone': 'sale', 'datafast': 'checkout',
            'paypal': 'capture', 'stripe': 'payment', 'bank_transfer': 'manual_evidence'}[provider]
    if provider not in PROVIDERS:
        reference = order
    if variant == 'missing_binding':
        return checkout, due
    if provider == 'bank_transfer':
        resource = str(uuid.uuid4())
        execute(db, '''INSERT INTO commerce_manual_payment_evidence(id,checkout_id,payment_attempt_id,status)
          VALUES (%s::uuid,%s::uuid,%s::uuid,'awaiting_evidence')''', (resource, checkout, attempt))
        execute(db, '''UPDATE commerce_manual_payment_evidence SET status='submitted',
          customer_reference='synthetic-transfer',submitted_amount_minor=%s,currency='USD',
          submitted_at=now(),submitted_by=1 WHERE id=%s::uuid''', (due, resource))
        execute(db, "UPDATE commerce_manual_payment_evidence SET status='under_review',reviewed_by=2 WHERE id=%s::uuid", (resource,))
        execute(db, "UPDATE commerce_manual_payment_evidence SET status='approved',reviewed_at=now(),review_notes='Synthetic independent review' WHERE id=%s::uuid", (resource,))
    execute(db, '''INSERT INTO commerce_provider_binding(payment_attempt_id,provider,environment,
        merchant_account_ref,resource_type,provider_resource_id,merchant_reference,amount_minor,currency)
        VALUES (%s::uuid,%s,%s,%s,%s,%s,%s,%s,%s)''',
        (attempt, provider, 'production' if variant in ('binding_environment', 'attempt_environment') else 'sandbox',
         'other-merchant' if variant == 'binding_account' else 'synthetic-merchant',
         'payment' if variant == 'resource_type' else kind,
         resource + '-other' if variant == 'resource_id' else resource,
         order if variant == 'internal_reference' else reference + '-other' if variant == 'reference' else reference,
         due + (variant in ('binding_amount', 'attempt_amount')),
         'EUR' if variant in ('binding_currency', 'attempt_currency') else 'USD'))
    return checkout, due


def settle(db, checkout, due):
    with db.cursor() as c:
        c.execute('''UPDATE commerce_checkout_session SET status='paid',paid_minor=%s,paid_at=now()
            WHERE id=%s::uuid AND status<>'paid' RETURNING id''', (due, checkout))
        return len(c.fetchall())


def snapshot(db, checkout):
    return one(db, '''SELECT jsonb_build_array(to_jsonb(c),
      (SELECT to_jsonb(r) FROM service_booking_checkout_runtime r WHERE checkout_id=c.id),
      (SELECT to_jsonb(r) FROM event_ticket_checkout_runtime r WHERE checkout_id=c.id),
      (SELECT to_jsonb(r) FROM course_registration_checkout_runtime r WHERE checkout_id=c.id),
      (SELECT to_jsonb(r) FROM domo_event_quote_runtime r WHERE checkout_id=c.id),
      (SELECT count(*) FROM commerce_receipt WHERE checkout_id=c.id),
      (SELECT count(*) FROM commerce_checkout_audit_event WHERE checkout_id=c.id))::text
      FROM commerce_checkout_session c WHERE id=%s::uuid''', (checkout,))


def denied(db, domain, checkout, due):
    before = snapshot(db, checkout)
    execute(db, 'SAVEPOINT denied')
    try:
        settle(db, checkout, due)
    except psycopg2.Error as error:
        assert error.pgcode == 'P0001', (domain, error.pgcode, error.diag.message_primary)
        assert error.diag.message_primary == (
            DOMAINS[domain] + ' checkout cannot become paid without bound verified payment evidence'), error
        execute(db, 'ROLLBACK TO SAVEPOINT denied')
        assert snapshot(db, checkout) == before
        return
    raise AssertionError('Invalid evidence admitted: ' + domain)


def matrix():
    count = 0
    for domain in DOMAINS:
        for provider in PROVIDERS:
            with connect() as db:
                checkout, due = evidence(db, domain, provider)
                assert settle(db, checkout, due) == 1
                paid = snapshot(db, checkout)
                assert settle(db, checkout, due) == 0
                assert snapshot(db, checkout) == paid
                db.rollback()
                count += 1
            for variant in NEGATIVES:
                with connect() as db:
                    execute(db, 'SAVEPOINT fixture')
                    try:
                        checkout, due = evidence(db, domain, provider, variant)
                    except psycopg2.Error as error:
                        if variant.startswith('binding_'):
                            expected_error = 'Provider binding does not match its payment attempt'
                        else:
                            assert variant in ('attempt_amount', 'attempt_currency', 'attempt_environment'), (variant, error)
                            expected_error = 'Payment attempt does not match checkout environment, amount, or currency'
                        assert error.pgcode == 'P0001' and error.diag.message_primary == expected_error, (variant, error)
                        execute(db, 'ROLLBACK TO SAVEPOINT fixture')
                    else:
                        denied(db, domain, checkout, due)
                    db.rollback()
                    count += 1
    for domain in DOMAINS:
        for provider in ('datafast', 'paypal', 'stripe', 'bank_transfer'):
            for variant in (None, 'reference'):
                with connect() as db:
                    checkout, due = evidence(db, domain, provider, variant)
                    if variant is None:
                        assert settle(db, checkout, due) == 1
                    else:
                        denied(db, domain, checkout, due)
                    db.rollback()
                    count += 1
    print(f'Provider/domain matrix: {count} cases passed', flush=True)


def races():
    count = 0
    for domain in DOMAINS:
        for provider in PROVIDERS:
            for isolation in ('READ COMMITTED', 'REPEATABLE READ'):
                with connect() as setup:
                    checkout, due = evidence(setup, domain, provider)
                holder, observer = connect(), connect()
                observer.autocommit = True  # each poll sees newly connected waiters
                released = False
                try:
                    one(holder, 'SELECT id FROM commerce_checkout_session WHERE id=%s::uuid FOR UPDATE', (checkout,))
                    def competing():
                        with connect() as worker:
                            worker.set_session(isolation_level=isolation)
                            try:
                                changed = settle(worker, checkout, due)
                                worker.commit()
                                return changed
                            except psycopg2.errors.SerializationFailure:
                                worker.rollback()
                                assert isolation == 'REPEATABLE READ'
                                changed = settle(worker, checkout, due)
                                worker.commit()
                                return changed
                    with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
                        future = executor.submit(competing)
                        try:
                            deadline = time.monotonic() + 4
                            while time.monotonic() < deadline:
                                if future.done():
                                    future.result()
                                    raise AssertionError('Contender completed before lock release')
                                blocked = one(observer, '''SELECT EXISTS(SELECT 1 FROM pg_stat_activity
                                  WHERE datname=current_database() AND %s=ANY(pg_blocking_pids(pid)))''',
                                  (holder.get_backend_pid(),))
                                if blocked:
                                    break
                                time.sleep(.02)
                            assert blocked, 'No observed PostgreSQL blocking'
                            assert not future.done()
                            assert settle(holder, checkout, due) == 1
                            holder.commit()
                            released = True
                        finally:
                            if not released:
                                holder.rollback()
                        assert future.result(timeout=10) == 0
                    with connect() as replay:
                        paid = snapshot(replay, checkout)
                        assert settle(replay, checkout, due) == 0
                        assert snapshot(replay, checkout) == paid
                        for table, column, expected in (
                            ('service_booking_checkout_runtime', 'fulfillment_status', 'confirmed'),
                            ('event_ticket_checkout_runtime', 'payment_status', 'paid'),
                            ('course_registration_checkout_runtime', 'enrollment_status', 'enrolled'),
                            ('domo_event_quote_runtime', 'fulfillment_status', 'date_reserved')):
                            if list(DOMAINS).index(domain) == (
                                    'service_booking_checkout_runtime', 'event_ticket_checkout_runtime',
                                    'course_registration_checkout_runtime', 'domo_event_quote_runtime').index(table):
                                result = one(replay, sql.SQL('SELECT {} FROM {} WHERE checkout_id=%s::uuid').format(
                                    sql.Identifier(column), sql.Identifier(table)), (checkout,))
                                assert result == expected, result
                    count += 1
                finally:
                    holder.close()
                    observer.close()
    print(f'Witnessed concurrency and retry: {count} cases passed', flush=True)


def controls():
    # Installing the original guard in a transaction must reproduce rejection of
    # each valid new rail in all four domains; rollback restores the candidate.
    originals = (
        ('2026-08-16_service_booking_checkout_runtime.sql', 'service_booking_require_verified_payment'),
        ('2026-08-18_public_ticket_checkout_runtime.sql', 'event_ticket_checkout_require_verified_payment'),
        ('2026-08-17_course_checkout_runtime.sql', 'course_checkout_require_verified_payment'),
        ('2026-08-18_domo_quote_checkout_runtime.sql', 'domo_quote_require_verified_payment'))
    for domain, (filename, function) in zip(DOMAINS, originals):
        text = (ROOT / 'tdf-hq/sql' / filename).read_text()
        begin = text.index('CREATE OR REPLACE FUNCTION ' + function + '()')
        end = text.index('END $$;', begin) + len('END $$;')
        for provider in PROVIDERS:
            with connect() as db:
                execute(db, text[begin:end])
                checkout, due = evidence(db, domain, provider)
                denied(db, domain, checkout, due)
                db.rollback()
    # A deliberately removed reference comparison must be detected by the real
    # negative matrix, rather than passing merely because some other guard fires.
    migration = (ROOT / 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql').read_text()
    begin = migration.index('CREATE OR REPLACE FUNCTION commerce_checkout_has_verified_payment(')
    end = migration.index('$$;', begin) + len('$$;')
    for provider in PROVIDERS:
        with connect() as db:
            execute(db, migration[begin:end].replace(
                'AND operation.provider_reference = binding.merchant_reference', ''))
            checkout, due = evidence(db, 'service_booking', provider, 'reference')
            assert settle(db, checkout, due) == 1, 'Reference mutation control was vacuous'
            db.rollback()
    print('Original guard and reference mutation controls: 10 expected failures witnessed', flush=True)


if __name__ == '__main__':
    with connect() as db:
        assert one(db, 'SELECT current_database()') == 'tdf_provider_retry_test'
        expected = __import__('json').loads((ROOT / 'scripts/production-migrations.json').read_text())['migrations']
        assert one(db, 'SELECT count(*) FROM tdf_schema_migration') == len(expected)
        revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
        assert one(db, 'SELECT bool_and(source_commit=%s) FROM tdf_schema_migration', (revision,))
    matrix()
    races()
    controls()
