#!/usr/bin/env python3
"""Prepare a read-only aggregate query or summarize its explicitly supplied result.

No database connection, credentials, network access or production writes.
"""
import argparse
from datetime import datetime, timedelta, timezone
import json
from pathlib import Path
from zoneinfo import ZoneInfo

COUNTS = ('recorded_signups', 'recorded_first_values', 'artist_profiles_all_origins')


def cutoff(value):
    parsed = datetime.fromisoformat(value.replace('Z', '+00:00'))
    if parsed.tzinfo is None:
        raise ValueError('Cutoff must include an explicit UTC offset')
    return parsed.astimezone(timezone.utc)


def query(value):
    end = cutoff(value)
    start = end - timedelta(days=7)
    previous = start - timedelta(days=7)
    # Only normalized datetime values are interpolated, never caller SQL.
    def literal(dt):
        return "TIMESTAMPTZ '" + dt.isoformat() + "'"
    return f"""BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
SET LOCAL statement_timeout = '15s';
SET LOCAL lock_timeout = '2s';
SET LOCAL search_path = pg_catalog, public;
WITH windows AS (
  SELECT 'current' AS period, {literal(start)} AS start_at, {literal(end)} AS end_at
  UNION ALL SELECT 'previous', {literal(previous)}, {literal(start)}
), counts AS (
  SELECT period, jsonb_build_object(
    'recorded_signups', (SELECT count(*) FROM public.user_onboarding_progress p
      WHERE p.signup_completed_at >= start_at AND p.signup_completed_at < end_at),
    'recorded_first_values', (SELECT count(*) FROM public.user_onboarding_progress p
      WHERE p.first_value_completed_at >= start_at AND p.first_value_completed_at < end_at
        AND p.signup_completed_at IS NOT NULL AND p.first_value IS NOT NULL),
    'artist_profiles_all_origins', (SELECT count(*) FROM public.artist_profile a
      WHERE a.created_at >= start_at AND a.created_at < end_at)
  ) AS values FROM windows
)
SELECT jsonb_build_object('schema_version', 1, 'cutoff_utc', '{end.isoformat()}',
  'collected_at', CURRENT_TIMESTAMP, 'source', 'postgresql-read-only-aggregate',
  'periods', (SELECT jsonb_object_agg(period, values) FROM counts));
COMMIT;
"""


def summarize(data, value):
    end = cutoff(value)
    if set(data) != {'schema_version', 'cutoff_utc', 'collected_at', 'source', 'periods'}:
        raise ValueError('Unexpected export fields; provide only the aggregate query result')
    if data['schema_version'] != 1 or data['source'] != 'postgresql-read-only-aggregate':
        raise ValueError('Unsupported aggregate source or schema')
    if cutoff(data['cutoff_utc']) != end or cutoff(data['collected_at']) < end:
        raise ValueError('Export cutoff mismatch or collection before the requested cutoff')
    if set(data['periods']) != {'current', 'previous'}:
        raise ValueError('Both comparable periods are required')
    for period in data['periods'].values():
        if set(period) != set(COUNTS) or any(type(v) is not int or v < 0 for v in period.values()):
            raise ValueError('Every aggregate must be a nonnegative integer')
    metrics = {}
    for name in COUNTS:
        current, previous = (data['periods'][p][name] for p in ('current', 'previous'))
        metrics[name] = {'current': current, 'previous': previous,
                         'delta': current - previous,
                         'percent_change': None if previous == 0 else round(100 * (current - previous) / previous, 2)}
    return {
        'schema_version': 1, 'timezone': 'America/Guayaquil',
        'start': (end - timedelta(days=7)).astimezone(ZoneInfo('America/Guayaquil')).isoformat(),
        'cutoff': end.astimezone(ZoneInfo('America/Guayaquil')).isoformat(),
        'interval': '[start, cutoff)', 'evidence': 'supplied aggregate export; not independently verified',
        'source': data['source'], 'collected_at': data['collected_at'], 'metrics': metrics,
        'unavailable': {name: None for name in ('verified_human_signups', 'professional_profiles',
            'imported_or_unclaimed_profiles', 'weekly_active_users', 'campaign_attributed_conversions')},
        'limitations': [
            'Recorded signup timestamps are not proof of unique human users; synthetic accounts are not excluded.',
            'Deleted records and signup paths without onboarding timestamps are not recoverable from this snapshot.',
            'First values are stored milestones across all signup cohorts, not a same-week conversion rate.',
            'Artist profile creation includes every origin; edits and fan profile saves are not creations.',
            'No campaign causality, consented telemetry coverage or production environment is inferred.',
        ],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cutoff', required=True)
    parser.add_argument('--input', type=Path)
    args = parser.parse_args()
    if args.input:
        print(json.dumps(summarize(json.loads(args.input.read_text()), args.cutoff), indent=2, ensure_ascii=False))
    else:
        print(query(args.cutoff), end='')


if __name__ == '__main__':
    main()
