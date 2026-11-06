#!/usr/bin/env python3
"""Recompute reported check timing; do not infer historical gate enforcement."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path


def timestamp(value):
    parsed = datetime.fromisoformat(value.replace('Z', '+00:00'))
    if parsed.tzinfo is None:
        raise ValueError('A timezone-aware timestamp is required')
    return parsed.astimezone(timezone.utc)


def derive(directory):
    raw = (directory / 'delivery.json').read_bytes()
    # Retain the exact snapshot published in commit2bca920c8; correcting an
    # interpretation must not rewrite the observed merge/check timestamps.
    if hashlib.sha256(raw).hexdigest() != '4e5eadf2500b1a15ac0f963c8cb59209944a11376ee03e1236a404afca0ef42f':
        raise ValueError('Original raw delivery snapshot changed')
    review_raw = (directory / 'review-observation.json').read_bytes()
    snapshot, reviews = json.loads(raw), json.loads(review_raw)
    merged = timestamp(snapshot['mergedAt'])
    buckets = {'reportedCompletionAtOrBeforeMerge': [],
               'reportedCompletionAfterMerge': [], 'completionTimeUnknown': []}
    inconsistent = []
    for index, row in enumerate(snapshot['statuses']):
        completed = row.get('completedAt')
        key = ('completionTimeUnknown' if completed is None else
               'reportedCompletionAtOrBeforeMerge' if timestamp(completed) <= merged else
               'reportedCompletionAfterMerge')
        entry = {'snapshotIndex': index, 'name': row.get('name', row.get('context')),
                 'reportedCompletedAt': completed,
                 'resultAtObservation': row.get('conclusion', row.get('state')),
                 'url': row.get('detailsUrl', row.get('targetUrl'))}
        buckets[key].append(entry)
        if completed and row.get('startedAt') and timestamp(row['startedAt']) > timestamp(completed):
            inconsistent.append(index)
    approval = [row for row in reviews['reviewsForReviewedHead']
                if row['commit_id'] == snapshot['reviewedHead']
                and row['state'] == 'APPROVED' and row['user'] != reviews['prAuthor']
                and timestamp(row['submitted_at']) <= merged]
    return {
        'schemaVersion': 1, 'algorithm': 'reported-completion-v1',
        'verifierSha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        'snapshotSha256': hashlib.sha256(raw).hexdigest(),
        'reviewObservationSha256': hashlib.sha256(review_raw).hexdigest(),
        'mergeCommit': snapshot['mergeCommit'], 'mergedAt': snapshot['mergedAt'],
        'snapshotMeasuredAt': snapshot['measuredAt'],
        'snapshotCount': len(snapshot['statuses']),
        'counts': {key: len(rows) for key, rows in buckets.items()},
        'entries': buckets,
        'reportedStartLaterThanCompletion': inconsistent,
        'observedIndependentApprovalRecordsPredatingMerge': approval,
        'historicalRequiredGateEnforcement': 'UNKNOWN',
        'limitations': [
            'This is an observation made after merge, not a retained pre-merge admission decision.',
            'Completion timestamps are GitHub-reported fields; inconsistent skipped-job intervals are retained.',
            'Untimed status contexts cannot establish their state at the merge instant.',
            'The historical required-check set, status history and enforcement audit are not retained here.',
            'Later success does not establish that a check gated this merge.',
        ],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write', action='store_true')
    args = parser.parse_args()
    directory = Path(__file__).resolve().parent
    result = derive(directory)
    target = directory / 'delivery-chronology.json'
    if args.write:
        target.write_text(json.dumps(result, indent=2) + '\n')
    elif json.loads(target.read_text()) != result:
        raise SystemExit('Stale or incorrect delivery chronology')
    print(json.dumps({'snapshotCount': result['snapshotCount'], 'counts': result['counts'],
                      'historicalRequiredGateEnforcement': 'UNKNOWN'}))


if __name__ == '__main__':
    main()
