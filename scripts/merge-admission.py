#!/usr/bin/env python3
"""Serialize admission to protected main; never approve, merge, or waive CI.

Run by the single GitHub Actions concurrency group. Dry-run is the default.
Only GitHub metadata is read: no PR code, names, or bodies are executed.
"""
import argparse
import json
import os
import re
import subprocess

CONTEXT = 'main-integration-admission'


class GitHub:
    def __init__(self, repo):
        if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repo):
            raise ValueError('Invalid repository')
        self.repo = repo

    def api(self, endpoint, *args):
        run = subprocess.run(['gh', 'api', f'repos/{self.repo}/{endpoint}', *args],
                             capture_output=True, text=True, timeout=60)
        if run.returncode:
            raise RuntimeError('GitHub metadata/status request failed; admission withheld')
        return json.loads(run.stdout)

    def snapshot(self):
        main = self.api('branches/main')['commit']['sha']
        pages = self.api('pulls?state=open&base=main&per_page=100', '--paginate', '--slurp')
        pulls = [{'number': p['number'], 'head': p['head']['sha'],
                  'draft': p['draft']} for page in pages for p in page
                 if p['state'] == 'open' and p['base']['ref'] == 'main']
        if not re.fullmatch(r'[a-f0-9]{40}', main) or any(
                not re.fullmatch(r'[a-f0-9]{40}', p['head']) for p in pulls):
            raise ValueError('Incomplete source identity')
        # A merge during pagination invalidates the snapshot, not the checks.
        if self.api('branches/main')['commit']['sha'] != main:
            raise RuntimeError('Main changed during observation; retry admission')
        return {'main': main, 'pulls': sorted(pulls, key=lambda p: p['number'])}

    def status(self, sha, state, description, target):
        current = self.api(f'commits/{sha}/status?per_page=100')
        statuses = current['statuses']
        if current.get('total_count', len(statuses)) > len(statuses):
            raise RuntimeError('Incomplete status inventory; admission withheld')
        previous = next((s for s in statuses if s['context'] == CONTEXT), None)
        if previous and (previous['state'], previous['description']) == (state, description):
            return  # Avoid exhausting GitHub's per-SHA/context status limit.
        self.api(f'statuses/{sha}', '--method', 'POST', '-f', f'state={state}',
                 '-f', f'context={CONTEXT}', '-f', f'description={description}',
                 '-f', f'target_url={target}')


def reconcile(github, *, apply=False, target):
    before = github.snapshot()
    candidates = [p for p in before['pulls'] if not p['draft']]
    # Shared commit heads can represent different PRs. A SHA-level status cannot
    # select between those PRs, so do not pretend that it grants one exclusive slot.
    heads = [p['head'] for p in before['pulls']]
    duplicate_heads = {h for h in heads if heads.count(h) > 1}
    selected = next((p for p in candidates if p['head'] not in duplicate_heads), None)
    number = selected['number'] if selected else None
    report = {'context': CONTEXT, 'base': before['main'], 'admittedPR': number,
              'admittedHead': selected['head'] if selected else None,
              'waitingPRs': [p['number'] for p in before['pulls'] if p != selected],
              'duplicateHeads': sorted(duplicate_heads), 'applied': False}
    if not apply:
        return report
    # Revoke every other slot before granting one. A write error fails closed.
    for p in before['pulls']:
        if p != selected:
            reason = (f'Waiting for PR #{number}' if number else 'No eligible integration candidate')
            if p['head'] in duplicate_heads:
                reason = 'Duplicate PR head; consolidate or close the duplicate PR'
            github.status(p['head'], 'pending', f'{reason}; base {before["main"]}', target)
    # A new PR, changed head, draft transition, close, retarget or main movement
    # requires a fresh run. Never attach an admission decision to an old snapshot.
    if github.snapshot() != before:
        if selected:
            github.status(selected['head'], 'pending', 'Repository changed; awaiting fresh admission', target)
        raise RuntimeError('Repository changed during admission; retry')
    if selected:
        github.status(selected['head'], 'success',
                      f'Admitted PR #{number}; base {before["main"]}', target)
    report['applied'] = True
    return report


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--repo', default=os.environ.get('GITHUB_REPOSITORY', 'diegueins680/tdf-app'))
    parser.add_argument('--apply', action='store_true')
    args = parser.parse_args()
    target = f'https://github.com/{args.repo}/actions/workflows/merge-admission.yml'
    print(json.dumps(reconcile(GitHub(args.repo), apply=args.apply, target=target), indent=2))
