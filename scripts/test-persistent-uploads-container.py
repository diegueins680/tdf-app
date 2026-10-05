#!/usr/bin/env python3
"""Hosted Linux CI only: actual entrypoint and synthetic container replacement."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import uuid

ROOT = Path(__file__).resolve().parent.parent
IMAGE = 'debian:bookworm-slim@sha256:3783cc01769c7b2b1b83a5c5ad96c815348e28ed7da68e2e3687004faa906251'


def main():
    if os.environ.get('GITHUB_ACTIONS') != 'true' or os.environ.get('RUNNER_ENVIRONMENT') != 'github-hosted':
        raise SystemExit('This synthetic Docker check requires an isolated GitHub-hosted runner')
    env = {key: value for key, value in os.environ.items() if not key.startswith('DOCKER_')}
    docker = ['docker', '--host', 'unix:///var/run/docker.sock']
    nonce = uuid.uuid4().hex
    owned = []
    results = []

    def invoke(args, timeout=90):
        return subprocess.run(docker + args, env=env, capture_output=True, text=True, timeout=timeout)

    def require(result, status=0):
        if result.returncode != status:
            raise RuntimeError(f'Container test failed: expected {status}, got {result.returncode}: {result.stderr[:2000]}')
        return result.stdout

    require(invoke(['pull', IMAGE], timeout=180))
    with tempfile.TemporaryDirectory(prefix='tdf-private-upload-') as directory:
        fixture = Path(directory)
        fixture.chmod(0o755)
        upload = fixture/'uploads'
        upload.mkdir(mode=0o777)
        upload.chmod(0o777)  # Synthetic fixture only, no production permission policy.
        server = fixture/'server'
        server.write_text('''#!/bin/sh
set -eu
case "$FIXTURE_ACTION" in
  write) printf 'synthetic-private-rider\\n' > /app/uploads/rider ;;
  read) test "$(cat /app/uploads/rider)" = 'synthetic-private-rider' ;;
  *) exit 96 ;;
esac
''')
        server.chmod(0o755)

        def run(name, mounts, action='read', expected=0):
            container_name = f'tdf-upload-test-{nonce}-{len(owned)}'
            owned.append(container_name)
            args = ['run', '--rm', '--name', container_name,
                    '--label', f'net.tdf.upload-test={nonce}', '--network', 'none',
                    '--memory', '128m', '--memory-swap', '128m', '--cpus', '0.5',
                    '--pids-limit', '32', '--cap-drop', 'ALL', '--security-opt', 'no-new-privileges',
                    '--read-only', '--user', '1000:1000', '--workdir', '/app',
                    '--mount', f'type=bind,source={ROOT}/tdf-hq,target=/contract,readonly',
                    '--mount', f'type=bind,source={server},target=/fixture-server,readonly',
                    '--env', 'APP_ENV=production', '--env', 'AUTO_APPLY_PRODUCTION_MIGRATIONS=false',
                    '--env', 'TDF_SERVER_BIN=/fixture-server', '--env', f'FIXTURE_ACTION={action}',
                    *mounts, IMAGE, 'sh', '/contract/production-entrypoint.sh']
            result = invoke(args)
            require(result, expected)
            if expected == 78 and 'persistent mount at /app/uploads' not in result.stderr:
                raise RuntimeError('Negative control failed for an unrelated reason')
            results.append({'case': name, 'expectedExit': expected, 'actualExit': result.returncode})

        try:
            bind = ['--mount', f'type=bind,source={upload},target=/app/uploads']
            run('write-on-persistent-mount', bind, 'write')
            run('replacement-retains-content', bind)
            run('missing-mount-rejected', [], expected=78)
            run('read-only-mount-rejected', ['--mount', f'type=bind,source={upload},target=/app/uploads,readonly'], expected=78)
            run('tmpfs-mount-rejected', ['--tmpfs', '/app/uploads:rw,uid=1000,gid=1000,mode=0700,size=1m'], expected=78)
        finally:
            for name in owned:
                found = invoke(['inspect', name])
                if found.returncode == 0:
                    row = json.loads(found.stdout)[0]
                    if row['Config']['Labels'].get('net.tdf.upload-test') != nonce:
                        raise RuntimeError('Refusing cleanup of a foreign container')
                    require(invoke(['rm', '-f', row['Id']]))
                elif 'No such' not in found.stderr:
                    raise RuntimeError('Could not confirm synthetic container cleanup')
        print(json.dumps({'sourceRevision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                          'image': IMAGE, 'checks': results,
                          'sourceSha256': {name: hashlib.sha256((ROOT/name).read_bytes()).hexdigest() for name in
                                           ['tdf-hq/production-entrypoint.sh', 'tdf-hq/persistent-uploads.sh']},
                          'scope': 'Actual shell entrypoint and container replacement; synthetic file, no application/database/provider.'}, indent=2))


if __name__ == '__main__':
    main()
