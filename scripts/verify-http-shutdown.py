#!/usr/bin/env python3
"""Execute real loopback Warp/POSIX shutdown and controlled broken implementations."""
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import queue
import signal
import socket
import subprocess
import tempfile
import threading
import time
import urllib.request

ROOT = Path(__file__).resolve().parent.parent
MODULE = 'tdf-hq/src/TDF/App/Shutdown.hs'
FIXTURE = 'tdf-hq/test/HttpShutdownMain.hs'


def check(condition, name):
    if not condition:
        raise AssertionError(name)


def exercise(binary, scenario, stop_signal=signal.SIGTERM, repeat_signal=False):
    # The free-port handoff is fixture setup, not part of the shutdown guarantee.
    with socket.socket() as reservation:
        reservation.bind(('127.0.0.1', 0))
        port = reservation.getsockname()[1]
        if scenario != 'bind-failure':
            reservation.close()
        mode = scenario if scenario in ('startup-failure', 'startup-wait') else 'ready'
        process = subprocess.Popen([str(binary), str(port), mode], cwd=binary.parent,
            env={'PATH': '/usr/bin:/bin', 'LANG': 'C', 'HOME': str(binary.parent)},
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        lines, events = [], queue.Queue()
        def consume():
            for line in process.stdout:
                lines.append(line.rstrip())
                events.put(line.rstrip())
        reader = threading.Thread(target=consume, daemon=True)
        reader.start()
        def until(expected):
            deadline = time.monotonic()+8
            while time.monotonic() < deadline:
                try:
                    if events.get(timeout=max(.001, min(.1, deadline-time.monotonic()))) == expected:
                        return
                except queue.Empty:
                    if process.poll() is not None:
                        break
            raise AssertionError('fixture did not reach '+expected)
        try:
            if scenario in ('startup-failure', 'bind-failure'):
                code = process.wait(timeout=8)
                reader.join(timeout=1)
                check(code != 0, 'startup or listener failure must not report success')
                check('server-finished' not in lines, 'failed startup must not complete successfully')
                check('handlers-restored' in lines, 'signal handlers must be restored on failure')
                expected = 'synthetic startup failure' if scenario == 'startup-failure' else 'Address already in use'
                check(any(expected in line for line in lines), 'fixture must fail for the intended startup or bind error')
                return {'scenario': scenario, 'exitCode': code, 'failurePropagated': True}
            until('listening')
            with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
                def request():
                    with urllib.request.urlopen(f'http://127.0.0.1:{port}/'+scenario, timeout=8) as response:
                        return response.status, response.read()
                request_result = executor.submit(request) if scenario in ('hold', 'stuck') else None
                if request_result:
                    until('request-started')
                started = time.monotonic()
                process.send_signal(stop_signal)
                # Listener closure must reject a new TCP connection while the held
                # request is still running, not merely after process termination.
                if scenario == 'hold':
                    closed = False
                    deadline = time.monotonic()+.75
                    while time.monotonic() < deadline:
                        try:
                            with socket.create_connection(('127.0.0.1', port), timeout=.1):
                                pass
                        except (ConnectionRefusedError, ConnectionResetError):
                            closed = True
                            break
                        time.sleep(.02)
                    check(closed and process.poll() is None, 'listener must close before accepted request completes')
                    if repeat_signal:
                        process.send_signal(signal.SIGINT)
                        process.send_signal(signal.SIGTERM)
                if request_result and scenario == 'hold':
                    try:
                        result = request_result.result(timeout=8)
                    except Exception as error:
                        raise AssertionError('accepted request must finish during graceful drain') from error
                    check(result == (200, b'accepted-completed'), 'accepted request must finish during graceful drain')
                code = process.wait(timeout=8)
                elapsed = time.monotonic()-started
                check(code == 0, 'shutdown must return successful process status')
                check(elapsed < 7, 'HTTP drain must remain bounded')
                reader.join(timeout=1)
                check('server-finished' in lines, 'server supervisor must return after shutdown')
                check('handlers-restored' in lines, 'signal handlers must be restored after shutdown')
                if scenario == 'startup-wait':
                    check('startup-finished' in lines, 'unfinished startup must be cancelled')
                if request_result and scenario == 'stuck':
                    try:
                        result = request_result.result(timeout=1)
                    except Exception:
                        result = None
                    check(result is None, 'expired request must not fabricate successful completion')
                return {'scenario': scenario, 'signal': stop_signal.name, 'exitCode': code,
                        'elapsedSeconds': round(elapsed, 3), 'acceptedRequestCompleted': scenario == 'hold', 'repeatedSignals': repeat_signal}
        except subprocess.TimeoutExpired as error:
            raise AssertionError('HTTP drain must remain bounded') from error
        finally:
            if process.poll() is None:
                process.kill()
            process.wait(timeout=3)
            reader.join(timeout=1)
            process.stdout.close()


def verify():
    sources = {p: (ROOT/p).read_bytes() for p in (MODULE, FIXTURE)}
    controls = [('intended', None, None),
        ('listener-not-closed', ('when first closeListener', 'when first (pure ())'),
         'listener must close before accepted request completes'),
        ('no-request-drain', ('Warp.setGracefulShutdownTimeout (Just 5)', 'Warp.setGracefulShutdownTimeout (Just 0)'),
         'accepted request must finish during graceful drain')]
    reports = []
    for name, mutation, expected in controls:
        with tempfile.TemporaryDirectory(prefix='tdf-http-shutdown-') as directory:
            work = Path(directory)
            module = sources[MODULE].decode()
            if mutation:
                check(module.count(mutation[0]) == 1, 'shutdown mutation correspondence changed')
                module = module.replace(*mutation)
            local_module = work/'TDF/App/Shutdown.hs'
            local_module.parent.mkdir(parents=True)
            local_module.write_text(module)
            fixture = work/'Main.hs'
            fixture.write_bytes(sources[FIXTURE])
            binary = work/'probe'
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal', 'exec', '--',
                       'ghc', '-threaded', '-i', '-i'+str(work), '-outputdir', str(work/'build'),
                       '-o', str(binary), str(fixture)]
            build = subprocess.run(command, cwd=work, text=True, capture_output=True, timeout=120)
            check(build.returncode == 0, 'shutdown fixture compilation failed: '+build.stderr)
            if expected:
                try:
                    exercise(binary, 'hold')
                except AssertionError as error:
                    check(str(error) == expected, 'negative control failed for an unrelated reason: '+str(error))
                    reports.append({'control': name, 'status': 'detected', 'assertion': expected})
                else:
                    raise AssertionError('Broken shutdown implementation passed: '+name)
            else:
                observations = [exercise(binary, 'hold', s) for s in (signal.SIGTERM, signal.SIGINT)]
                observations.append(exercise(binary, 'hold', repeat_signal=True))
                observations += [exercise(binary, s) for s in ('stuck', 'startup-wait', 'startup-failure', 'bind-failure')]
                reports.append({'control': name, 'status': 'passed', 'scenarios': observations})
    check(all((ROOT/p).read_bytes() == value for p, value in sources.items()), 'shutdown source changed during verification')
    return {'schemaVersion': 1, 'sourceFingerprints': {p: hashlib.sha256(v).hexdigest() for p,v in sources.items()},
            'scope': 'Actual local POSIX signals, Warp HTTP requests and startup supervision; not worker drainage, PID1 container qualification, external-effect fencing or production release authorization.',
            'results': reports}


if __name__ == '__main__':
    print(json.dumps(verify(), indent=2))
