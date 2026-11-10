#!/usr/bin/env python3
"""Execute shutdown contracts, source mutations and actual Unix signal controls."""
import hashlib
import json
import os
from pathlib import Path
import re
import select
import signal
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parent.parent
MODULE = 'tdf-hq/src/TDF/App/Shutdown.hs'
RETRY = 'tdf-hq/src/TDF/App/DatabaseRetry.hs'
TEST = 'tdf-hq/test/TDF/ShutdownSpec.hs'
BOOT = 'tdf-hq/src/TDF/App/Boot.hs'


def verify():
    sources = {name: (ROOT/name).read_text() for name in [MODULE, RETRY, TEST, BOOT]}
    assert 'makePoolWithRetry retries connStr = retryDatabaseConnection retries (makePool connStr)' in sources[BOOT]
    assert 'runBootServer = runWithUnixShutdown (30 * 1000 * 1000)' in sources[BOOT]
    assert 'Warp.setGracefulShutdownTimeout Nothing' in sources[BOOT]
    controls = [
        ('intended', MODULE, [], None),
        ('late-listener-open', MODULE, [('when closeNow stop', 'when closeNow (pure ())')],
         'closes a listener registered after stop was accepted'),
        ('false-clean-deadline', MODULE, [('Nothing -> throwIO ShutdownDeadlineExpired', 'Nothing -> pure ()')],
         'reports a drain deadline as failure'),
        ('late-publication', MODULE, [('if stopping then throwIO StartupAdmissionClosed else effect', 'if stopping then effect else effect')],
         'rejects publication after accepted stop'),
        ('cancel-after-lock', MODULE, [('cancelTask startup\n        stop <- acceptStop', 'stop <- acceptStop\n        cancelTask startup')],
         'cancels initialization that is blocked inside worker admission'),
        ('swallowed-startup-cancel', RETRY, [
            ('import qualified Control.Exception.Safe as Safe', 'import qualified Control.Exception.Safe as Safe\nimport qualified Control.Exception as Unsafe'),
            ('result <- Safe.tryAny connect', 'result <- Unsafe.try connect'),
            ('throwIO err', 'throwIO (err :: Unsafe.SomeException)')],
         'does not swallow cancellation as a synchronous startup retry'),
    ]
    results = []
    with tempfile.TemporaryDirectory(prefix='tdf-shutdown-') as directory:
        work = Path(directory)
        for relative, content in sources.items():
            path = work/relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
        main = work/'Main.hs'
        main.write_text('import Test.Hspec\nimport qualified TDF.ShutdownSpec\nmain = hspec TDF.ShutdownSpec.spec\n')
        command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal',
                   'exec', '--', 'runghc', '-i', '-i'+str(work/'tdf-hq/src'), '-i'+str(work/'tdf-hq/test')]
        for name, target, mutations, expected in controls:
            for relative in [MODULE, RETRY]: (work/relative).write_text(sources[relative])
            content = sources[target]
            for before, after in mutations:
                if content.count(before) != 1:
                    raise ValueError('Shutdown source changed; review mutation correspondence: '+name)
                content = content.replace(before, after)
            (work/target).write_text(content)
            result = subprocess.run(command+[str(main)], cwd=work, text=True, capture_output=True, timeout=90)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', result.stdout)
            if not summary or int(summary[1]) != 11:
                raise RuntimeError('Shutdown tests did not complete: '+result.stdout+result.stderr)
            if expected:
                assert result.returncode != 0 and int(summary[2]) > 0, name
                assert expected in result.stdout.partition('Failures:')[2], result.stdout
            else: assert result.returncode == 0 and int(summary[2]) == 0, result.stdout
            results.append({'control': name, 'exitCode': result.returncode, 'examples': int(summary[1]),
                            'failures': int(summary[2]), 'output': result.stdout})
        for relative in [MODULE, RETRY]: (work/relative).write_text(sources[relative])
        for mode in ['preparation', 'serving']:
            # A child owns the process-global handlers. No signal is sent to the
            # test runner, shell, process group, or any existing application.
            body = ('putStrLn "ready" >> threadDelay 100000000 >> pure (pure (),pure ())'
                    if mode == 'preparation' else
                    'do\n  closed <- newEmptyMVar\n  pure (registerServerStop control (putMVar closed ()) >> putStrLn "ready" >> readMVar closed, pure ())')
            main.write_text('import Control.Concurrent\nimport System.IO\nimport TDF.App.Shutdown\n'
                            'main = do\n hSetBuffering stdout LineBuffering\n'
                            ' runWithUnixShutdown 1000000 $ \\control -> '+body+'\n')
            for sig in [signal.SIGTERM, signal.SIGINT]:
                with tempfile.TemporaryFile(mode='w+') as errors:
                    child = subprocess.Popen(command+[str(main)], cwd=work, stdout=subprocess.PIPE,
                                             stderr=errors, text=True)
                    try:
                        assert select.select([child.stdout], [], [], 30)[0], 'signal fixture did not become ready'
                        assert child.stdout.readline().strip() == 'ready', 'signal fixture failed before readiness'
                        started = time.monotonic()
                        child.send_signal(sig)
                        code = child.wait(timeout=5)
                        elapsed = time.monotonic()-started
                        errors.seek(0)
                        assert code == 0, errors.read()
                        results.append({'control': mode+'-'+sig.name, 'exitCode': code, 'elapsedSeconds': elapsed})
                    finally:
                        if child.poll() is None:
                            child.kill()
                            child.wait(timeout=5)
                        child.stdout.close()
    for name, content in sources.items():
        if (ROOT/name).read_text() != content: raise RuntimeError('Source changed during verification')
    return {'sourceFingerprints': {name: hashlib.sha256(content.encode()).hexdigest() for name,content in sources.items()},
            'scope': 'Actual Haskell supervisor/retry/Warp request, five detected source mutations and four owned Unix-signal children. Not PID1 image or worker-drain evidence.',
            'results': results}


if __name__ == '__main__': print(json.dumps(verify(), indent=2))
