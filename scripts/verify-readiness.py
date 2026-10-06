#!/usr/bin/env python3
"""Run the actual readiness boundary and three source-derived negative controls."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
MODULE = 'tdf-hq/src/TDF/App/Readiness.hs'
TEST = 'tdf-hq/test/TDF/ReadinessSpec.hs'


def verify():
    sources = {name: (ROOT/name).read_text() for name in [MODULE, TEST]}
    controls = [
        ('intended', [], None),
        ('no-database-probe', [("databaseReady pool = withinReadinessDeadline 2000000 $ do\n  rows <- runSqlPool (rawSql \"SELECT 1\" []) pool\n  pure (rows == [Single (1 :: Int)])", 'databaseReady _ = pure True')], 'includes unavailable pool acquisition in the failure boundary'),
        ('no-deadline', [('result <- timeout microseconds (Safe.tryAny check)', 'result <- Just <$> Safe.tryAny check')], 'rejects work that exceeds its deadline'),
        ('swallowed-cancellation', [('import qualified Control.Exception.Safe as Safe', 'import qualified Control.Exception.Safe as Safe\nimport qualified Control.Exception as Unsafe'),
            ('result <- timeout microseconds (Safe.tryAny check)', 'result <- timeout microseconds (Unsafe.try check :: IO (Either Unsafe.SomeException Bool))')], 'propagates thread killed'),
    ]
    results = []
    for name, mutations, expected in controls:
        with tempfile.TemporaryDirectory(prefix='tdf-readiness-') as directory:
            work = Path(directory)
            for relative, content in sources.items():
                path = work/relative; path.parent.mkdir(parents=True, exist_ok=True); path.write_text(content)
            module = sources[MODULE]
            for before, after in mutations:
                if module.count(before) != 1: raise ValueError('Readiness source changed; review mutation correspondence')
                module = module.replace(before, after)
            (work/MODULE).write_text(module)
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal',
                       'exec', '--', 'runghc', '-i', '-i'+str(work/'tdf-hq/src'), str(work/TEST)]
            result = subprocess.run(command, cwd=work, text=True, capture_output=True, timeout=120)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', result.stdout)
            if not summary or int(summary[1]) != 9: raise RuntimeError('Readiness tests did not complete: '+result.stderr)
            if expected:
                assert result.returncode != 0 and int(summary[2]) > 0
                assert expected in result.stdout.partition('Failures:')[2]
            else: assert result.returncode == 0 and int(summary[2]) == 0, result.stdout
            results.append({'control': name, 'exitCode': result.returncode, 'examples': int(summary[1]),
                            'failures': int(summary[2]), 'output': result.stdout})
    for name, content in sources.items():
        if (ROOT/name).read_text() != content: raise RuntimeError('Source changed during verification')
    return {'sourceFingerprints': {name: hashlib.sha256(content.encode()).hexdigest() for name,content in sources.items()},
            'scope': 'Finite Haskell readiness/deadline/cancellation checks and three detected code mutations; not whole-server conformance.',
            'results': results}


if __name__ == '__main__': print(json.dumps(verify(), indent=2))
