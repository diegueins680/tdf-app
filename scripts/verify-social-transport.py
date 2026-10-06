#!/usr/bin/env python3
"""Actual loopback dispatch controls; never contacts a provider."""
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
IG = 'tdf-hq/src/TDF/Services/InstagramMessaging.hs'
FB = 'tdf-hq/src/TDF/Services/FacebookMessaging.hs'
WA = 'tdf-hq/src/TDF/WhatsApp/Client.hs'
MANAGER = 'tdf-hq/src/TDF/Services/MessagingManager.hs'


def main():
    bindings = {
        IG: 'sendInstagramTextWithContextAndTag = sendInstagramTextWithContextAndTagUsing sharedMessagingManager',
        FB: 'sendFacebookText = sendFacebookTextUsing sharedMessagingManager',
        'tdf-hq/src/TDF/WhatsApp/Transport.hs': 'manager <- pure sharedMessagingManager',
        'tdf-hq/src/TDF/WhatsApp/Service.hs': 'mgr <- pure sharedMessagingManager',
    }
    for path, binding in bindings.items():
        source = (ROOT / path).read_text()
        assert binding in source and 'TDF.Services.MessagingManager (sharedMessagingManager)' in source, path
        assert 'sharedTlsManager' not in source, path
    originals = {p: (ROOT / p).read_text() for p in [IG, FB, WA, MANAGER]}
    controls = [
        ('intended', [], None),
        ('replay-pooled-connection', [(MANAGER, 'managerRetryableException = const False',
          'managerRetryableException = managerRetryableException tlsManagerSettings')],
         'does not replay a lost reply on a previously warmed pooled connection'),
        ('repeat-after-lost-reply', [(IG,
         '      pure $ case respE of',
         '      _ <- case respE of\n        Left _ -> Safe.tryAny (httpLbs req manager)\n        Right _ -> pure respE\n      pure $ case respE of')],
         'does not dispatch a fallback after acceptance followed by a lost response'),
        ('follow-message-redirect', [(p, 'redirectCount = 0', 'redirectCount = 10') for p in [IG, FB, WA]],
         'does not follow a redirect with a second message request'),
        ('swallow-cancellation', [(p, 'import qualified Control.Exception.Safe as Safe',
          'import qualified Control.Exception as Safe\n-- intentionally invalid synchronous/async boundary') for p in [IG, FB, WA]]
          + [(p, 'Safe.tryAny', 'Safe.try') for p in [IG, FB, WA]],
         'propagates cancellation after dispatch without a fallback'),
    ]
    results = []
    with tempfile.TemporaryDirectory(prefix='tdf-social-transport-') as directory:
        work = Path(directory)
        test_main = work / 'Main.hs'
        test_main.write_text('import Test.Hspec\nimport qualified TDF.SocialTransportSpec\nmain = hspec TDF.SocialTransportSpec.spec\n')
        command = ['stack', '--stack-yaml', str(ROOT / 'tdf-hq/stack.yaml'), '--no-terminal',
                   'exec', '--', 'runghc', '-i', '-i' + str(work / 'tdf-hq/src'),
                   '-i' + str(ROOT / 'tdf-hq/src'), '-i' + str(ROOT / 'tdf-hq/test'), str(test_main)]
        for name, mutations, expected in controls:
            sources = dict(originals)
            for path, before, after in mutations:
                if before not in sources[path]:
                    raise ValueError('Mutation no longer corresponds to implementation: ' + name)
                sources[path] = sources[path].replace(before, after)
            for path, source in sources.items():
                target = work / path
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(source)
            result = subprocess.run(command, cwd=work, text=True, capture_output=True, timeout=180)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', result.stdout)
            if not summary or int(summary[1]) != 12:
                raise RuntimeError('Controls did not execute all twelve cases: ' + result.stdout + result.stderr)
            if expected:
                assert result.returncode != 0 and int(summary[2]) > 0, name
                assert expected in result.stdout.partition('Failures:')[2], result.stdout
            else:
                assert result.returncode == 0 and int(summary[2]) == 0, result.stdout
            results.append({'control': name, 'exitCode': result.returncode,
                            'examples': int(summary[1]), 'failures': int(summary[2]), 'output': result.stdout})
    print(json.dumps({'status': 'loopback-transport-controls-passed', 'results': results}, indent=2))


if __name__ == '__main__':
    main()
