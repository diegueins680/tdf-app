#!/usr/bin/env python3
"""Actual private-store Haskell tests and compiled counterexamples; no source edits."""
import hashlib
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
BACKEND = ROOT / 'tdf-hq'
SOURCE = BACKEND / 'src/TDF/Contracts/Storage.hs'
source = SOURCE.read_text()
original = hashlib.sha256(SOURCE.read_bytes()).hexdigest()
variants = [
    ('intended', source, None),
    ('ephemeral-store', source.replace('root </> "uploads" </> "contracts" </> name', 'root </> "contracts" </> "store" </> name')
      .replace('directory = uploads </> "contracts"', 'directory = root </> "contracts" </> "store"')
      .replace('createDirectoryIfMissing True directory', 'createDirectoryIfMissing True uploads >> createDirectoryIfMissing True directory'),
     'reopens complete private documents in the persistent uploads tree'),
    ('ignored-collision', source.replace('unless published $ ioError (userError "Stored contract identifier already exists")', 'pure ()'),
     'does not overwrite an existing current document even for identical creation'),
    ('legacy-hides-conflict', source.replace('ioError (userError "Conflicting stored contract copies")', 'pure (Just b)'),
     'rejects differing copies including corrupt current bytes'),
]
with tempfile.TemporaryDirectory(prefix='tdf-contract-storage-') as temporary:
    for name, content, match in variants:
        assert name == 'intended' or content != source
        directory = Path(temporary) / name
        module = directory / 'TDF/Contracts/Storage.hs'
        module.parent.mkdir(parents=True); module.write_text(content)
        binary = directory / 'verify'
        build = subprocess.run(['stack','exec','--','ghc','-O0','-threaded','-i'+str(directory),'-isrc','-itest',
            '-outputdir',str(directory/'objects'),'-o',str(binary),'test/ContractStorageMain.hs'],
            cwd=BACKEND,text=True,capture_output=True,timeout=180)
        if build.returncode:
            print(build.stdout); print(build.stderr)
        assert build.returncode == 0, name + ': compile failure is not a counterexample'
        result = subprocess.run([str(binary), *([] if match is None else ['--match',match])],
            cwd=BACKEND,text=True,capture_output=True,timeout=30)
        if match is None:
            assert result.returncode == 0 and '10 examples, 0 failures' in result.stdout, result.stdout
        else:
            assert result.returncode != 0 and '1 example, 1 failure' in result.stdout and match in result.stdout, result.stdout
            assert ('expected:' in result.stdout or 'did not get expected exception: IOException' in result.stdout), 'Unexpected exception is not the intended counterexample: '+result.stdout
        print(name + ': ' + ('10 filesystem checks passed' if match is None else 'expected runtime counterexample detected'),flush=True)
assert hashlib.sha256(SOURCE.read_bytes()).hexdigest() == original
print('Contract storage source unchanged: '+original)
