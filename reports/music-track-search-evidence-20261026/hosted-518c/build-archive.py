"""Offline deterministic evidence replay; no Git, services, network, or repository edits."""
from pathlib import Path
import argparse,gzip,hashlib,io,json,tarfile
p=argparse.ArgumentParser();p.add_argument('--source',type=Path,required=True);p.add_argument('--output',type=Path,required=True);a=p.parse_args()
root=a.source.resolve();final=json.loads((root/'root-final-qualification.json').read_text());assert final['readyForEvidenceSeal'] is True
names=final['files']+['root-final-qualification.json','build-archive.py'];assert len(names)==len(set(names))
files={}
for name in names:
 assert not Path(name).is_absolute() and '..' not in Path(name).parts
 path=root/name;assert path.is_file() and not path.is_symlink();files[name]=path.read_bytes()
with a.output.open('wb') as out:
 with gzip.GzipFile(filename='',mode='wb',fileobj=out,mtime=0) as gz:
  with tarfile.open(fileobj=gz,mode='w') as tar:
   for name,b in sorted(files.items()):
    i=tarfile.TarInfo(name);i.size=len(b);i.mode=0o644;i.mtime=0;i.uid=i.gid=0;i.uname=i.gname='';tar.addfile(i,io.BytesIO(b))
with tarfile.open(a.output) as tar:
 assert set(tar.getnames())==set(files)
 for i in tar:assert i.isfile() and tar.extractfile(i).read()==files[i.name]
print(json.dumps({'members':len(files),'bytes':a.output.stat().st_size,'sha256':hashlib.sha256(a.output.read_bytes()).hexdigest()}))
