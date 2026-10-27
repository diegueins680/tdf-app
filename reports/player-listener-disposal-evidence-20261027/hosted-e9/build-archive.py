from pathlib import Path
import argparse,gzip,tarfile,hashlib,json,io
p=argparse.ArgumentParser();p.add_argument('--members',type=Path,required=True);p.add_argument('--output',type=Path,required=True);a=p.parse_args();names=json.loads((a.members/'replay-inputs.json').read_text())
with a.output.open('wb') as out:
 with gzip.GzipFile(filename='',fileobj=out,mode='wb',mtime=0) as gz:
  with tarfile.open(fileobj=gz,mode='w') as t:
   for n in sorted(names):
    assert not Path(n).is_absolute() and '..' not in Path(n).parts;b=(a.members/n).read_bytes();i=tarfile.TarInfo(n);i.size=len(b);i.mode=0o644;i.mtime=0;i.uid=i.gid=0;i.uname=i.gname='';t.addfile(i,io.BytesIO(b))
print(hashlib.sha256(a.output.read_bytes()).hexdigest())
