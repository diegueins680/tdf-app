"""Operator-only encrypted download over SSH; never emits tokens or plaintext."""
import sys
import urllib.request
from drive_backup import BASE,DriveBackup,safe_id
from providers import NoRedirect


def main():
    from server import make_runtime
    store,providers,config=make_runtime()
    identifier=safe_id(sys.argv[1])
    with store.db() as db:
        settings=db.execute('SELECT * FROM backup_settings WHERE id=1').fetchone()
        row=db.execute('SELECT id FROM backup_objects WHERE remote_id=? AND verified_at IS NOT NULL',
                       (identifier,)).fetchone()
    if not settings or not row:raise ValueError('Unknown backup object')
    drive=DriveBackup(store,providers,dict(settings))
    drive.private_folder(settings['folder_id'])
    request=urllib.request.Request(BASE+'/files/'+identifier+'?alt=media',
                                   headers={'Authorization':'Bearer '+drive.token()})
    with urllib.request.build_opener(NoRedirect).open(request,timeout=120) as response:
        for block in iter(lambda:response.read(1024*1024),b''):sys.stdout.buffer.write(block)
    sys.stdout.buffer.flush()


if __name__=='__main__':
    try:main()
    except Exception:
        sys.stderr.write('backup_object_stream_failed\n');raise SystemExit(2)
