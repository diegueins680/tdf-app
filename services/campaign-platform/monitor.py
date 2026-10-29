"""Independent monitor: creates durable notices even when the executor is down."""
import json,time
from server import make_runtime
from notifications import tick
s,p,c=make_runtime()
with s.db(True) as d:
 h=d.execute('SELECT at FROM heartbeat WHERE name="worker"').fetchone()
 if not h or time.time()-h['at']>90:
  for row in d.execute('SELECT DISTINCT owner FROM campaigns WHERE status="active"').fetchall():s.notice(d,row['owner'],'El ejecutor de campañas no responde. Los trabajos están conservados; revisaremos su recuperación antes de reenviar.','',severity='error')
 d.execute('INSERT INTO heartbeat VALUES(?,?,?) ON CONFLICT(name) DO UPDATE SET at=excluded.at',('monitor',time.time(),'{}'))
tick(s,c)
