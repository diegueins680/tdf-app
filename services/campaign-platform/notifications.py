"""Opt-in email delivery with a durable uncertain state; in-app is always available."""
import email.message,json,smtplib,time
from domain import uid

def tick(store,config):
 if not config.get('SMTP_HOST') or not config.get('SMTP_FROM'):return
 with store.db(True) as d:
  rows=d.execute('SELECT n.*,p.email,p.channels FROM notices n JOIN notification_preferences p ON p.owner=n.owner WHERE n.created_at>? ORDER BY n.created_at LIMIT 50',(time.time()-86400,)).fetchall()
  for n in rows:
   delivered=json.loads(n['delivered']);channels=json.loads(n['channels'])
   if not n['email'] or 'email' not in channels or delivered.get('email'):continue
   # Claim before SMTP. A crash after DATA must not produce a duplicate email.
   delivered['email']='sending_or_unknown';d.execute('UPDATE notices SET delivered=? WHERE id=?',(json.dumps(delivered),n['id']))
   break
  else:return
 msg=email.message.EmailMessage();msg['Subject']='TDF · Una campaña requiere tu atención';msg['From']=config['SMTP_FROM'];msg['To']=n['email'];msg['Message-ID']='<campaign-'+n['id']+'@tdfrecords.net>';msg.set_content(n['message']+'\n\nAbre tus campañas: '+config['PUBLIC_BASE']+'/')
 try:
  with smtplib.SMTP(config['SMTP_HOST'],int(config.get('SMTP_PORT',587)),timeout=20) as smtp:
   smtp.ehlo();smtp.starttls();smtp.ehlo()
   if config.get('SMTP_USERNAME'):smtp.login(config['SMTP_USERNAME'],config['SMTP_PASSWORD'])
   smtp.send_message(msg)
  delivered['email']='smtp_accepted_not_read_confirmation'
 except Exception:delivered['email']='outcome_unknown_requires_review'
 with store.db(True) as d:d.execute('UPDATE notices SET delivered=? WHERE id=?',(json.dumps(delivered),n['id']))
