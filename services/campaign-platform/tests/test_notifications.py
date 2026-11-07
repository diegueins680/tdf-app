import concurrent.futures,json,tempfile,threading,time,unittest
from pathlib import Path
from unittest.mock import patch,MagicMock
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from domain import Store,uid
from notifications import tick


class NotificationTests(unittest.TestCase):
 def setUp(self):
  self.tmp=tempfile.TemporaryDirectory();self.store=Store(Path(self.tmp.name)/'db.sqlite')
  self.config={'SMTP_HOST':'smtp.example.test','SMTP_FROM':'notices@example.test','PUBLIC_BASE':'https://example.test/campaigns'}
  self.preference(1);self.preference(2)
 def tearDown(self):self.tmp.cleanup()
 def preference(self,owner,email='recipient@example.test',channels=None):
  self.store.user({'id':owner,'name':'Synthetic recipient','admin':False})
  with self.store.db(True) as db:db.execute('INSERT OR REPLACE INTO notification_preferences(owner,email,channels) VALUES(?,?,?)',(owner,email,json.dumps(['email'] if channels is None else channels)))
 def notice(self,owner=1,delivered=None,created=None):
  identifier=uid()
  with self.store.db(True) as db:db.execute('INSERT INTO notices(id,owner,severity,message,created_at,delivered) VALUES(?,?,"error","Synthetic campaign failure",?,?)',(identifier,owner,time.time() if created is None else created,json.dumps(delivered or {})))
  return identifier
 def delivery(self,identifier):
  with self.store.db() as db:return json.loads(db.execute('SELECT delivered FROM notices WHERE id=?',(identifier,)).fetchone()[0])
 def test_delivered_rows_do_not_hide_a_newer_pending_notice(self):
  for _ in range(50):self.notice(2,{'email':'smtp_accepted_not_read_confirmation'},time.time()-60)
  pending=self.notice()
  with patch('notifications.smtplib.SMTP') as smtp:
   tick(self.store,self.config);send=smtp.return_value.__enter__.return_value.send_message
   send.assert_called_once();self.assertEqual(send.call_args.args[0]['Message-ID'],'<campaign-'+pending+'@tdfrecords.net>')
  self.assertEqual(self.delivery(pending)['email'],'smtp_accepted_not_read_confirmation')
 def test_opted_out_or_empty_recipient_rows_do_not_hide_other_users(self):
  self.preference(2,channels=['in_app']);self.preference(3,email='')
  for _ in range(50):self.notice(2,created=time.time()-60);self.notice(3,created=time.time()-60)
  pending=self.notice()
  with patch('notifications.smtplib.SMTP') as smtp:
   tick(self.store,self.config);smtp.return_value.__enter__.return_value.send_message.assert_called_once()
  self.assertEqual(self.delivery(pending)['email'],'smtp_accepted_not_read_confirmation')
 def test_uncertain_claims_and_expired_notices_are_not_replayed(self):
  identifiers=[self.notice(delivered={'email':state}) for state in ['sending_or_unknown','outcome_unknown_requires_review','smtp_accepted_not_read_confirmation']]
  expired=self.notice(created=time.time()-90000)
  with patch('notifications.smtplib.SMTP') as smtp:tick(self.store,self.config);smtp.assert_not_called()
  self.assertNotIn('email',self.delivery(expired))
  self.assertEqual([self.delivery(i)['email'] for i in identifiers],['sending_or_unknown','outcome_unknown_requires_review','smtp_accepted_not_read_confirmation'])
 def test_smtp_failure_retains_uncertain_outcome_without_automatic_retry(self):
  pending=self.notice()
  with patch('notifications.smtplib.SMTP') as smtp:
   send=smtp.return_value.__enter__.return_value.send_message;send.side_effect=OSError('synthetic SMTP failure')
   tick(self.store,self.config);tick(self.store,self.config);send.assert_called_once()
  self.assertEqual(self.delivery(pending)['email'],'outcome_unknown_requires_review')
 def test_durable_claim_prevents_concurrent_smtp_delivery(self):
  pending=self.notice();sending=threading.Event();release=threading.Event();pool=concurrent.futures.ThreadPoolExecutor(max_workers=2)
  def send(_):sending.set();self.assertTrue(release.wait(5))
  smtp_client=MagicMock();smtp_client.send_message.side_effect=send
  with patch('notifications.smtplib.SMTP') as smtp:
   smtp.return_value.__enter__.return_value=smtp_client
   try:
    first=pool.submit(tick,self.store,self.config);self.assertTrue(sending.wait(2))
    second=pool.submit(tick,self.store,self.config);second.result(timeout=2)
    self.assertEqual(self.delivery(pending)['email'],'sending_or_unknown')
   finally:release.set();pool.shutdown(wait=True)
   first.result();smtp_client.send_message.assert_called_once()
  self.assertEqual(self.delivery(pending)['email'],'smtp_accepted_not_read_confirmation')


if __name__=='__main__':unittest.main()
