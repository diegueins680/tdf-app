import importlib.util
import pathlib
import unittest
from email.message import EmailMessage
import gzip
import io
import json
import struct
import tempfile
import zipfile
from unittest.mock import MagicMock, patch

spec = importlib.util.spec_from_file_location('monitor', pathlib.Path(__file__).with_name('mail-deliverability-monitor.py'))
monitor = importlib.util.module_from_spec(spec)
spec.loader.exec_module(monitor)


class MonitorTests(unittest.TestCase):
    def test_unreadable_zip_does_not_stop_later_reports(self):
        xml = b'<feedback><policy_published><domain>tdfrecords.net</domain></policy_published><record><row><count>1</count><policy_evaluated><dkim>pass</dkim></policy_evaluated></row></record></feedback>'
        archive = io.BytesIO()
        with zipfile.ZipFile(archive, 'w') as output:
            output.writestr('report.xml', xml)
        original = archive.getvalue()
        for case in ['encrypted', 'unsupported_compression']:
            with self.subTest(case=case):
                broken = bytearray(original)
                central = broken.index(b'PK\x01\x02')
                if case == 'encrypted':
                    struct.pack_into('<H', broken, 6, 1)
                    struct.pack_into('<H', broken, central + 8, 1)
                else:
                    struct.pack_into('<H', broken, 8, 999)
                    struct.pack_into('<H', broken, central + 10, 999)
                messages = []
                for payload, filename, subtype in [(bytes(broken), 'bad.zip', 'zip'), (xml, 'good.xml', 'xml')]:
                    message = EmailMessage()
                    message.set_content('Report')
                    message.add_attachment(payload, maintype='application', subtype=subtype, filename=filename)
                    messages.append(message.as_bytes())
                mailbox = MagicMock()
                mailbox.select.return_value = ('OK', [])
                mailbox.uid.side_effect = [('OK', [b'1 2']), ('OK', [(b'1', messages[0])]),
                                           ('OK', [(b'2', messages[1])]), ('OK', [b''])]
                process = MagicMock(returncode=0, stdout='DNS answer')
                live = MagicMock(returncode=0, stdout=json.dumps({'Machines': [{'id': 'abcd', 'state': 'started'}]}))
                config = MagicMock(returncode=0, stdout='SMTP_USERNAME=test\nSMTP_PASSWORD=test\n')
                with tempfile.TemporaryDirectory() as directory, \
                     patch.object(monitor.subprocess, 'run', side_effect=[process] * 20 + [live, config]), \
                     patch.object(monitor.imaplib, 'IMAP4_SSL') as client, patch('builtins.print'):
                    client.return_value.__enter__.return_value = mailbox
                    self.assertEqual(monitor.collect(pathlib.Path(directory)), 1)
                    report = json.loads((pathlib.Path(directory) / 'latest.json').read_text())
                self.assertTrue(report['mailboxReadSucceeded'])
                self.assertEqual(report['errors'], ['unparseable_report'])
                self.assertEqual(report['aggregateReports'], [{'messages': 1, 'aligned': 1}])

    def test_discovers_replacement_and_rejects_unhealthy_or_wrong_process(self):
        self.assertEqual(monitor.select_live_machine({'Machines': [
            {'id': 'aabb', 'state': 'stopped'},
            {'id': 'bbcc', 'state': 'started', 'checks': [{'status': 'critical'}]},
            {'id': 'ccdd', 'state': 'started', 'config': {'metadata': {'fly_process_group': 'worker'}}},
            {'id': 'ddee', 'state': 'started', 'checks': [{'status': 'passing'}]},
        ]}), 'ddee')
        with self.assertRaises(RuntimeError):
            monitor.select_live_machine({'Machines': []})

    def test_alignment_uses_policy_evaluated_not_isolated_authentication(self):
        xml = b'''<feedback><policy_published><domain>tdfrecords.net</domain></policy_published>
        <record><row><count>3</count><policy_evaluated><dkim>pass</dkim><spf>fail</spf></policy_evaluated></row></record>
        <record><row><count>2</count><policy_evaluated><dkim>fail</dkim><spf>fail</spf></policy_evaluated></row>
        <auth_results><spf><domain>unaligned.test</domain><result>pass</result></spf></auth_results></record></feedback>'''
        self.assertEqual(monitor.aggregate_xml(xml), {'messages': 5, 'aligned': 3, 'unaligned': 2})

    def test_refuses_entity_declarations_and_unrelated_domains(self):
        with self.assertRaises(ValueError):
            monitor.aggregate_xml(b'<!DOCTYPE a [<!ENTITY x "unsafe">]><a>&x;</a>')
        self.assertIsNone(monitor.aggregate_xml(b'<feedback><policy_published><domain>tdfrecords.net.evil.test</domain></policy_published></feedback>'))

    def test_decompressed_size_is_bounded(self):
        message = EmailMessage()
        message.set_content('Report')
        message.add_attachment(gzip.compress(b'x' * (monitor.LIMIT + 1)), maintype='application', subtype='gzip', filename='report.xml.gz')
        with self.assertRaises(ValueError):
            monitor.attachment_reports(message)

    def test_dsn_does_not_expose_recipient_or_diagnostic_content(self):
        raw = b'''MIME-Version: 1.0\r
Content-Type: multipart/report; boundary="x"; report-type=delivery-status\r
\r
--x\r
Content-Type: message/delivery-status\r
\r
Reporting-MTA: dns; mail.example.test\r
\r
Final-Recipient: rfc822; private@example.test\r
Action: failed\r
Status: 5.1.1\r
Diagnostic-Code: smtp; 550 private@example.test does not exist\r
\r
--x--\r
'''
        statuses, reports = monitor.summarize_message(raw)
        self.assertEqual(statuses, {'failed:5.1.1': 1})
        self.assertEqual(reports, [])


if __name__ == '__main__':
    unittest.main()
