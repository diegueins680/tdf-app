import importlib.util
import pathlib
import unittest
from email.message import EmailMessage
import gzip

spec = importlib.util.spec_from_file_location('monitor', pathlib.Path(__file__).with_name('mail-deliverability-monitor.py'))
monitor = importlib.util.module_from_spec(spec)
spec.loader.exec_module(monitor)


class MonitorTests(unittest.TestCase):
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
