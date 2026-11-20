from datetime import datetime, timedelta, timezone
from email import policy
from email.message import EmailMessage
import importlib.util
from pathlib import Path
import smtplib
import tempfile
import unittest
from unittest.mock import MagicMock, patch

spec = importlib.util.spec_from_file_location(
    "android_beta_email", Path(__file__).with_name("android-beta-email.py")
)
campaign = importlib.util.module_from_spec(spec)
spec.loader.exec_module(campaign)


class AndroidBetaEmailTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.handle = campaign.open_state(Path(self.temp.name) / "campaign.sqlite")
        self.db = self.handle.__enter__()
        self.addCleanup(self.handle.__exit__, None, None, None)
        self.now = datetime(2026, 11, 16, 20, tzinfo=timezone.utc)
        self.sender = "info@tdfrecords.net"
        self.sent = []
        self.inbox = {}
        self.people = {
            party_id: {"id": party_id, "name": f"Contacto {party_id}", "email": f"tester{party_id}@example.com"}
            for party_id in range(1, 10)
        }

    def plan(self, count=1):
        return campaign.make_plan(
            [
                {"party_id": str(i), "email": self.people[i]["email"], "permission_ref": "test-fixture"}
                for i in range(1, count + 1)
            ],
            self.sender, self.people.get, "sample",
        )

    def activate(self, count=1):
        plan = self.plan(count)
        campaign.activate(self.db, plan, campaign.digest(plan))
        self.set_count(5)
        return plan

    def set_count(self, count):
        with self.db:
            self.db.execute("UPDATE campaign SET console_count = ?", (count,))

    def state(self, party_id=1):
        return self.db.execute("SELECT * FROM contact WHERE id = ?", (party_id,)).fetchone()

    def tick(self, days=0, send=None, poll=None):
        return campaign.tick(
            self.db, self.now + timedelta(days=days), self.people.get,
            poll or (lambda _, queries: {
                recipient["id"]: self.inbox[recipient["id"]]
                for recipient, _, _ in queries if recipient["id"] in self.inbox
            }),
            send or self.sent.append,
        )

    def record(self, event, seconds=0, **options):
        campaign.record_contact(self.db, 1, event, options.get("google_email"),
                                self.now + timedelta(seconds=seconds))

    def test_approval_and_play_count_gate(self):
        plan = self.plan()
        self.assertEqual(self.tick(), (0, 0))
        reviewed = campaign.digest(plan)
        plan["contacts"][0]["messages"]["invite"] += "Texto distinto"
        with self.assertRaises(ValueError):
            campaign.activate(self.db, plan, reviewed)
        campaign.activate(self.db, plan, campaign.digest(plan))
        self.assertEqual(self.tick(), (0, 0))
        self.set_count(12)
        self.assertEqual(self.tick(), (0, 0))
        self.set_count(5)
        self.assertEqual(self.tick(), (1, 0))
        self.assertIn(campaign.PLAY, self.sent[0].get_content())
        self.assertEqual(self.tick(), (0, 0))

    def test_rejects_duplicate_and_updated_audience(self):
        rows = [
            {"party_id": "1", "email": "tester1@example.com", "permission_ref": "test"},
            {"party_id": "2", "email": " TESTER1@example.com ", "permission_ref": "test"},
        ]
        with self.assertRaises(ValueError):
            campaign.make_plan(rows, self.sender, self.people.get, "sample")
        self.activate()
        self.people[1]["email"] = "changed@example.com"
        self.assertEqual(self.tick(), (0, 0))
        self.assertEqual(self.state()["stopped"], "crm-changed")

    def test_reminds_only_once_and_only_without_response(self):
        self.activate(2)
        self.assertEqual(self.tick(), (2, 0))
        self.assertEqual(self.tick(2), (0, 0))
        self.inbox[1] = [("reply", None)]
        self.assertEqual(self.tick(3), (1, 0))
        self.assertEqual(self.tick(4), (0, 0))
        self.assertIsNotNone(self.state()["responded_at"])
        self.assertEqual(str(self.sent[-1]["To"]), self.people[2]["email"])
        self.assertIn(".reminder@", str(self.sent[-1]["Message-ID"]))
        self.assertIn(".invite@", str(self.sent[-1]["In-Reply-To"]))

    def test_request_access_claim_verification_and_installation_are_distinct(self):
        self.activate()
        self.tick()
        self.inbox[1] = [("request", "alpha@example.com")]
        self.assertEqual(self.tick(1), (0, 0))
        self.assertIsNotNone(self.state()["requested_at"])
        self.assertIsNone(self.state()["access_at"])
        self.record("access", seconds=86401)
        self.assertEqual(self.tick(2), (1, 0))
        self.assertIn(".access@", str(self.sent[-1]["Message-ID"]))
        self.inbox[1].append(("joined-claim", None))
        self.assertEqual(self.tick(3), (0, 0))
        self.assertIsNotNone(self.state()["joined_claim_at"])
        self.assertIsNone(self.state()["verified_at"])
        with self.assertRaises(ValueError):
            self.record("installed", seconds=3 * 86400)
        self.record("verified", seconds=3 * 86400)
        self.record("installed", seconds=3 * 86400 + 20)
        self.assertEqual(self.tick(9), (0, 0))
        self.assertEqual(self.tick(10), (1, 0))
        self.assertEqual(self.tick(17), (0, 0))
        self.assertIn(".check-in@", str(self.sent[-1]["Message-ID"]))

    def test_duplicate_google_account_is_not_two_testers(self):
        self.activate(2)
        self.tick()
        self.inbox = {1: [("request", "one@example.com")], 2: [("request", "one@example.com")]}
        self.tick(1)
        self.assertIsNotNone(self.state(1)["requested_at"])
        self.assertIsNone(self.state(2)["requested_at"])

    def test_recruitment_stops_at_seven_verified_new_testers(self):
        self.activate(9)
        for party_id in range(1, 8):
            campaign.record_contact(self.db, party_id, "requested", f"alpha{party_id}@example.com", self.now)
            campaign.record_contact(self.db, party_id, "access", None, self.now)
            campaign.record_contact(self.db, party_id, "verified", None, self.now)
        self.assertEqual(self.tick(), (0, 0))
        campaign.record_contact(self.db, 1, "withdrawn", None, self.now)
        self.assertEqual(self.tick(), (2, 0))

    def test_daily_cap_pause_and_transport_failure(self):
        self.activate(9)
        self.assertEqual(self.tick(), (5, 0))
        self.assertEqual(self.tick(0.9), (0, 0))
        with self.db:
            self.db.execute("UPDATE campaign SET paused = 1")
        self.assertEqual(self.tick(2), (0, 0))
        with self.db:
            self.db.execute("UPDATE campaign SET paused = 0")

        def fail(message):
            self.sent.append(message)
            raise smtplib.SMTPServerDisconnected("ambiguous fixture")

        self.assertEqual(self.tick(2, fail), (0, 1))
        self.assertEqual(self.state(6)["verified_at"], None)
        self.assertEqual(self.tick(2), (3, 0))
        self.assertEqual(self.tick(2), (0, 0))
        self.assertEqual(sum(str(m["To"]) == self.people[6]["email"] for m in self.sent), 1)

    def test_opt_out_and_failed_mailbox_stop_followups(self):
        self.activate()
        self.tick()

        def unavailable(*_):
            raise OSError("IMAP unavailable")

        with self.assertRaises(OSError):
            self.tick(3, poll=unavailable)
        self.inbox[1] = [("opt-out", None), ("request", "alpha@example.com")]
        self.assertEqual(self.tick(3), (0, 0))
        self.assertIsNone(self.state()["requested_at"])
        self.assertEqual(self.state()["stopped"], "opt-out")

    def test_correlates_replies_and_detects_bounces(self):
        plan = self.plan()
        contact = plan["contacts"][0]
        original = campaign.outbound(plan, contact, "invite", self.now, {})
        message_id = str(original["Message-ID"])
        reply = EmailMessage(policy=policy.SMTP)
        reply["From"] = contact["email"]
        reply["Subject"] = "Re: " + contact["subject"]
        reply["In-Reply-To"] = message_id
        reply.set_content("alpha@example.com\n\n> Si prefieres dejar de recibir mensajes, responde BAJA.")
        self.assertEqual(campaign.reply_kind(reply.as_bytes(), contact, None, [message_id]),
                         ("request", "alpha@example.com"))
        reply.set_content("Ya me uní.")
        self.assertEqual(campaign.reply_kind(reply.as_bytes(), contact, None, [message_id]),
                         ("joined-claim", None))
        reply.set_content("BAJA")
        self.assertEqual(campaign.reply_kind(reply.as_bytes(), contact, None, [message_id]),
                         ("opt-out", None))
        reply.replace_header("From", "other@example.com")
        self.assertIsNone(campaign.reply_kind(reply.as_bytes(), contact, None, [message_id]))
        dsn = (
            "Content-Type: multipart/report; report-type=delivery-status; boundary=test\r\n\r\n"
            "--test\r\nContent-Type: message/delivery-status\r\n\r\n"
            "Reporting-MTA: dns; example.com\r\n\r\nFinal-Recipient: rfc822; tester@example.com\r\n"
            "Action: failed\r\nStatus: 5.1.1\r\n\r\n"
            "--test\r\nContent-Type: text/rfc822-headers\r\n\r\nMessage-ID: "
            + message_id + "\r\n--test--\r\n"
        )
        self.assertEqual(campaign.reply_kind(dsn.encode(), contact, None, [message_id]), ("bounce", None))
        self.assertIsNone(campaign.reply_kind(dsn.encode(), contact, None, ["<unrelated@example.com>"]))

    def test_imap_reads_without_marking_when_reply_is_found(self):
        plan = self.activate()
        self.tick()
        reply = EmailMessage()
        reply["From"] = plan["contacts"][0]["email"]
        reply["Subject"] = "Re: " + plan["contacts"][0]["subject"]
        reply.set_content("Ya me uní")
        mailbox = MagicMock()
        mailbox.select.return_value = ("OK", [b"1"])
        mailbox.uid.side_effect = [("OK", [b"100"]), ("OK", [(b"100", reply.as_bytes())])]
        with patch.dict(campaign.os.environ, {
            "TDF_CAMPAIGN_MAIL_USER": self.sender, "TDF_CAMPAIGN_MAIL_PASSWORD": "test"
        }), patch.object(campaign.imaplib, "IMAP4_SSL") as factory:
            factory.return_value.__enter__.return_value = mailbox
            self.tick(1, poll=campaign.mailbox_replies)
        mailbox.select.assert_called_once_with("INBOX", readonly=True)
        self.assertEqual(mailbox.uid.call_args.args, ("fetch", b"100", "(BODY.PEEK[]<0.131073>)"))
        self.assertIsNotNone(self.state()["joined_claim_at"])


if __name__ == "__main__":
    unittest.main()
