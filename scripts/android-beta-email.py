#!/usr/bin/env python3
"""TDF Mobile Android closed-test email campaign."""
import argparse
import contextlib
import csv
from datetime import datetime, timedelta, timezone
from email import policy
from email.message import EmailMessage
from email.parser import BytesParser
from email.utils import parseaddr
import fcntl
import hashlib
from html.parser import HTMLParser
import imaplib
import json
import os
from pathlib import Path
import re
import smtplib
import sqlite3
import ssl
import sys
from urllib.error import HTTPError
from urllib.request import HTTPRedirectHandler, Request, build_opener
import uuid


PLAY = "https://play.google.com/apps/testing/com.tdf.records"
API = "https://api.tdfrecords.net"
GOAL = 7
DAILY_LIMIT = 5
FIRST_REMINDER = timedelta(days=3)
CHECK_IN = timedelta(days=7)
TEST_PERIOD = timedelta(days=14)
SUBJECT = "¿Nos ayudas a probar TDF Mobile en Android?"
SIGNATURE = "\n\nDiego Saá\nTDF Records\n\nSi prefieres dejar de recibir mensajes de esta prueba, responde BAJA."
TEMPLATES = {
    "invite": (
        "Hola, {name}.\n\nEstamos preparando el lanzamiento de TDF Mobile y buscamos "
        "personas con Android que quieran probar la app y contarnos cómo les va. "
        "Si te animas, responde con la dirección de la cuenta que usas en Google Play "
        "para solicitar acceso. Te avisaremos cuando esté habilitada.\n\n"
        "Con esa misma cuenta abre {play} y elige «Unirse» o «Convertirse en tester». "
        "Cuando Google Play indique que ya participas, responde «Ya me uní». "
        "Luego instala la app desde Google Play y mantente en el programa al menos "
        "14 días consecutivos desde que te incorpores."
    ),
    "reminder": (
        "Hola, {name}. ¿Te animas a probar TDF Mobile en Android? "
        "Si quieres sumarte, responde con la dirección de tu cuenta de Google Play "
        "para que podamos habilitarte el acceso. El enlace de la prueba es {play}. "
        "Si tienes dudas, cuéntanos por aquí."
    ),
    "access": (
        "Hola, {name}. Tu cuenta ya está habilitada para la prueba cerrada. "
        "Abre {play} con la cuenta de Google que nos compartiste y elige «Unirse» "
        "o «Convertirse en tester». Cuando Google Play indique que ya participas, "
        "responde «Ya me uní». Después instala TDF Mobile y sigue en el programa "
        "al menos 14 días consecutivos desde tu incorporación. Cuéntanos si tienes "
        "algún problema al unirte o instalarla."
    ),
    "check-in": (
        "Hola, {name}. ¿Cómo te ha ido con TDF Mobile? Si algo falló, cuéntanos qué "
        "hiciste y qué pasó. Recuerda mantenerte en la prueba hasta completar al menos "
        "14 días consecutivos desde tu incorporación. Puedes revisar tu participación "
        "en {play}."
    ),
}
EMAIL = re.compile(r"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)+")
NO_CONTACT = re.compile(
    r"\b(?:no me (?:interesa|escriban)|no (?:quiero|deseo) recibir|d[aeé]r?me de baja)\b", re.I
)
SCHEMA = """
CREATE TABLE IF NOT EXISTS campaign (
  id integer PRIMARY KEY CHECK(id = 1),
  plan text NOT NULL,
  paused integer NOT NULL DEFAULT 0,
  console_count integer CHECK(console_count BETWEEN 0 AND 1000000)
);
CREATE TABLE IF NOT EXISTS contact (
  id integer PRIMARY KEY,
  responded_at text,
  requested_at text,
  google_email text UNIQUE,
  access_at text,
  joined_claim_at text,
  verified_at text,
  installation_at text,
  stopped text
);
CREATE TABLE IF NOT EXISTS delivery (
  contact_id integer NOT NULL REFERENCES contact(id),
  stage text NOT NULL,
  message_id text NOT NULL UNIQUE,
  attempted_at text NOT NULL,
  status text NOT NULL CHECK(status IN ('uncertain', 'accepted', 'rejected')),
  PRIMARY KEY(contact_id, stage)
);
"""


def utcnow():
    return datetime.now(timezone.utc)


def stamp(moment):
    return moment.isoformat(timespec="seconds")


def address(value):
    value = (value or "").strip().lower()
    if not EMAIL.fullmatch(value):
        raise ValueError("Se requiere una dirección de correo válida.")
    return value


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


def crm_contact(party_id):
    request = Request(
        f"{API}/parties/{party_id}",
        headers={
            "Accept": "application/json",
            "Authorization": "Bearer " + os.environ["TDF_CRM_TOKEN"],
        },
    )
    try:
        with build_opener(NoRedirect()).open(request, timeout=15) as response:
            party = json.load(response)
    except HTTPError as error:
        if error.code == 404:
            return None
        raise
    if party["partyId"] != party_id or party["isOrg"] or not party["primaryEmail"]:
        return None
    return {
        "id": party_id,
        "name": " ".join(party["displayName"].split()) or "qué gusto saludarte",
        "email": address(party["primaryEmail"]),
    }


def make_plan(rows, sender, lookup, batch_id):
    sender = address(sender)
    if not sender.endswith("@tdfrecords.net"):
        raise ValueError("Usa un buzón de TDF Records.")
    if not rows:
        raise ValueError("Incluye al menos un contacto en la cohorte.")
    contacts = []
    seen_ids = set()
    seen_emails = set()
    for row in rows:
        party_id = int(row["party_id"])
        expected_email = address(row["email"])
        evidence = row["permission_ref"].strip()
        if party_id < 1 or not evidence or party_id in seen_ids or expected_email in seen_emails:
            raise ValueError("Cada fila requiere un ID y correo únicos y una referencia de autorización.")
        contact = lookup(party_id)
        if contact is None or contact["email"] != expected_email:
            raise ValueError(f"El contacto CRM {party_id} cambió o no es elegible.")
        contact = dict(contact, permission_ref=evidence)
        contact["tag"] = f"TDF-A24-{batch_id}-{party_id}"
        contact["subject"] = f"{SUBJECT} [{contact['tag']}]"
        contact["messages"] = {
            stage: template.format(name=contact["name"], play=PLAY) + SIGNATURE
            for stage, template in TEMPLATES.items()
        }
        contacts.append(contact)
        seen_ids.add(party_id)
        seen_emails.add(expected_email)
    return {
        "sender": sender,
        "contacts": contacts,
        "goal": GOAL,
        "daily_limit": DAILY_LIMIT,
        "reminder_days": FIRST_REMINDER.days,
        "check_in_days": CHECK_IN.days,
        "test_days": TEST_PERIOD.days,
        "distribution": PLAY,
    }


def canonical(plan):
    return json.dumps(plan, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def digest(plan):
    return hashlib.sha256(canonical(plan).encode()).hexdigest()


@contextlib.contextmanager
def open_state(path):
    lock_path = path.with_suffix(path.suffix + ".lock")
    with lock_path.open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        with contextlib.closing(sqlite3.connect(path)) as db:
            db.row_factory = sqlite3.Row
            db.execute("PRAGMA foreign_keys = ON")
            db.executescript(SCHEMA)
            yield db


def activate(db, plan, approved_hash):
    if digest(plan) != approved_hash:
        raise ValueError("La huella no coincide con la vista previa aprobada.")
    with db:
        if db.execute("SELECT 1 FROM campaign").fetchone():
            raise ValueError("La campaña ya está preparada en este registro.")
        db.execute("INSERT INTO campaign(id, plan) VALUES (1, ?)", (canonical(plan),))
        db.executemany("INSERT INTO contact(id) VALUES (?)", [(x["id"],) for x in plan["contacts"]])


def outbound(plan, recipient, stage, now, deliveries):
    message = EmailMessage()
    message["From"] = plan["sender"]
    message["Reply-To"] = plan["sender"]
    message["To"] = recipient["email"]
    message["Subject"] = recipient["subject"]
    message["Date"] = now
    message["Message-ID"] = f"<{recipient['tag']}.{stage}@tdfrecords.net>"
    if "invite" in deliveries:
        original = deliveries["invite"]["message_id"]
        message["In-Reply-To"] = original
        message["References"] = original
    message.set_content(recipient["messages"][stage])
    return message


def smtp_send(message):
    host = os.getenv("TDF_CAMPAIGN_SMTP_HOST", "mail.webador.com")
    username = os.environ["TDF_CAMPAIGN_MAIL_USER"]
    password = os.environ["TDF_CAMPAIGN_MAIL_PASSWORD"]
    if address(username) != str(message["From"]):
        raise ValueError("El buzón SMTP no coincide con el remitente aprobado.")
    tls = ssl.create_default_context()
    with smtplib.SMTP_SSL(host, 465, timeout=20, context=tls) as smtp:
        smtp.login(username, password)
        smtp.send_message(message)


class VisibleReplyText(HTMLParser):
    """The text a person sees in an HTML reply, without the quoted message."""

    BLOCKS = {"address", "blockquote", "br", "div", "h1", "h2", "h3", "h4", "h5", "h6", "hr",
              "li", "ol", "p", "pre", "table", "td", "th", "tr", "ul"}
    HIDDEN = {"head", "script", "style", "title"}

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.parts = []
        self.hidden = 0
        self.quoted = False

    def handle_starttag(self, tag, attrs):
        if self.quoted:
            return
        classes = " ".join(value or "" for key, value in attrs if key == "class")
        if tag == "blockquote" or "gmail_quote" in classes or "moz-cite-prefix" in classes:
            self.quoted = True
        elif tag in self.HIDDEN:
            self.hidden += 1
        elif tag in self.BLOCKS:
            self.parts.append("\n")

    def handle_endtag(self, tag):
        if tag in self.HIDDEN:
            self.hidden = max(0, self.hidden - 1)
        elif tag in self.BLOCKS and not self.quoted:
            self.parts.append("\n")

    def handle_data(self, data):
        if not self.quoted and not self.hidden:
            self.parts.append(data)


def html_visible_text(markup):
    parser = VisibleReplyText()
    parser.feed(markup)
    parser.close()
    return "".join(parts.replace("\xa0", " ") for parts in parser.parts)


def reply_kind(raw, recipient, google_email, message_ids):
    message = BytesParser(policy=policy.default).parsebytes(raw)
    if message.get_param("report-type") == "delivery-status":
        for part in message.walk():
            if part.get_content_type() != "message/delivery-status":
                continue
            if any(str(block.get("Action", "")).lower() == "failed" for block in part.get_payload()):
                if any(mid.encode("ascii") in raw for mid in message_ids):
                    return ("bounce", None)
        return None
    if str(message.get("Auto-Submitted", "no")).lower() != "no":
        return None
    sender = parseaddr(str(message.get("From", "")))[1].lower()
    if sender not in {recipient["email"], google_email}:
        return None
    thread_headers = " ".join(str(message.get(field, "")) for field in ("In-Reply-To", "References"))
    if not (any(mid in thread_headers for mid in message_ids)
            or f"[{recipient['tag']}]" in str(message.get("Subject", ""))):
        return None
    body = message.get_body(preferencelist=("plain", "html"))
    content = body.get_content() if body is not None else ""
    if body is not None and isinstance(content, str):
        if body.get_content_type() == "text/html":
            # An HTML-only reply carries the opt-out word inside markup.
            content = html_visible_text(content)
        visible_lines = []
        for line in content.splitlines():
            stripped = line.strip()
            if re.match(r"^(?:>|El .+ escribió:|On .+ wrote:|-----Original Message-----)", stripped):
                break
            if stripped:
                visible_lines.append(stripped)
        first = visible_lines[0] if visible_lines else ""
        command = first.strip(" .!¡").casefold()
        if command in {"baja", "salir", "stop", "unsubscribe"} or NO_CONTACT.search(
            " ".join(visible_lines[:3])
        ):
            return ("opt-out", None)
        if body.get_content_type() == "text/plain" and first:
            if re.fullmatch(r"[¡! ]*ya me un[ií][.!¡ ]*", first, re.I):
                return ("joined-claim", None)
            if EMAIL.fullmatch(first):
                return ("request", address(first))
    return ("reply", None)


def mailbox_replies(sender, queries):
    username = os.environ["TDF_CAMPAIGN_MAIL_USER"]
    if address(username) != sender:
        raise ValueError("El buzón IMAP no coincide con el remitente aprobado.")
    if not queries:
        return {}
    results = {}
    host = os.getenv("TDF_CAMPAIGN_IMAP_HOST", "mail.webador.com")
    folder = os.getenv("TDF_CAMPAIGN_IMAP_FOLDER", "INBOX")
    with imaplib.IMAP4_SSL(host, 993, ssl_context=ssl.create_default_context(), timeout=20) as mailbox:
        mailbox.login(username, os.environ["TDF_CAMPAIGN_MAIL_PASSWORD"])
        if mailbox.select(folder, readonly=True)[0] != "OK":
            raise RuntimeError("No se pudo leer el buzón de respuestas.")
        for recipient, google_email, deliveries in queries:
            since = min(delivery["attempted_at"] for delivery in deliveries.values())
            since = (datetime.fromisoformat(since) - timedelta(days=1)).strftime("%d-%b-%Y")
            status, matches = mailbox.uid("search", None, "SINCE", since, "TEXT", recipient["tag"])
            if status != "OK":
                raise RuntimeError("Falló la búsqueda de respuestas de la campaña.")
            message_ids = [delivery["message_id"] for delivery in deliveries.values()]
            replies = []
            for uid in matches[0].split():
                status, data = mailbox.uid("fetch", uid, "(BODY.PEEK[]<0.131073>)")
                raw = next((part[1] for part in data if isinstance(part, tuple)), b"")
                if status != "OK" or not raw:
                    raise RuntimeError("No se pudo leer una respuesta de la campaña.")
                reply = reply_kind(raw, recipient, google_email, message_ids)
                if reply:
                    replies.append(reply)
            if replies:
                results[recipient["id"]] = replies
    return results


def apply_replies(db, party_id, replies, now):
    kinds = {kind for kind, _ in replies}
    if "opt-out" in kinds or "bounce" in kinds:
        reason = "opt-out" if "opt-out" in kinds else "bounce"
        db.execute("UPDATE contact SET stopped = ? WHERE id = ?", (reason, party_id))
        return
    db.execute("UPDATE contact SET responded_at = COALESCE(responded_at, ?) WHERE id = ?",
               (stamp(now), party_id))
    if "joined-claim" in kinds:
        db.execute("UPDATE contact SET joined_claim_at = COALESCE(joined_claim_at, ?) WHERE id = ?",
                   (stamp(now), party_id))
    accounts = {account for kind, account in replies if kind == "request"}
    if len(accounts) == 1:
        db.execute(
            "UPDATE OR IGNORE contact SET requested_at = ?, google_email = ? "
            "WHERE id = ? AND requested_at IS NULL",
            (stamp(now), accounts.pop(), party_id),
        )


def next_stage(row, deliveries, now, plan, recruiting):
    if row["stopped"] or any(x["status"] != "accepted" for x in deliveries.values()):
        return None
    if row["verified_at"]:
        elapsed = now - datetime.fromisoformat(row["verified_at"])
        if "check-in" not in deliveries and timedelta(days=plan["check_in_days"]) <= elapsed < timedelta(days=plan["test_days"]):
            return "check-in"
        return None
    if row["access_at"] and "access" not in deliveries:
        return "access"
    if not recruiting or row["responded_at"] or row["requested_at"] or row["access_at"]:
        return None
    if "invite" not in deliveries:
        return "invite"
    elapsed = now - datetime.fromisoformat(deliveries["invite"]["attempted_at"])
    if "reminder" not in deliveries and elapsed >= timedelta(days=plan["reminder_days"]):
        return "reminder"
    return None


def tick(db, now, lookup, poll, send):
    campaign = db.execute("SELECT * FROM campaign").fetchone()
    if campaign is None or campaign["paused"]:
        return 0, 0
    plan = json.loads(campaign["plan"])
    by_contact = {recipient["id"]: {} for recipient in plan["contacts"]}
    for delivery in db.execute("SELECT * FROM delivery"):
        by_contact[delivery["contact_id"]][delivery["stage"]] = delivery
    rows = {row["id"]: row for row in db.execute("SELECT * FROM contact")}
    queries = [
        (recipient, rows[recipient["id"]]["google_email"], by_contact[recipient["id"]])
        for recipient in plan["contacts"]
        if by_contact[recipient["id"]] and not rows[recipient["id"]]["stopped"]
    ]
    replies = poll(plan["sender"], queries)
    with db:
        for party_id, observations in replies.items():
            apply_replies(db, party_id, observations, now)
    rows = {row["id"]: row for row in db.execute("SELECT * FROM contact")}
    verified = sum(bool(row["verified_at"] and row["stopped"] != "withdrawn") for row in rows.values())
    console_count = campaign["console_count"]
    recruiting = (
        console_count is not None
        and console_count < 12
        and verified < max(plan["goal"], 12 - console_count)
    )
    since = stamp(now - timedelta(days=1))
    attempted = db.execute("SELECT count(*) FROM delivery WHERE attempted_at >= ?", (since,)).fetchone()[0]
    accepted = problems = 0
    # Handle active testers and access confirmations before recruiting new participants.
    priority = {"check-in": 0, "access": 1, "reminder": 2, "invite": 3}
    messages = []
    for recipient in plan["contacts"]:
        party_id = recipient["id"]
        stage = next_stage(rows[party_id], by_contact[party_id], now, plan, recruiting)
        if stage:
            messages.append((priority[stage], party_id, stage, recipient))
    for _, party_id, stage, recipient in sorted(messages):
        if attempted >= plan["daily_limit"]:
            break
        live = lookup(party_id)
        if live is None or live["email"] != recipient["email"]:
            with db:
                db.execute("UPDATE contact SET stopped = 'crm-changed' WHERE id = ?", (party_id,))
            continue
        message = outbound(plan, recipient, stage, now, by_contact[party_id])
        # Commit the attempt before SMTP. After a crash or uncertain outcome, do not retry.
        with db:
            db.execute(
                "INSERT INTO delivery VALUES (?, ?, ?, ?, 'uncertain')",
                (party_id, stage, str(message["Message-ID"]), stamp(now)),
            )
        attempted += 1
        try:
            send(message)
        except smtplib.SMTPRecipientsRefused:
            result = "rejected"
        except (OSError, smtplib.SMTPException, ValueError):
            # Stop the tick on a transport/config failure before risking the rest of the cohort.
            return accepted, problems + 1
        else:
            result = "accepted"
        with db:
            db.execute("UPDATE delivery SET status = ? WHERE contact_id = ? AND stage = ?",
                       (result, party_id, stage))
        accepted += result == "accepted"
        problems += result != "accepted"
    return accepted, problems


def record_contact(db, party_id, event, google_email, now, verified_at=None):
    row = db.execute("SELECT * FROM contact WHERE id = ?", (party_id,)).fetchone()
    if row is None:
        raise ValueError("Contacto fuera de la cohorte aprobada.")
    with db:
        if event in {"stop", "withdrawn"}:
            db.execute("UPDATE contact SET stopped = ? WHERE id = ?", (event, party_id))
        elif row["stopped"]:
            raise ValueError("El seguimiento del contacto está detenido.")
        elif event == "requested":
            new_email = address(google_email)
            if row["google_email"] and row["google_email"] != new_email:
                raise ValueError("Ya existe otra cuenta de Google registrada para este contacto.")
            db.execute(
                "UPDATE contact SET requested_at = COALESCE(requested_at, ?), google_email = ? WHERE id = ?",
                (stamp(now), new_email, party_id),
            )
        elif event == "access" and row["requested_at"]:
            db.execute("UPDATE contact SET access_at = COALESCE(access_at, ?) WHERE id = ?", (stamp(now), party_id))
        elif event == "verified" and row["access_at"]:
            moment = datetime.fromisoformat(verified_at) if verified_at else now
            if moment.tzinfo is None or not datetime.fromisoformat(row["access_at"]) <= moment <= now:
                raise ValueError("La fecha de incorporación debe incluir zona y estar entre habilitación y ahora.")
            db.execute("UPDATE contact SET verified_at = COALESCE(verified_at, ?) WHERE id = ?",
                       (stamp(moment.astimezone(timezone.utc)), party_id))
        elif event == "installed" and row["verified_at"]:
            db.execute("UPDATE contact SET installation_at = COALESCE(installation_at, ?) WHERE id = ?",
                       (stamp(now), party_id))
        else:
            raise ValueError("Registra primero la etapa anterior del contacto.")


def status(db):
    campaign = db.execute("SELECT paused, console_count FROM campaign").fetchone()
    if campaign is None:
        return {"active": False}
    return {
        "active": not campaign["paused"],
        "play_console_count": campaign["console_count"],
        "contacts": [dict(row) for row in db.execute(
            "SELECT id, responded_at, requested_at, access_at, joined_claim_at, verified_at, installation_at, stopped FROM contact ORDER BY id"
        )],
        "deliveries": [dict(row) for row in db.execute("SELECT * FROM delivery ORDER BY attempted_at, contact_id")],
    }


def main():
    os.umask(0o077)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", type=Path, required=True)
    commands = parser.add_subparsers(dest="command", required=True)
    preview = commands.add_parser("preview")
    preview.add_argument("cohort", type=Path)
    preview.add_argument("output", type=Path)
    preview.add_argument("--sender", required=True)
    approval = commands.add_parser("activate")
    approval.add_argument("preview", type=Path)
    approval.add_argument("--approved-sha256", required=True)
    for command in ("tick", "status", "pause", "resume"):
        commands.add_parser(command)
    console = commands.add_parser("console-count")
    console.add_argument("count", type=int, choices=range(0, 1001), metavar="COUNT")
    record = commands.add_parser("record")
    record.add_argument("party_id", type=int)
    record.add_argument("event", choices=("requested", "access", "verified", "installed", "stop", "withdrawn"))
    record.add_argument("--google-email")
    record.add_argument("--verified-at")
    args = parser.parse_args()
    if args.command == "preview":
        with args.cohort.open(newline="", encoding="utf-8-sig") as cohort:
            plan = make_plan(list(csv.DictReader(cohort)), args.sender, crm_contact, uuid.uuid4().hex[:10])
        with args.output.open("x", encoding="utf-8") as output:
            json.dump(plan, output, indent=2, ensure_ascii=False)
        print(digest(plan))
        return 0
    with open_state(args.state) as db:
        if args.command == "activate":
            activate(db, json.loads(args.preview.read_text()), args.approved_sha256)
        elif args.command == "tick":
            accepted, problems = tick(db, utcnow(), crm_contact, mailbox_replies, smtp_send)
            print(json.dumps({"smtp_accepted": accepted, "problems": problems}))
            return int(problems > 0)
        elif args.command == "status":
            print(json.dumps(status(db), indent=2))
        elif args.command == "record":
            record_contact(db, args.party_id, args.event, args.google_email, utcnow(), args.verified_at)
        elif args.command == "console-count":
            with db:
                if not db.execute("UPDATE campaign SET console_count = ? WHERE id = 1", (args.count,)).rowcount:
                    raise ValueError("Primero activa la campaña aprobada.")
        else:
            with db:
                db.execute("UPDATE campaign SET paused = ? WHERE id = 1", (args.command == "pause",))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, KeyError, RuntimeError, sqlite3.Error, imaplib.IMAP4.error, smtplib.SMTPException) as error:
        # Transport and database errors may contain addresses, paths or provider responses.
        detail = str(error) if type(error) is ValueError else type(error).__name__
        print(f"Campaña detenida. {detail}", file=sys.stderr)
        raise SystemExit(1)
