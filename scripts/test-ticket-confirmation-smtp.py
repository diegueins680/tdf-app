#!/usr/bin/env python3
"""Exercise the actual worker against loopback SMTP only; never send externally."""
import email
import email.policy
import os
from pathlib import Path
import socketserver
import subprocess
import threading


class Sink(socketserver.StreamRequestHandler):
    def reply(self, line):
        self.wfile.write(line.encode() + b"\r\n")
        self.wfile.flush()

    def handle(self):
        self.reply("220 localhost synthetic SMTP")
        while line := self.rfile.readline():
            command = line.decode().strip()
            verb = command.split(" ", 1)[0].upper()
            if verb in ("EHLO", "HELO"):
                self.reply("250-localhost")
                self.reply("250 AUTH LOGIN PLAIN")
            elif verb == "AUTH":
                if "LOGIN" in command:
                    self.reply("334 VXNlcm5hbWU6")
                    self.rfile.readline()
                    self.reply("334 UGFzc3dvcmQ6")
                    self.rfile.readline()
                self.reply("235 authenticated synthetic session")
            elif verb == "MAIL":
                with self.server.lock:
                    first = not self.server.refused
                    self.server.refused = True
                if first:
                    self.reply("421 synthetic temporary refusal")
                    return
                self.reply("250 sender accepted")
            elif verb == "RCPT":
                assert "buyer@example.invalid" in command, "Unexpected test recipient"
                self.reply("250 recipient accepted")
            elif verb == "DATA":
                self.reply("354 end with dot")
                data = []
                while (part := self.rfile.readline()) not in (b".\r\n", b".\n", b""):
                    data.append(part[1:] if part.startswith(b"..") else part)
                with self.server.lock:
                    self.server.messages.append(b"".join(data))
                self.reply("250 synthetic message accepted")
            elif verb == "QUIT":
                self.reply("221 bye")
                return
            elif verb in ("RSET", "NOOP"):
                self.reply("250 ok")
            else:
                self.reply("500 unsupported synthetic command")


root = Path(__file__).resolve().parents[1]
with socketserver.ThreadingTCPServer(("127.0.0.1", 0), Sink) as server:
    server.daemon_threads = True
    server.messages = []
    server.refused = False
    server.lock = threading.Lock()
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    env = dict(os.environ, TICKET_CONFIRMATION_SMTP_PORT=str(server.server_address[1]))
    try:
        result = subprocess.run(["stack", "exec", "--", "runghc", "-Wall", "-isrc", "test/TicketConfirmationSmtpMain.hs"],
                                cwd=root / "tdf-hq", env=env, capture_output=True, text=True, timeout=900)
        if result.returncode:
            print(result.stdout, result.stderr)
            result.check_returncode()
        diagnostics = result.stdout + result.stderr
        assert "buyer@example.invalid" not in diagnostics
        assert "TDF-ABCDEF012345" not in diagnostics and "TDF-ABCDEF012346" not in diagnostics
        print(result.stdout.strip())
        assert server.refused and len(server.messages) == 1
        message = email.message_from_bytes(server.messages[0], policy=email.policy.default)
        assert message["Message-ID"] and message["Date"]
        body = message.get_body(preferencelist=("plain",)).get_content()
        assert "https://www.tdfrecords.net/eventos/1" in body
        assert "https://www.tdfrecords.net/app" in body
        assert "No necesitas instalar una app" in body
        assert "TDF-ABCDEF012345" in body and "TDF-ABCDEF012346" in body
        assert "tdf://" not in body
        html = message.get_body(preferencelist=("html",)).get_content()
        assert "<script>alert(1)</script>" not in html
        assert "&lt;script&gt;" in html
        print("Captured exactly one SMTP message after retry; headers, admission codes and web links verified.")
    finally:
        server.shutdown()
        thread.join(timeout=5)
