"""Loopback-only synthetic inbox. No relaying; never logs message contents."""
import asyncio, base64, email, email.policy, json, os, re, sys
from pathlib import Path

out = Path(sys.argv[1])
counter = 0

async def client(reader, writer):
    global counter
    recipients = []
    authenticated = False
    def reply(value): writer.write((value + '\r\n').encode())
    reply('220 synthetic.persona.test ESMTP')
    try:
        while line := await asyncio.wait_for(reader.readline(), 30):
            text = line.decode().rstrip('\r\n')
            verb, _, argument = text.partition(' ')
            verb = verb.upper()
            if verb in ('EHLO', 'HELO'):
                reply('250-synthetic.persona.test\r\n250-AUTH LOGIN PLAIN\r\n250 SIZE 1048576')
            elif verb == 'AUTH':
                kind, _, encoded = argument.partition(' ')
                if kind.upper() == 'PLAIN':
                    if not encoded:
                        reply('334 '); await writer.drain()
                        encoded = (await reader.readline()).decode().strip()
                    parts = base64.b64decode(encoded).split(b'\0')
                    authenticated = parts[-2:] == [b'synthetic', b'synthetic-only']
                elif kind.upper() == 'LOGIN':
                    reply('334 VXNlcm5hbWU6'); await writer.drain()
                    username = base64.b64decode((await reader.readline()).strip())
                    reply('334 UGFzc3dvcmQ6'); await writer.drain()
                    password = base64.b64decode((await reader.readline()).strip())
                    authenticated = username == b'synthetic' and password == b'synthetic-only'
                reply('235 Authentication successful' if authenticated else '535 Authentication failed')
            elif verb == 'MAIL':
                recipients = []
                reply('250 OK' if authenticated else '530 Authentication required')
            elif verb == 'RCPT':
                address = argument.removeprefix('TO:').strip().strip('<>')
                if authenticated and re.fullmatch(r'st001-[a-z0-9-]+@persona\.test', address):
                    recipients.append(address); reply('250 OK')
                else: reply('550 Synthetic namespace only')
            elif verb == 'DATA' and authenticated and recipients:
                reply('354 End with dot'); await writer.drain()
                chunks = []; size = 0
                while True:
                    chunk = await asyncio.wait_for(reader.readline(), 30)
                    if chunk == b'.\r\n': break
                    if not chunk: raise ValueError('incomplete DATA')
                    size += len(chunk)
                    if size > 1048576: raise ValueError('message too large')
                    chunks.append(chunk[1:] if chunk.startswith(b'..') else chunk)
                message = email.message_from_bytes(b''.join(chunks), policy=email.policy.default)
                parts = [p.get_content() for p in message.walk() if p.get_content_type() in ('text/plain', 'text/html')]
                links = re.findall(r'http://127\.0\.0\.1:4295/reset\?[^\s<>"\']+', '\n'.join(parts))
                counter += 1
                value = {'recipient': recipients, 'subject': str(message['subject']), 'links': links}
                target = out / f'mail-{counter}.json'
                fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
                with os.fdopen(fd, 'w') as file: json.dump(value, file)
                reply('250 Accepted into disposable synthetic inbox')
            elif verb == 'QUIT': reply('221 Bye'); await writer.drain(); break
            elif verb in ('NOOP', 'RSET'): reply('250 OK')
            else: reply('502 Unsupported command')
            await writer.drain()
    except (OSError, ValueError, asyncio.TimeoutError):
        pass
    finally:
        writer.close()
        await writer.wait_closed()

async def main():
    server = await asyncio.start_server(client, '127.0.0.1', 18860, limit=1048576)
    async with server: await server.serve_forever()

asyncio.run(main())
