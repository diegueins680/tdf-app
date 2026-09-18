#!/usr/bin/env python3
"""Read-only DNS and Webador mailbox monitor; no sending or automatic suppression.

Credentials are read from the existing Fly app into memory only. Reports contain
aggregate counts, never message bodies, recipient addresses, or SMTP credentials.
"""
import argparse
import collections
import datetime
import email
import email.policy
import gzip
import imaplib
import io
import json
import os
import pathlib
import re
import shlex
import ssl
import subprocess
import zipfile
import xml.etree.ElementTree as ET

DOMAIN = 'tdfrecords.net'
LIMIT = 2 * 1024 * 1024


def select_live_machine(status):
    eligible = [machine['id'] for machine in status.get('Machines', [])
                if machine.get('state') == 'started'
                and re.fullmatch(r'[0-9a-f]+', machine.get('id', ''))
                and machine.get('config', {}).get('metadata', {}).get('fly_process_group', 'app') == 'app'
                and all(check.get('status') == 'passing' for check in machine.get('checks', []))]
    if not eligible:
        raise RuntimeError('No healthy application Machine available')
    return sorted(eligible)[0]


def aggregate_xml(payload):
    if len(payload) > LIMIT or b'<!DOCTYPE' in payload.upper() or b'<!ENTITY' in payload.upper():
        raise ValueError('Unsafe or oversized aggregate XML')
    root = ET.fromstring(payload)
    domain = root.findtext('policy_published/domain', '').lower()
    if domain != DOMAIN and not domain.endswith('.' + DOMAIN):
        return None
    counts = collections.Counter()
    for record in root.findall('record'):
        count = int(record.findtext('row/count', '0'))
        if count < 0:
            raise ValueError('Invalid aggregate count')
        counts['messages'] += count
        aligned = any(record.findtext('row/policy_evaluated/' + method) == 'pass'
                      for method in ('spf', 'dkim'))
        counts['aligned' if aligned else 'unaligned'] += count
    return dict(counts)


def attachment_reports(message):
    reports = []
    for part in message.walk():
        name = (part.get_filename() or '').lower()
        if not name.endswith(('.xml', '.xml.gz', '.zip', '.gz')):
            continue
        payload = part.get_payload(decode=True) or b''
        if len(payload) > LIMIT:
            raise ValueError('Oversized attachment')
        docs = []
        if name.endswith('.zip'):
            with zipfile.ZipFile(io.BytesIO(payload)) as archive:
                if len(archive.infolist()) > 20:
                    raise ValueError('Too many archive entries')
                for member in archive.infolist():
                    if member.filename.lower().endswith('.xml'):
                        if member.file_size > LIMIT:
                            raise ValueError('Oversized XML')
                        with archive.open(member) as source:
                            docs.append(source.read(LIMIT + 1))
        elif name.endswith('.gz'):
            with gzip.GzipFile(fileobj=io.BytesIO(payload)) as source:
                docs.append(source.read(LIMIT + 1))
        else:
            docs.append(payload)
        for doc in docs:
            result = aggregate_xml(doc)
            if result is not None:
                reports.append(result)
    return reports


def summarize_message(raw):
    message = email.message_from_bytes(raw, policy=email.policy.default)
    statuses = collections.Counter()
    for part in message.walk():
        if part.get_content_type() == 'message/delivery-status':
            for block in part.get_payload():
                status = str(block.get('Status', '')).strip()
                action = str(block.get('Action', '')).strip()
                if status and action:
                    statuses[action + ':' + status] += 1
    return dict(statuses), attachment_reports(message)


def collect(output):
    os.umask(0o077)
    output.mkdir(parents=True, exist_ok=True)
    report = {'observedAt': datetime.datetime.now(datetime.timezone.utc).isoformat(),
              'dns': {}, 'dsnStatuses': {}, 'aggregateReports': [], 'errors': [],
              'limitation': 'Last 14 days, INBOX/Junk, max 200 recent messages per folder; '
                            'no spam complaint feed or inbox-placement inference. Reports are untrusted observations.'}
    for server in ['ns1.openprovider.nl', 'ns2.openprovider.be', 'ns3.openprovider.eu', '1.1.1.1', '8.8.8.8']:
        for name, kind in [(DOMAIN, 'TXT'), ('_dmarc.' + DOMAIN, 'TXT'),
                           ('jouwweb._domainkey.' + DOMAIN, 'TXT'), (DOMAIN, 'MX')]:
            try:
                result = subprocess.run(['dig', '+time=3', '+tries=1', '+noall', '+answer',
                                         '@' + server, name, kind], capture_output=True, text=True, timeout=8)
                report['dns'][server + '/' + name + '/' + kind] = result.stdout.strip()
                if result.returncode or not result.stdout.strip():
                    report['errors'].append('dns_query_failed:' + server + '/' + name)
            except (OSError, subprocess.TimeoutExpired):
                report['errors'].append('dns_query_failed:' + server + '/' + name)
    keys = ['SMTP_USERNAME', 'SMTP_PASSWORD']
    command = 'for k in ' + ' '.join(keys) + '; do printf "%s=" "$k"; printenv "$k"; done'
    try:
        status = subprocess.run(['flyctl', 'status', '-a', 'tdf-hq', '--json'],
                                capture_output=True, text=True, timeout=20)
        if status.returncode:
            raise RuntimeError('Fly status unavailable')
        machine = select_live_machine(json.loads(status.stdout))
        result = subprocess.run(['flyctl', 'ssh', 'console', '-a', 'tdf-hq', '--machine',
                                 machine, '-q', '-C', 'sh -lc ' + shlex.quote(command)],
                                capture_output=True, text=True, timeout=50)
        if result.returncode:
            raise RuntimeError('Fly configuration unavailable')
        config = dict(line.split('=', 1) for line in result.stdout.splitlines()
                      if '=' in line and line.split('=', 1)[0] in keys)
        since = (datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(days=14)).strftime('%d-%b-%Y')
        statuses = collections.Counter()
        with imaplib.IMAP4_SSL('mail.webador.com', 993, ssl_context=ssl.create_default_context(), timeout=20) as mailbox:
            mailbox.login(config['SMTP_USERNAME'], config['SMTP_PASSWORD'])
            for folder in ['INBOX', 'Junk']:
                status, _ = mailbox.select(folder, readonly=True)
                if status != 'OK':
                    report['errors'].append('folder_unavailable:' + folder)
                    continue
                status, found = mailbox.uid('search', None, 'SINCE', since)
                if status != 'OK':
                    raise RuntimeError('Mailbox search failed')
                ids = found[0].split()
                if len(ids) > 200:
                    report['errors'].append('scan_truncated:' + folder)
                for uid in ids[-200:]:
                    status, data = mailbox.uid('fetch', uid, '(BODY.PEEK[]<0.2097153>)')
                    raw = next((x[1] for x in data if isinstance(x, tuple)), b'')
                    if status != 'OK' or len(raw) > LIMIT:
                        report['errors'].append('message_unavailable_or_oversized:' + folder)
                        continue
                    try:
                        failures, aggregates = summarize_message(raw)
                        statuses.update(failures)
                        report['aggregateReports'].extend(aggregates)
                    except (ValueError, ET.ParseError, zipfile.BadZipFile, OSError):
                        report['errors'].append('unparseable_report')
        report['dsnStatuses'] = dict(statuses)
        report['mailboxReadSucceeded'] = True
    except Exception as error:
        report['mailboxReadSucceeded'] = False
        report['errors'].append('mailbox_read_failed:' + type(error).__name__)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%SZ')
    data = json.dumps(report, indent=2) + '\n'
    (output / (stamp + '.json')).write_text(data)
    (output / 'latest.tmp').write_text(data)
    os.replace(output / 'latest.tmp', output / 'latest.json')
    print(json.dumps({'at': report['observedAt'], 'mailboxReadSucceeded': report['mailboxReadSucceeded'],
                      'dsnStatuses': report['dsnStatuses'], 'aggregateReports': len(report['aggregateReports']),
                      'errors': report['errors']}))
    return 1 if report['errors'] else 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    args = parser.parse_args()
    raise SystemExit(collect(args.output))
