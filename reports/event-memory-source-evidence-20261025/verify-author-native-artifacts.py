#!/usr/bin/env python3
"""Inspect isolated author-action test builds without installing or executing them.

This checks packaging and source receipts, not a device, production signature,
App Store release, API authorization, or a complete user journey.
"""
import argparse
import hashlib
import json
from pathlib import Path
import plistlib
import re
import tarfile
import zipfile


RUNTIME = '1.0.1-interactions-audio.1'
MARKERS = ['emeExpectedUpdatedAt', 'Confirmar eliminación', 'Cancelar eliminación', 'Editar recuerdo']
FIXTURE_API = 'http://127.0.0.1:18128'


def sha(path):
    with path.open('rb') as stream:
        result = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(chunk)
        return result.hexdigest()


def contains(data, text):
    return any(text.encode(encoding) in data for encoding in ('utf-8', 'utf-16le', 'latin1'))


def verify_js(data):
    markers = {text: contains(data, text) for text in MARKERS}
    if not all(markers.values()):
        raise ValueError('Missing author-action JavaScript markers: ' + repr(markers))
    if not contains(data, FIXTURE_API):
        raise ValueError('Expected the isolated loopback API in the bundled JavaScript')
    return {'bytes': len(data), 'sha256': hashlib.sha256(data).hexdigest(),
            'authorActionMarkers': markers, 'loopbackApiPresent': True}


def one(paths, label):
    values = list(paths)
    if len(values) != 1:
        raise ValueError('Expected exactly one ' + label)
    return values[0]


def android(directory, checkout_log, head):
    # Bind to the actual checked-out revision printed by checkout, rather than
    # treating the artifact name or workflow's requested ref as source proof.
    lines = checkout_log.read_text().splitlines()
    log_indexes = [i for i, line in enumerate(lines) if 'git log -1 --format=%H' in line]
    if len(log_indexes) != 1 or not lines[log_indexes[0] + 1].endswith(' ' + head):
        raise ValueError('Android checkout log does not confirm the expected source head')
    apk = one(directory.rglob('*.apk'), 'APK')
    with zipfile.ZipFile(apk) as archive:
        names = archive.namelist()
        bundle_name = one((n for n in names if n.endswith('/index.android.bundle')), 'Android JS bundle')
        javascript = verify_js(archive.read(bundle_name))
        dex = [n for n in names if n.endswith('.dex')
               and b'expo/modules/documentpicker/DocumentPickerModule' in archive.read(n)]
        if not dex or not contains(archive.read('resources.arsc'), RUNTIME):
            raise ValueError('Missing native DocumentPicker or expected runtime resource')
        abis = sorted({n.split('/')[1] for n in names if n.startswith('lib/') and n.endswith('.so')})
        if abis != ['x86_64']:
            raise ValueError('Expected the isolated x86_64 emulator APK')
    return {'sourceHead': head, 'checkoutLogSha256': sha(checkout_log), 'apkBytes': apk.stat().st_size,
            'apkSha256': sha(apk), 'javascript': javascript, 'documentPickerDex': dex,
            'runtimeInCompiledResources': RUNTIME, 'abis': abis,
            'fixtureChanges': ['AndroidManifest: cleartext loopback permitted; OTA disabled'],
            'signing': 'Repository debug test key configured by workflow; not production signature verification',
            'scope': 'Compiled test APK inspected; not installed, executed or distributed to a store'}


def ios(directory, head):
    source = one(directory.rglob('tdf-interaction-source.txt'), 'iOS source receipt')
    if source.read_text().strip() != head:
        raise ValueError('iOS source receipt does not match the expected source head')
    archive_path = one(directory.rglob('tdf-interaction-simulator.tgz'), 'simulator archive')
    with tarfile.open(archive_path, 'r:gz') as archive:
        members = archive.getmembers()
        if any(Path(m.name).is_absolute() or '..' in Path(m.name).parts for m in members):
            raise ValueError('Unsafe simulator archive path')

        def read(name):
            member = one((m for m in members if m.name == name), name)
            if not member.isfile():
                raise ValueError('Expected a regular archive member: ' + name)
            return archive.extractfile(member).read()

        info = plistlib.loads(read('TDFRecords.app/Info.plist'))
        if info.get('CFBundleIdentifier') != 'com.tdfrecords.app' or info.get('CFBundleSupportedPlatforms') != ['iPhoneSimulator']:
            raise ValueError('Unexpected simulator application identity/platform')
        executable = info['CFBundleExecutable']
        if '/' in executable or executable in ('.', '..'):
            raise ValueError('Unsafe native executable name')
        binary = read('TDFRecords.app/' + executable)
        if b'DocumentPickerModule' not in binary:
            raise ValueError('Missing native DocumentPicker module')
        javascript = verify_js(read('TDFRecords.app/main.jsbundle'))
        updates = plistlib.loads(read('TDFRecords.app/Expo.plist'))
        if updates.get('EXUpdatesRuntimeVersion') != RUNTIME or updates.get('EXUpdatesEnabled') is not False:
            raise ValueError('Unexpected simulator runtime or OTA configuration')
    return {'sourceHead': head, 'archiveBytes': archive_path.stat().st_size, 'archiveSha256': sha(archive_path),
            'bundleIdentifier': info['CFBundleIdentifier'], 'platforms': info['CFBundleSupportedPlatforms'],
            'nativeBinarySha256': hashlib.sha256(binary).hexdigest(), 'documentPickerModulePresent': True,
            'javascript': javascript, 'runtime': RUNTIME, 'otaEnabled': False,
            'fixtureChanges': ['Test application/keychain identity; OTA disabled'],
            'scope': 'Compiled simulator app inspected; no physical device, production signing or store distribution'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--head', required=True)
    parser.add_argument('--android-dir', type=Path)
    parser.add_argument('--android-checkout-log', type=Path)
    parser.add_argument('--ios-dir', type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    if not re.fullmatch('[0-9a-f]{40}', args.head) or not (args.android_dir or args.ios_dir):
        parser.error('Expected a full source SHA and at least one artifact directory')
    if args.android_dir and not args.android_checkout_log:
        parser.error('--android-checkout-log is required with --android-dir')
    result = {'sourceHead': args.head}
    if args.android_dir:
        result['android'] = android(args.android_dir, args.android_checkout_log, args.head)
    if args.ios_dir:
        result['iosSimulator'] = ios(args.ios_dir, args.head)
    args.output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n')
    print(json.dumps({'verified': list(result)[1:], 'sourceHead': args.head}))


if __name__ == '__main__':
    main()
