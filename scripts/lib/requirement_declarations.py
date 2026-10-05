"""Extract authored identifiers; never promote a declaration to approved policy."""
import hashlib
import re

HEADERS = {'id', 'requirement', 'requirements', 'invariant', 'requisito', 'requisitos'}
# Numeric segments terminate an ID; joined ranges must not become invented IDs.
IDENTIFIER = re.compile(r'^([A-Z][A-Z0-9]*(?:-[A-Z][A-Z0-9]*)*-?\d{2,4})(?:\s+(.+))?$')
REFERENCE_TAIL = re.compile(r'^(?:[-–—/,]|(?:to|through|and)\b)\s*(?:[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*-?\d{2,4}|\d{2,4})(?:\b|$)', re.IGNORECASE)


def cells(line):
    # Markdown table pipes are separators even in code spans, unless escaped.
    if line.startswith(('    ', '\t')):
        return None
    if not line.strip().startswith('|') or not line.strip().endswith('|'):
        return None
    result, current, escaped = [], [], False
    for character in line.strip()[1:-1]:
        if character == '|' and not escaped:
            result.append(''.join(current).strip()); current = []
        else:
            current.append(character)
        escaped = character == '\\' and not escaped
    result.append(''.join(current).strip())
    return result


def extract_declarations(source, relative, authority):
    rows, headers, pending, fence = [], None, None, None
    for number, line in enumerate(source.splitlines(), 1):
        marker = re.match(r'^\s{0,3}(`{3,}|~{3,})', line)
        if marker:
            token = marker[1]
            if fence is None: fence = token
            elif token[0] == fence[0] and len(token) >= len(fence) and not line[marker.end():].strip(): fence = None
            headers, pending = None, None
            continue
        if fence is not None:
            continue
        parts = cells(line)
        if not parts:
            headers, pending = None, None
            continue
        first = parts[0].strip('` ').lower()
        if first in HEADERS:
            pending, headers = parts, None
            continue
        if all(re.fullmatch(r':?-{3,}:?', part) for part in parts):
            if pending is not None and len(parts) == len(pending):
                if len(set(pending)) != len(pending) or any(not item for item in pending):
                    raise ValueError(f'Ambiguous declaration table header: {relative}:{number-1}')
                headers = pending
            pending = None
            continue
        if pending is not None:
            pending, headers = None, None
        if headers is None or len(parts) != len(headers):
            continue
        match = IDENTIFIER.fullmatch(parts[0].replace('`', ''))
        if not match:
            continue  # Ranges/references are not new requirement declarations.
        declared, title = match.groups()
        if title and REFERENCE_TAIL.match(title):
            continue
        columns = dict(zip(headers, parts))
        # A titled first cell in a traceability table states only that title;
        # preserve other cells as claims, never replace it with a model property.
        statement = title or (parts[1] if len(parts) > 1 else '')
        if not statement:
            continue  # An identifier alone does not declare intended behavior.
        rows.append({'id': 'DECL-' + declared + '-' + hashlib.sha256(relative.encode()).hexdigest()[:8].upper(),
                     'declaredId': declared, 'statement': statement, 'source': relative,
                     'line': number, 'sourceSha256': hashlib.sha256(source.encode()).hexdigest(),
                     'status': authority, 'declarationColumns': columns,
                     'deliveryScope': 'Authored domain declaration; source authority and current applicability require reconciliation.',
                     'verification': 'open',
                     'gap': 'Neither adjacent test claims nor a historical PASS establish current implementation conformance.'})
    # Repeated declarations are retained together. Conflicting statements cannot
    # silently disappear through dictionary last-write-wins behavior.
    combined = {}
    for row in rows:
        key = row['id']
        if key not in combined:
            combined[key] = {**row, 'occurrences': [], 'reconciliation': 'unreviewed'}
        current = combined[key]
        current['occurrences'].append({'line': row['line'], 'statement': row['statement'], 'columns': row['declarationColumns']})
        if current['statement'] != row['statement']:
            current['reconciliation'] = 'competing declarations; explicit authority decision required'
    return list(combined.values())
