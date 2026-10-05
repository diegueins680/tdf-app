import assert from 'node:assert/strict';
import test from 'node:test';
import { compiledApiSurface, compareApiSurface } from '../lib/compiled-api-surface.mjs';

const node = (module, name, ...args) => ({ module, name, args });
const symbol = text => node('GHC.TypeLits', JSON.stringify(text));
const number = text => node('GHC.TypeLits', String(text));
const list = (...xs) => xs.length ? node('GHC.Types', "':", xs[0], list(...xs.slice(1))) : node('GHC.Types', "'[]");
const textType = node('Data.Text.Internal', 'Text');
const modifier = name => node('Servant.API.Modifiers', name);
const method = name => node('Network.HTTP.Types.Method', `'${name}`);
const verb = (name = 'GET', status = 200) => node('Servant.API.Verbs', 'Verb', method(name), number(status), list(node('Servant.API.ContentTypes', 'JSON')), textType);
const sub = (a, b) => node('Servant.API.Sub', ':>', a, b);
const alternative = (a, b) => node('Servant.API.Alternative', ':<|>', a, b);
const describe = api => compiledApiSurface({ schemaVersion: 1, api });

// Constructors and argument order verified with the selected Stack/GHC and
// actual servant/servant-multipart TypeRep, not names inferred from alias text.
test('compiled branches retain authentication scope, captures, modifiers and original order', () => {
  const api = alternative(sub(symbol('public'), verb()), sub(
    node('Servant.API.Experimental.Auth', 'AuthProtect', symbol('bearer-token')),
    sub(symbol('bookings'), sub(node('Servant.API.Capture', "Capture'", list(), symbol('id'), textType),
      sub(node('Servant.API.Header', "Header'", list(modifier('Required'), modifier('Strict')), symbol('Idempotency-Key'), textType),
        alternative(verb('PUT'), verb('DELETE', 204)))))));
  const rows = describe(api).operations;
  assert.deepEqual(rows.map(row => [row.ordinal, row.method, row.path, row.authCombinators]),
    [[0, 'GET', '/public', []], [1, 'PUT', '/bookings/{id}', ['bearer-token']], [2, 'DELETE', '/bookings/{id}', ['bearer-token']]]);
  assert.equal(rows[1].parameters[0].in, 'path');
  assert.equal(rows[1].parameters[1].modifiers[0].name, 'Required');
  assert.equal(rows[0].parameters.length, 0, 'Metadata cannot leak between alternative branches');
});

test('request bodies, query variants, multipart, remote host and NoContent survive reflection', () => {
  const api = sub(node('Servant.API.QueryParam', "QueryParam'", list(modifier('Optional')), symbol('q'), textType),
    sub(node('Servant.API.QueryParam', 'QueryParams', symbol('tag'), textType),
      sub(node('Servant.API.QueryParam', 'QueryFlag', symbol('verbose')),
        sub(node('Servant.API.RemoteHost', 'RemoteHost'),
          alternative(sub(node('Servant.API.ReqBody', "ReqBody'", list(modifier('Required')), list(node('Servant.API.ContentTypes', 'JSON')), textType), verb('POST', 201)),
            sub(node('Servant.Multipart.API', "MultipartForm'", list(), node('Servant.Multipart.API', 'Tmp'), textType),
              node('Servant.API.Verbs', 'NoContentVerb', method('DELETE'))))))));
  const rows = describe(api).operations;
  assert.equal(rows[0].bodies[0].modifiers[0].name, 'Required');
  assert.equal(rows[1].bodies[0].multipart, true);
  assert.equal(rows[1].successStatus, 204);
  assert.deepEqual(rows[0].parameters.map(x => x.name), ['q', 'tag', 'verbose']);
  assert.deepEqual(rows[0].internalInputs, ['remote-host']);
});

test('opaque Raw mounts and capture-all remain explicit rather than invented GET endpoints', () => {
  const surface = describe(alternative(verb(), sub(symbol('assets'),
    sub(node('Servant.API.Capture', 'CaptureAll', symbol('segments'), textType), node('Servant.API.Raw', 'Raw')))));
  assert.equal(surface.operations.length, 1);
  assert.equal(surface.rawMounts[0].path, '/assets/{segments*}');
  assert.equal(surface.rawMounts[0].parameters[0].repeated, true);
});

test('unknown or corrupted compiled branches cannot disappear silently', () => {
  for (const broken of [
    alternative(verb(), node('Unknown', 'API')),
    sub(node('Unknown', 'Authorization'), verb()),
    sub(node('Servant.API.Header', "Header'", list()), verb()),
    node('Servant.API.Verbs', 'Verb', node('Unknown', "'GET"), number(200), list(), textType),
    verb('GET', 0),
    node('Servant.API.Empty', 'EmptyAPI'),
  ]) assert.throws(() => describe(broken));
});

test('correspondence detects removed/extra routes, changed success status and ambiguous route declarations', () => {
  const surface = describe(alternative(sub(symbol('new'), verb()),
    alternative(sub(symbol('known'), verb('POST', 201)), sub(symbol('known'), verb('POST', 202)))));
  const result = compareApiSurface(surface, [
    { id: 'POST /known', responses: ['201'] }, { id: 'GET /removed', responses: ['200'] },
  ]);
  assert.deepEqual(result.undocumented, ['GET /new']);
  assert.deepEqual(result.documentedWithoutTypedRoute, ['GET /removed']);
  assert.deepEqual(result.successStatusDifferences, [{ id: 'POST /known', compiled: 202, documented: ['201'] }]);
  assert.equal(result.competingCompiledRoutes[0].id, 'POST /known');
  assert.throws(() => compareApiSurface(surface, [{ id: 'GET /x/{a}', responses: [] }, { id: 'GET /x/{b}', responses: [] }]), /Competing/);
});

test('capture spelling is retained as metadata but does not create false routing drift', () => {
  const surface = describe(sub(symbol('x'), sub(node('Servant.API.Capture', "Capture'", list(), symbol('internalId'), textType), verb())));
  const result = compareApiSurface(surface, [{ id: 'GET /x/{publicId}', responses: ['200'] }]);
  assert.deepEqual(result.undocumented, []);
  assert.deepEqual(result.documentedWithoutTypedRoute, []);
});
