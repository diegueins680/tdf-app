import { isDeepStrictEqual } from 'node:util';

// Consume compiler TypeRep structure, never a regex approximation of Haskell.
// Unknown combinators fail rather than silently dropping a production branch.
const key = node => `${node.module}.${node.name}`;
function checked(node) {
  if (!node || typeof node.module !== 'string' || typeof node.name !== 'string' || !Array.isArray(node.args)) {
    throw new Error('Malformed compiled API type node');
  }
  return node;
}
function arity(node, count) {
  checked(node);
  if (node.args.length !== count) throw new Error(`Unexpected type arity: ${key(node)}`);
  return node.args;
}
function symbol(node) {
  arity(node, 0);
  if (node.module !== 'GHC.TypeLits') throw new Error('Expected compiler type-level string');
  const value = JSON.parse(node.name);
  if (typeof value !== 'string') throw new Error('Expected compiler type-level string');
  return value;
}
function list(node) {
  checked(node);
  if (key(node) === "GHC.Types.'[]") { arity(node, 0); return []; }
  if (key(node) !== "GHC.Types.':") throw new Error('Expected compiler type-level list');
  const [head, tail] = arity(node, 2);
  return [head, ...list(tail)];
}
function method(node) {
  arity(node, 0);
  if (node.module !== 'Network.HTTP.Types.Method' || !/^'(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS|CONNECT|TRACE)$/.test(node.name)) {
    throw new Error('Unknown compiled HTTP method');
  }
  return node.name.slice(1);
}
function natural(node) {
  arity(node, 0);
  if (node.module !== 'GHC.TypeLits' || !/^\d+$/.test(node.name)) throw new Error('Expected compiled status');
  const result = Number(node.name);
  if (!Number.isSafeInteger(result) || result < 100 || result > 599) throw new Error('Invalid compiled status');
  return result;
}
const modifiedParameters = new Map([
  ["Servant.API.QueryParam.QueryParam'", 'query'],
  ["Servant.API.Header.Header'", 'header'],
]);
export function compiledApiSurface(document) {
  if (document?.schemaVersion !== 1) throw new Error('Unsupported compiled API description');
  const operations = [], rawMounts = [];
  let ordinal = 0;
  function walk(node, state) {
    checked(node);
    const name = key(node);
    if (name === 'Servant.API.Alternative.:<|>') {
      const [left, right] = arity(node, 2);
      walk(left, state); walk(right, state); return;
    }
    if (name === 'Servant.API.Sub.:>') {
      const [component, tail] = arity(node, 2);
      checked(component);
      const modifier = key(component);
      const next = structuredClone(state);
      if (component.module === 'GHC.TypeLits') next.segments.push(symbol(component));
      else if (modifier === "Servant.API.Capture.Capture'") {
        const [mods, label, type] = arity(component, 3), capture = symbol(label);
        next.segments.push(`{${capture}}`);
        next.parameters.push({ in: 'path', name: capture, modifiers: list(mods), type });
      } else if (modifier === 'Servant.API.Capture.CaptureAll') {
        const [label, type] = arity(component, 2), capture = symbol(label);
        next.segments.push(`{${capture}*}`);
        next.parameters.push({ in: 'path', name: capture, repeated: true, type });
      } else if (modifiedParameters.has(modifier)) {
        const [mods, label, type] = arity(component, 3);
        next.parameters.push({ in: modifiedParameters.get(modifier), name: symbol(label), modifiers: list(mods), type });
      } else if (modifier === 'Servant.API.QueryParam.QueryParams') {
        const [label, type] = arity(component, 2);
        next.parameters.push({ in: 'query', name: symbol(label), repeated: true, type });
      } else if (modifier === 'Servant.API.QueryParam.QueryFlag') {
        const [label] = arity(component, 1);
        next.parameters.push({ in: 'query', name: symbol(label), flag: true });
      } else if (modifier === "Servant.API.ReqBody.ReqBody'") {
        const [mods, contentTypes, type] = arity(component, 3);
        next.bodies.push({ modifiers: list(mods), contentTypes: list(contentTypes), type });
      } else if (modifier === "Servant.Multipart.API.MultipartForm'") {
        const [mods, storage, type] = arity(component, 3);
        next.bodies.push({ multipart: true, modifiers: list(mods), storage, type });
      } else if (modifier === 'Servant.API.Experimental.Auth.AuthProtect') {
        const [tag] = arity(component, 1);
        next.authCombinators.push(symbol(tag));
      } else if (modifier === 'Servant.API.RemoteHost.RemoteHost') {
        arity(component, 0); next.internalInputs.push('remote-host');
      } else {
        throw new Error(`Unrecognized compiled API combinator: ${modifier}`);
      }
      walk(tail, next); return;
    }
    const base = { ordinal: ordinal++, path: '/' + state.segments.join('/'), ...state };
    delete base.segments;
    if (name === 'Servant.API.Verbs.Verb') {
      const [verb, status, contentTypes, responseType] = arity(node, 4);
      operations.push({ ...base, method: method(verb), successStatus: natural(status), contentTypes: list(contentTypes), responseType });
    } else if (name === 'Servant.API.Verbs.NoContentVerb') {
      const [verb] = arity(node, 1);
      operations.push({ ...base, method: method(verb), successStatus: 204, contentTypes: [], responseType: null });
    } else if (name === 'Servant.API.Raw.Raw') {
      arity(node, 0); rawMounts.push(base);
    } else if (name === 'Servant.API.Empty.EmptyAPI') {
      arity(node, 0);
    } else throw new Error(`Unrecognized compiled API terminal: ${name}`);
  }
  walk(document.api, { segments: [], parameters: [], bodies: [], authCombinators: [], internalInputs: [] });
  if (operations.length === 0) throw new Error('Compiled API has no operations');
  return { operations, rawMounts };
}
// Parameter names do not alter routing. Keep originals in both source records.
export const routeKey = (methodName, pathname) => `${methodName} ${pathname.replace(/\{([^}]+)\}/g, (_, capture) => capture.endsWith('*') ? '{*}' : '{}')}`;
export function compareApiSurface(surface, documented) {
  const docs = new Map();
  for (const operation of documented) {
    const match = /^(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS|TRACE|CONNECT) (\/.*)$/.exec(operation.id);
    if (!match) throw new Error('Malformed documented operation identity');
    const id = routeKey(match[1], match[2]);
    if (docs.has(id)) throw new Error(`Competing OpenAPI routing declarations: ${id}`);
    docs.set(id, operation);
  }
  const compiled = new Map();
  for (const row of surface.operations) {
    const id = routeKey(row.method, row.path);
    compiled.set(id, [...(compiled.get(id) ?? []), row]);
  }
  return {
    undocumented: [...compiled.keys()].filter(id => !docs.has(id)).sort(),
    documentedWithoutTypedRoute: [...docs.keys()].filter(id => !compiled.has(id)).sort(),
    competingCompiledRoutes: [...compiled].filter(([, rows]) => rows.length > 1).map(([id, rows]) => ({ id, ordinals: rows.map(row => row.ordinal) })),
    successStatusDifferences: [...compiled].filter(([id]) => docs.has(id)).flatMap(([id, rows]) =>
      rows.filter(row => !docs.get(id).responses.includes(String(row.successStatus)))
        .map(row => ({ id, compiled: row.successStatus, documented: docs.get(id).responses }))),
    limitations: 'Typed route and declared success-status correspondence only. Raw mounts may own additional paths. Handler authorization, middleware, errors, JSON codecs and feature activation require separate verification.',
  };
}

export function compiledApiDeclarationSnapshot(surface) {
  return {
    schemaVersion: 1,
    authority: 'Reviewed implementation declaration snapshot; not product approval or complete API conformance.',
    surface,
  };
}
export function verifyCompiledApiDeclarationSnapshot(surface, snapshot) {
  if (!isDeepStrictEqual(snapshot, compiledApiDeclarationSnapshot(surface))) {
    throw new Error('Compiled API declaration drift: review the retained contract-candidate.json, reconcile intent and regenerate the canonical snapshot.');
  }
}

// Deferral is a reviewed product boundary, not permission to omit arbitrary APIs.
export function verifyApiAvailability(surface, documented, policy) {
  if (policy?.schemaVersion !== 1 || typeof policy.authority !== 'string' || !policy.authority.trim()
    || !Array.isArray(policy.deferredOperations)) throw new Error('Malformed API availability policy');
  const comparison = compareApiSurface(surface, documented);
  const missing = new Set(comparison.documentedWithoutTypedRoute);
  const declared = new Set(documented.map(row => {
    const split = row.id.indexOf(' ');
    return routeKey(row.id.slice(0, split), row.id.slice(split + 1));
  }));
  const deferred = new Set();
  for (const row of policy.deferredOperations) {
    if (!row || typeof row.id !== 'string' || !/^[A-Z]+ \/[^?]*$/.test(row.id)
      || typeof row.requirement !== 'string' || !/^[A-Z]+-[A-Z0-9-]+$/.test(row.requirement)
      || typeof row.reason !== 'string' || !row.reason.trim()
      || !Array.isArray(row.sources) || !row.sources.length
      || row.sources.some(source => typeof source !== 'string' || !source.trim())) {
      throw new Error('Malformed deferred API operation');
    }
    const split = row.id.indexOf(' '), id = routeKey(row.id.slice(0, split), row.id.slice(split + 1));
    if (id !== row.id || deferred.has(id)) throw new Error('Duplicate or noncanonical deferred API operation');
    deferred.add(id);
    if (!declared.has(id)) throw new Error(`Deferred API declaration disappeared: ${id}`);
    if (!missing.has(id)) throw new Error(`Deferred API unexpectedly mounted: ${id}`);
  }
  const unexplained = [...missing].filter(id => !deferred.has(id));
  if (unexplained.length) throw new Error(`Documented API unexpectedly unmounted: ${unexplained.join(', ')}`);
  return { deferredOperations: [...deferred].sort(), unexpectedUnmounted: [],
    limitations: 'Explicit typed-route deferrals only; Raw mount behavior, production flags and authorization require separate evidence.' };
}

// These are explicit unavailable handlers, not waivers for undocumented success.
export function verifyApiResponseStatus(surface, documented, policy) {
  if (policy?.schemaVersion !== 1 || typeof policy.authority !== 'string' || !policy.authority.trim()
    || !Array.isArray(policy.unavailableOperations)) throw new Error('Invalid API response-status policy');
  const comparison = compareApiSurface(surface, documented);
  if (comparison.competingCompiledRoutes.length) throw new Error('Competing response-status routes');
  const differences = new Map(comparison.successStatusDifferences.map(row => [row.id, row]));
  const admitted = new Set();
  for (const row of policy.unavailableOperations) {
    if (!row || typeof row.id !== 'string' || !/^(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS|TRACE|CONNECT) \/[^?\s]*$/.test(row.id)
      || typeof row.requirement !== 'string' || !/^[A-Z]+-[A-Z0-9-]+$/.test(row.requirement)
      || typeof row.reason !== 'string' || !row.reason.trim()
      || !Array.isArray(row.sources) || !row.sources.length || row.sources.some(x => typeof x !== 'string' || !x.trim())
      || !Number.isInteger(row.compiledStatus) || row.compiledStatus < 200 || row.compiledStatus > 299
      || row.unavailableStatus !== 503) throw new Error('Malformed unavailable API status');
    const split = row.id.indexOf(' ');
    if (routeKey(row.id.slice(0, split), row.id.slice(split + 1)) !== row.id || admitted.has(row.id))
      throw new Error('Duplicate or noncanonical unavailable API status');
    admitted.add(row.id);
    const difference = differences.get(row.id);
    if (!difference || difference.compiled !== row.compiledStatus
      || !difference.documented.includes(String(row.unavailableStatus))
      || difference.documented.some(status => /^2[0-9]{2}$/.test(status)))
      throw new Error(`Unavailable API status contract changed: ${row.id}`);
  }
  const unexpected = [...differences.keys()].filter(id => !admitted.has(id));
  if (unexpected.length) throw new Error(`Unreconciled API response status: ${unexpected.join(', ')}`);
  return { unavailableOperations: [...admitted].sort(), unexpectedStatusDifferences: [],
    limitations: 'Declared status correspondence only. Current handler/error, DTO codec and authorization evidence remains required.' };
}
