import { lookup as dnsLookup } from 'node:dns';
import { request as httpsRequest } from 'node:https';
import { BlockList, isIP } from 'node:net';

const privateNetworks = new BlockList();
for (const [network, prefix] of [['0.0.0.0', 8], ['10.0.0.0', 8], ['100.64.0.0', 10],
  ['127.0.0.0', 8], ['169.254.0.0', 16], ['172.16.0.0', 12], ['192.0.0.0', 24],
  ['192.0.2.0', 24], ['192.168.0.0', 16], ['198.18.0.0', 15], ['198.51.100.0', 24],
  ['203.0.113.0', 24], ['224.0.0.0', 3]]) privateNetworks.addSubnet(network, prefix, 'ipv4');
const ipv6Global = new BlockList();
ipv6Global.addSubnet('2000::', 3, 'ipv6');
for (const [network, prefix] of [['2001::', 23], ['2001:db8::', 32], ['2002::', 16], ['3fff::', 20]]) {
  privateNetworks.addSubnet(network, prefix, 'ipv6');
}

export function isPublicImageAddress(address) {
  const family = isIP(address);
  if (family === 4) return !privateNetworks.check(address, 'ipv4');
  return family === 6 && ipv6Global.check(address, 'ipv6') && !privateNetworks.check(address, 'ipv6');
}

export function publicImageUrl(value) {
  const url = new URL(value);
  if (url.protocol !== 'https:' || url.username || url.password || (url.port && url.port !== '443')) {
    throw new Error('Artist images require credential-free HTTPS on the standard port');
  }
  const host = url.hostname.replace(/^\[|\]$/g, '');
  if (isIP(host) && !isPublicImageAddress(host)) throw new Error('Artist image address is not public');
  url.hash = '';
  return url;
}

/** DNS validation happens in the TLS connection's lookup, not in a separate
 * preflight that could resolve differently at connection time. No cookies,
 * credentials, proxy environment or caller headers are forwarded.
 */
async function requestImage(url, { lookup, request, maxBytes, signal }) {
  return new Promise((resolve, reject) => {
    let admittedAddresses;
    const connection = request(url, {
      agent: false, method: 'GET', signal,
      headers: { Accept: 'image/avif,image/webp,image/png,image/jpeg', 'Accept-Encoding': 'identity' },
      lookup(host, options, callback) {
        lookup(host, { ...options, all: true }, (error, addresses) => {
          if (error) return callback(error);
          if (!addresses?.length || addresses.some(({ address }) => !isPublicImageAddress(address))) {
            callback(new Error('Artist image DNS returned a non-public address'));
            return;
          }
          admittedAddresses = addresses.map(({ address }) => address);
          callback(null, options.all ? addresses : addresses[0].address, addresses[0].family);
        });
      },
    }, (response) => {
      const remoteAddress = response.socket?.remoteAddress?.replace(/^::ffff:/, '');
      if (!isPublicImageAddress(remoteAddress ?? '')
        || (admittedAddresses && !admittedAddresses.includes(remoteAddress))) {
        response.destroy();
        reject(new Error('Artist image connection address did not match admitted public DNS'));
        return;
      }
      const status = response.statusCode ?? 0;
      if ([301, 302, 303, 307, 308].includes(status)) {
        const location = response.headers.location;
        response.destroy();
        resolve({ status, location });
        return;
      }
      if (status !== 200 || Number(response.headers['content-length'] ?? 0) > maxBytes
        || !['identity', undefined].includes(response.headers['content-encoding'])) {
        response.destroy();
        reject(new Error(`Artist image download rejected (HTTP ${status}, size or encoding)`));
        return;
      }
      const chunks = [];
      let size = 0;
      response.on('data', (chunk) => {
        size += chunk.length;
        if (size > maxBytes) response.destroy(new Error('Artist image exceeds the streaming byte limit'));
        else chunks.push(chunk);
      });
      response.on('error', reject);
      response.on('end', () => resolve({ status, bytes: Buffer.concat(chunks) }));
    });
    connection.on('error', reject);
    connection.end();
  });
}

export async function downloadArtistImage(value, {
  lookup = dnsLookup, request = httpsRequest, maxBytes = 25 * 1024 * 1024,
  signal = AbortSignal.timeout(30_000),
} = {}) {
  let url = publicImageUrl(value);
  for (let hop = 0; hop <= 3; hop += 1) {
    const response = await requestImage(url, { lookup, request, maxBytes, signal });
    if (response.bytes) return response.bytes;
    if (!response.location || hop === 3) throw new Error('Artist image redirect limit exceeded');
    url = publicImageUrl(new URL(response.location, url));
  }
  throw new Error('Artist image download failed');
}
