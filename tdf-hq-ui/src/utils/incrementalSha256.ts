const ROUND_CONSTANTS = new Uint32Array([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
]);

const rotateRight = (value: number, places: number) => (value >>> places) | (value << (32 - places));

export class IncrementalSha256 {
  private readonly state = new Uint32Array([
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
  ]);
  private readonly buffer = new Uint8Array(64);
  private readonly words = new Uint32Array(64);
  private bufferLength = 0;
  private bytesHashed = 0;
  private finalized = false;

  update(input: Uint8Array): this {
    if (this.finalized) throw new Error('SHA-256 digest is already finalized');
    this.bytesHashed += input.byteLength;
    let offset = 0;
    while (offset < input.byteLength) {
      const writable = Math.min(64 - this.bufferLength, input.byteLength - offset);
      this.buffer.set(input.subarray(offset, offset + writable), this.bufferLength);
      this.bufferLength += writable;
      offset += writable;
      if (this.bufferLength === 64) {
        this.processBlock(this.buffer);
        this.bufferLength = 0;
      }
    }
    return this;
  }

  hex(): string {
    if (!this.finalized) this.finalize();
    return Array.from(this.state, (word) => word.toString(16).padStart(8, '0')).join('');
  }

  private finalize() {
    const bitLengthLow = (this.bytesHashed * 8) >>> 0;
    const bitLengthHigh = Math.floor(this.bytesHashed / 0x20000000) >>> 0;
    this.buffer[this.bufferLength] = 0x80;
    this.bufferLength += 1;
    if (this.bufferLength > 56) {
      this.buffer.fill(0, this.bufferLength);
      this.processBlock(this.buffer);
      this.bufferLength = 0;
    }
    this.buffer.fill(0, this.bufferLength, 56);
    const view = new DataView(this.buffer.buffer);
    view.setUint32(56, bitLengthHigh, false);
    view.setUint32(60, bitLengthLow, false);
    this.processBlock(this.buffer);
    this.bufferLength = 0;
    this.finalized = true;
  }

  private processBlock(block: Uint8Array) {
    const view = new DataView(block.buffer, block.byteOffset, block.byteLength);
    for (let index = 0; index < 16; index += 1) this.words[index] = view.getUint32(index * 4, false);
    for (let index = 16; index < 64; index += 1) {
      const previous15 = this.words[index - 15] ?? 0;
      const previous2 = this.words[index - 2] ?? 0;
      const sigma0 = rotateRight(previous15, 7) ^ rotateRight(previous15, 18) ^ (previous15 >>> 3);
      const sigma1 = rotateRight(previous2, 17) ^ rotateRight(previous2, 19) ^ (previous2 >>> 10);
      this.words[index] = ((this.words[index - 16] ?? 0) + sigma0 + (this.words[index - 7] ?? 0) + sigma1) >>> 0;
    }
    let [a, b, c, d, e, f, g, h] = this.state;
    for (let index = 0; index < 64; index += 1) {
      const upperSigma1 = rotateRight(e ?? 0, 6) ^ rotateRight(e ?? 0, 11) ^ rotateRight(e ?? 0, 25);
      const choose = ((e ?? 0) & (f ?? 0)) ^ (~(e ?? 0) & (g ?? 0));
      const temp1 = ((h ?? 0) + upperSigma1 + choose + (ROUND_CONSTANTS[index] ?? 0) + (this.words[index] ?? 0)) >>> 0;
      const upperSigma0 = rotateRight(a ?? 0, 2) ^ rotateRight(a ?? 0, 13) ^ rotateRight(a ?? 0, 22);
      const majority = ((a ?? 0) & (b ?? 0)) ^ ((a ?? 0) & (c ?? 0)) ^ ((b ?? 0) & (c ?? 0));
      const temp2 = (upperSigma0 + majority) >>> 0;
      h = g; g = f; f = e; e = ((d ?? 0) + temp1) >>> 0;
      d = c; c = b; b = a; a = (temp1 + temp2) >>> 0;
    }
    this.state[0] = ((this.state[0] ?? 0) + (a ?? 0)) >>> 0;
    this.state[1] = ((this.state[1] ?? 0) + (b ?? 0)) >>> 0;
    this.state[2] = ((this.state[2] ?? 0) + (c ?? 0)) >>> 0;
    this.state[3] = ((this.state[3] ?? 0) + (d ?? 0)) >>> 0;
    this.state[4] = ((this.state[4] ?? 0) + (e ?? 0)) >>> 0;
    this.state[5] = ((this.state[5] ?? 0) + (f ?? 0)) >>> 0;
    this.state[6] = ((this.state[6] ?? 0) + (g ?? 0)) >>> 0;
    this.state[7] = ((this.state[7] ?? 0) + (h ?? 0)) >>> 0;
  }
}

export const sha256Hex = (input: Uint8Array): string => new IncrementalSha256().update(input).hex();
