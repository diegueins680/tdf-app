const { TextDecoder, TextEncoder } = require('node:util');

Object.defineProperties(globalThis, {
  TextDecoder: {
    configurable: true,
    value: TextDecoder,
  },
  TextEncoder: {
    configurable: true,
    value: TextEncoder,
  },
});
