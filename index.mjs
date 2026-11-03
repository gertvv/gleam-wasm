// Node.js >= 22 is needed for WASM feature support
import fs from 'node:fs';
const load = (filename, imports) => {
  const wasmBuffer = fs.readFileSync(filename);
  return WebAssembly.instantiate(wasmBuffer, imports);
}

let memory = null;
let decoder = new TextDecoder();

function print_string(offset, length) {
  let bytes = new DataView(memory, offset, length);
  let string = decoder.decode(bytes);
  // console.log(`Got ${length} bytes to print: "${string}"`);
  process.stdout.write(string);
}

load('sayer.wasm', {
  'host': {
    'print': print_string
  }
}).then(module => {
  const hello = module.instance.exports.rec_main;
  memory = module.instance.exports.memory.buffer;
  hello();
})
