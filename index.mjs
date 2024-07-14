import fs from 'node:fs';
const load = (filename, imports) => {
  const wasmBuffer = fs.readFileSync(filename);
  return WebAssembly.instantiate(wasmBuffer, imports);
}
load('gleam.wasm').then(gleamModule => {
  const gleam = gleamModule.instance.exports;
  const { list_empty, list_non_empty, int_box, int_unbox } = gleamModule.instance.exports;
  load('example.wasm', { gleam }).then(exampleModule => {
    const { sum } = exampleModule.instance.exports;
    const list = list_non_empty(
      int_box(BigInt(1)),
      list_non_empty(
        int_box(BigInt(41)),
        list_empty()));
    console.log(int_unbox(sum(list)));
  });
});
