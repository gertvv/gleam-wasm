// Node.js >= 22 is needed for WASM feature support
import fs from 'node:fs';
const load = (filename, imports) => {
  const wasmBuffer = fs.readFileSync(filename);
  return WebAssembly.instantiate(wasmBuffer, imports);
}
/*
load('gleam.wasm').then(gleamModule => {
  const gleam = gleamModule.instance.exports;
  const { list_empty, list_non_empty, int_box } = gleamModule.instance.exports;
  load('generated.wasm', { gleam }).then(exampleModule => {
    const { sum } = exampleModule.instance.exports;
    const list = list_non_empty(
      int_box(BigInt(1)),
      list_non_empty(
        int_box(BigInt(41)),
        list_empty()));
    console.log(sum(list));
  });
});
  */
load('custom.wasm').then(gleamModule => {
  const g = gleamModule.instance.exports;
  const i = g['gleam/Int'](BigInt(1));
  console.log(g['gleam/result/is_ok'](g['gleam/Ok'](i)));
  console.log(g['gleam/result/is_ok'](g['gleam/Error'](i)));
  console.log(g['gleam/result/is_ok'](g['gleam/list/first'](g['gleam/NonEmpty'](g['gleam/Empty'](), i))));
  console.log(g['gleam/result/is_ok'](g['gleam/list/first'](g['gleam/Empty']())));
});
