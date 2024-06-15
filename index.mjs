import fs from 'node:fs';
const wasmBuffer = fs.readFileSync('example.wasm');
WebAssembly.instantiate(wasmBuffer).then(wasmModule => {
  const { list_empty, list_non_empty, int_box, int_unbox, sum } = wasmModule.instance.exports;
  const list = list_non_empty(
    int_box(BigInt(1)),
    list_non_empty(
      int_box(BigInt(41)),
      list_empty()));
  console.log(int_unbox(sum(list)));
});
