Gleam "compiled" to WebAssembly
===

For some reason, I have a concurrent fascination with Gleam and WASM/WASM-GC/WASI at the moment.

I'd like to be able to compile the one to the other, but hacking the Gleam compiler feels daunting. I don't want to _also_ learn Rust at the same time.

This is a basic proof-of-concept that tackles some of the challenges but is missing many things. Because I cared as much about experimenting with WebAssembly as Gleam, this outputs WebAssembly text format (.wat).

Dependencies
---

Gleam (erlang) >= 1.0, binaryen >= 118, node >= 22

"Using"
---

This compiles the `example.gleam` file to `generated.wat`, compiles the WebAssembly, and runs it:

```
gleam run
wasm-as --enable-gc --enable-reference-types --enable-tail-call gleam.wat
wasm-as --enable-gc --enable-reference-types --enable-tail-call generated.wat
node index.mjs
```

Disassembling
---

```
wasm-dis --all-features generated.wasm -o generated.reversed.wat
```

See also
---

If you're serious about compiling Gleam to WASM you probably want to look here instead:

 - [The Gleam compiler](https://github.com/gleam-lang/gleam/)
 - [Fork of the Gleam compiler targeting WASM](https://github.com/DanielleMaywood/fyp-gleam/)
