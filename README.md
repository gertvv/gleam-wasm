Gleam transcribed to WebAssembly
===

For some reason, I have a concurrent fascination with Gleam and WASM/WASM-GC/WASI at the moment.

I'd like to be able to compile the one to the other, but hacking the Gleam compiler feels daunting.

As a starting point, I built very simple implementations of List and Int, and used these to transcribe some Gleam code to WebAssembly.