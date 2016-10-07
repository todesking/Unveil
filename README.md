# Unveil: Runtime JVM bytecode optimizer

* [日本語の発表資料](http://techlog.mvrck.co.jp/entry/todesking-runtime-jvm-bytecode-optimization/)

## Motivation

In spite of JVM has JIT compilation, theres remaining some performance overhead.

These overhead becomes deadly in some application.

This project aim to reducing overhead with runtime bytecode optimization.

## Current Status

Very experimental. Stay tuned!


## Benchmark

Coming soon(really).


## Related work

[Soot](https://sable.github.io/soot/) is JVM Bytecode optimizer framework.
It aims compile-time optimization, not runtime(AFAIK).

[StreamJIT](https://github.com/jbosboom/streamjit) proposes "Commensal compiler" paradigm.
It uses runtime optimization techniques via `MethodHandle`.


