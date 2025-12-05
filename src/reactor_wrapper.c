// src/reactor_wrapper.c
// Minimal C wrapper to call an exported Haskell symbol named `reactor`
// Exports a C function `call_reactor` which will be exported into the final .wasm

// If HsFFI.h is present in your wasi-enabled GHC include, you can include it.
// But to remain portable we declare the symbol manually.

#ifdef __cplusplus
extern "C" {
#endif

// Haskell exported symbol - adjust name & signature to match your Haskell code.
// Here we assume: foreign export ccall reactor :: IO ()
extern void reactor(void);

// Wrapper we will export from the wasm module
void call_reactor(void) {
    // call the Haskell exported function
    reactor();
}

#ifdef __cplusplus
}
#endif
