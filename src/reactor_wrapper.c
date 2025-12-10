/* Minimal C wrapper to expose a named function to JS/WASM runtime.
 *
 * Put this file at: ~/Dev/NGOLogisticsCG/src/reactor_wrapper.c
 *
 * This example exposes `reactor_entry` which you can call from JS after instantiating the WASM.
 *
 * If your Haskell/ghc-wasm build produces functions with known names (C symbols), you can declare them
 * as extern and call them from here (or use this wrapper purely to export a small entrypoint).
 */

#include <stdint.h>

/* example: export a small entry function that returns an integer (0) */
int reactor_entry(void) {
    /* Replace with a call to a real symbol from your GHC-produced objects if available:
       extern int hs_reactor_start(void);
       return hs_reactor_start();
    */
    return 0;
}

/* Export the function name explicitly via attribute (keeps symbol unmangled) */
__attribute__((used))
extern int reactor_entry(void);
