// frontend/main.ts

// This shim usually comes from the ghc-wasm-meta environment, 
// but for a raw setup, we can define the minimal WASI imports needed 
// or use a polyfill like @bjorn3/browser_wasi_shim if using npm.
// For GHC WASM Reactor, we strictly need to initialize the exports.

// Define the shape of our WASM exports (adjust based on your Haskell exports)
interface WasmExports {
    hs_init: (argc: number, argv: number) => void;
    malloc: (size: number) => number;
    free: (ptr: number) => void;
}

async function runWasm() {
    try {
        const response = await fetch('reactor.wasm');
        const buffer = await response.arrayBuffer();

        // Standard WASI imports that GHC expects
        // In a real production app, you would use 'browser_wasi_shim' package here
        const wasiImports = {
            wasi_snapshot_preview1: {
                proc_exit: (rval: number) => console.log(`proc_exit: ${rval}`),
                fd_write: (fd: number, iovs_ptr: number, iovs_len: number, nwritten_ptr: number) => {
                    console.log("WASI fd_write called (output logging)");
                    return 0;
                },
                // Add other WASI shims as required by your specific dependencies
                fd_close: () => 0,
                fd_seek: () => 0,
                fd_fdstat_get: () => 0,
                environ_sizes_get: () => 0,
                environ_get: () => 0,
                clock_time_get: () => 0,
            }
        };

        const module = await WebAssembly.instantiate(buffer, wasiImports);
        const exports = module.instance.exports as unknown as WasmExports;

        // Initialize the Haskell Runtime
        console.log("Initializing GHC WASM Reactor...");
        // 0 arguments passed to main
        exports.hs_init(0, 0); 
        
        console.log("Haskell Reactor Initialized. Ready for Reflex callbacks.");

    } catch (e) {
        console.error("Failed to load WASM:", e);
    }
}

runWasm();
