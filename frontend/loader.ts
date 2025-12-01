// frontend/loader.ts
export async function loadReactor(url = "/static/wasm/reactor.wasm") {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to fetch ${url}: ${res.status}`);
  const buf = await res.arrayBuffer();
  const module = await WebAssembly.compile(buf);

  // For WASI-like imports you'll need a minimal import object. We'll provide env stub.
  const importObject = {
    env: {
      // minimal stubs used by some builds
      abort: () => { throw new Error("wasm abort"); },
      // add more as necessary (memory, table, fd_read, etc)
    },
    wasi_snapshot_preview1: {
      // will be provided if you use WASI runtime (see Node/WASI shims)
    }
  };

  const instance = await WebAssembly.instantiate(module, importObject);
  const exports = instance.exports as any;
  if (typeof exports.reactor_entry !== "function") {
    throw new Error("reactor_entry not exported by wasm module");
  }
  return {
    module,
    instance,
    callEntry: () => exports.reactor_entry()
  };
}
