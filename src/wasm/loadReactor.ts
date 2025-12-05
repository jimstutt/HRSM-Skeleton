// src/wasm/loadReactor.ts
// Minimal loader to instantiate a wasm module that exports `call_reactor`.
// Adjust for Node/WASI or Browser needs.

export type ReactorModule = {
  instance: WebAssembly.Instance;
  callReactor: () => void;
};

export async function loadReactorFromURL(url: string): Promise<ReactorModule> {
  // In browsers use instantiateStreaming when available
  let result: WebAssembly.WebAssemblyInstantiatedSource;
  if ((globalThis as any).WebAssembly && (fetch as any) && WebAssembly.instantiateStreaming) {
    result = await WebAssembly.instantiateStreaming(fetch(url), {});
  } else {
    const resp = await fetch(url);
    const bytes = await resp.arrayBuffer();
    result = await WebAssembly.instantiate(bytes, {});
  }

  const instance = result.instance;
  const exports = instance.exports as any;
  if (!exports.call_reactor && !exports.callReactor && !exports.call_reactor) {
    throw new Error("call_reactor export not found in wasm module");
  }

  const call = exports.call_reactor || exports.callReactor;

  return {
    instance,
    callReactor: () => (call as Function)(),
  };
}
