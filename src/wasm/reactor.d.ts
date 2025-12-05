// src/wasm/reactor.d.ts
export interface ReactorModule {
  instance: WebAssembly.Instance;
  callReactor: () => void;
}

export function loadReactorFromURL(url: string): Promise<ReactorModule>;
