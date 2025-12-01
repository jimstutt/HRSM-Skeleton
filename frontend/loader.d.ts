// frontend/loader.d.ts
export function loadReactor(url?: string): Promise<{
  module: WebAssembly.Module;
  instance: WebAssembly.Instance;
  callEntry: () => number;
}>;
