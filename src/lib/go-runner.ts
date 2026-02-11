import type { GoResult, Test, TestResult } from "@/lib/lessons/types";

declare global {
  interface Window {
    _yaegiRun?: (code: string) => { stdout: string; stderr: string; error: string };
    Go?: new () => { run: (instance: WebAssembly.Instance) => void; importObject: WebAssembly.Imports };
  }
}

let wasmReady = false;
let wasmLoading = false;
let wasmLoadPromise: Promise<void> | null = null;

export function isGoReady(): boolean {
  return wasmReady;
}

export function initGoRunner(): Promise<void> {
  if (wasmReady) return Promise.resolve();
  if (wasmLoadPromise) return wasmLoadPromise;

  wasmLoadPromise = (async () => {
    wasmLoading = true;
    try {
      await new Promise<void>((resolve, reject) => {
        const script = document.createElement("script");
        script.src = "/wasm_exec.js";
        script.onload = () => resolve();
        script.onerror = () => reject(new Error("Failed to load wasm_exec.js"));
        document.head.appendChild(script);
      });

      if (!window.Go) throw new Error("Go WASM support not found");

      const go = new window.Go();
      const result = await WebAssembly.instantiateStreaming(
        fetch("/yaegi.wasm"),
        go.importObject,
      );
      go.run(result.instance);
      wasmReady = true;
    } catch {
      wasmReady = false;
    } finally {
      wasmLoading = false;
    }
  })();

  return wasmLoadPromise;
}

export function isGoLoading(): boolean {
  return wasmLoading;
}

export async function runGo(code: string): Promise<GoResult> {
  if (!wasmReady || !window._yaegiRun) {
    return {
      stdout: "",
      stderr: "",
      error: "Go runtime is not loaded. Build the WASM module with: ./scripts/build-wasm.sh",
    };
  }

  try {
    const result = window._yaegiRun(code);
    return {
      stdout: result.stdout ?? "",
      stderr: result.stderr ?? "",
      error: result.error ?? "",
    };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts user-defined functions, types, vars, and constants from Go source.
 * Strips package declaration, imports, and func main().
 */
export function extractFunctions(code: string): string {
  let result = code;

  // Remove package declaration
  result = result.replace(/^\s*package\s+\w+\s*;?\s*\n/m, "");

  // Remove single-line imports: import "fmt"
  result = result.replace(/^\s*import\s+"[^"]+"\s*\n/gm, "");

  // Remove grouped imports: import ( ... )
  result = result.replace(/^\s*import\s*\([\s\S]*?\)\s*\n/gm, "");

  // Remove func main() { ... }
  const mainRegex = /func\s+main\s*\(\s*\)\s*\{/;
  const match = mainRegex.exec(result);
  if (match) {
    const startIndex = match.index;
    let braceCount = 0;
    let endIndex = startIndex;
    let foundOpen = false;

    for (let i = startIndex; i < result.length; i++) {
      if (result[i] === "{") {
        braceCount++;
        foundOpen = true;
      } else if (result[i] === "}") {
        braceCount--;
        if (foundOpen && braceCount === 0) {
          endIndex = i + 1;
          break;
        }
      }
    }

    result = result.slice(0, startIndex) + result.slice(endIndex);
  }

  return result.trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runGo(codeToRun);

    const hasError = result.error !== "";
    const actual = hasError ? result.error : result.stdout;

    results.push({
      name: test.name,
      passed: !hasError && result.stdout === test.expected,
      actual,
      expected: test.expected,
    });
  }

  return results;
}
