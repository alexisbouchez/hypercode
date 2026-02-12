import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let zigReady = false;
let zigLoading = false;
let zigLoadPromise: Promise<void> | null = null;
let compilerWorker: Worker | null = null;
let runnerWorker: Worker | null = null;

export function isZigReady(): boolean {
  return zigReady;
}

export function initZigRunner(): Promise<void> {
  if (zigReady) return Promise.resolve();
  if (zigLoadPromise) return zigLoadPromise;

  zigLoadPromise = (async () => {
    zigLoading = true;
    try {
      compilerWorker = new Worker(
        new URL("./zig-compiler.worker.ts", import.meta.url),
        { type: "module" },
      );
      runnerWorker = new Worker(
        new URL("./zig-runner.worker.ts", import.meta.url),
        { type: "module" },
      );

      // Wait for compiler worker to be ready (it loads zig.wasm + std lib)
      await new Promise<void>((resolve, reject) => {
        if (!compilerWorker) return reject(new Error("No compiler worker"));
        const handler = (e: MessageEvent) => {
          if (e.data.type === "ready") {
            compilerWorker!.removeEventListener("message", handler);
            resolve();
          } else if (e.data.type === "error") {
            compilerWorker!.removeEventListener("message", handler);
            reject(new Error(e.data.error));
          }
        };
        compilerWorker.addEventListener("message", handler);
        compilerWorker.postMessage({ type: "init" });
      });

      zigReady = true;
    } catch (err) {
      zigReady = false;
      zigLoadPromise = null; // allow retry
      throw err;
    } finally {
      zigLoading = false;
    }
  })();

  return zigLoadPromise;
}

export function isZigLoading(): boolean {
  return zigLoading;
}

async function compileZig(code: string): Promise<Uint8Array> {
  if (!compilerWorker) throw new Error("Zig compiler not initialized");

  return new Promise((resolve, reject) => {
    const handler = (e: MessageEvent) => {
      compilerWorker!.removeEventListener("message", handler);
      if (e.data.type === "compiled") {
        resolve(new Uint8Array(e.data.wasm));
      } else if (e.data.type === "error") {
        reject(new Error(e.data.error));
      }
    };
    compilerWorker!.addEventListener("message", handler);
    compilerWorker!.postMessage({ type: "compile", code });
  });
}

async function executeWasm(wasm: Uint8Array): Promise<{ stdout: string; stderr: string }> {
  if (!runnerWorker) throw new Error("Zig runner not initialized");

  return new Promise((resolve, reject) => {
    const handler = (e: MessageEvent) => {
      runnerWorker!.removeEventListener("message", handler);
      if (e.data.type === "result") {
        resolve({ stdout: e.data.stdout, stderr: e.data.stderr });
      } else if (e.data.type === "error") {
        reject(new Error(e.data.error));
      }
    };
    runnerWorker!.addEventListener("message", handler);
    runnerWorker!.postMessage({ type: "run", wasm: wasm.buffer }, [wasm.buffer]);
  });
}

export async function runZig(code: string): Promise<RunResult> {
  if (!zigReady) {
    return {
      stdout: "",
      stderr: "",
      error: "Zig runtime is not loaded. Please wait for it to initialize.",
    };
  }

  try {
    const wasm = await compileZig(code);
    const result = await executeWasm(wasm);
    return {
      stdout: result.stdout || result.stderr,
      stderr: "",
      error: "",
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
 * Extracts user-defined functions, types, and constants from Zig source.
 * Strips the std import and pub fn main.
 */
export function extractZigFunctions(code: string): string {
  let result = code;

  // Remove std import
  result = result.replace(/^\s*const\s+std\s*=\s*@import\s*\(\s*"std"\s*\)\s*;\s*\n/m, "");

  // Remove pub fn main
  const mainRegex = /pub\s+fn\s+main\s*\([^)]*\)\s*[^{]*\{/;
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
      const funcs = extractZigFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runZig(codeToRun);

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
