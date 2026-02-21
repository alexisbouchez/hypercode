import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let rustReady = false;
let worker: Worker | null = null;
let loadPromise: Promise<void> | null = null;

export function isRustReady(): boolean {
  return rustReady;
}

export function initRustRunner(): Promise<void> {
  if (rustReady) return Promise.resolve();
  if (loadPromise) return loadPromise;

  loadPromise = new Promise<void>((resolve, reject) => {
    try {
      // Create Web Worker — Next.js bundles this automatically
      worker = new Worker(new URL("../workers/rust-worker.ts", import.meta.url), {
        type: "module",
      });

      worker.onmessage = (e: MessageEvent) => {
        if (e.data?.loaded) {
          rustReady = true;
          resolve();
        }
      };

      worker.onerror = (err) => {
        loadPromise = null;
        reject(new Error(String(err.message || err)));
      };
    } catch (err) {
      loadPromise = null;
      reject(err);
    }
  });

  return loadPromise;
}

// Strip ANSI escape codes from Miri output
function stripAnsi(str: string): string {
  // eslint-disable-next-line no-control-regex
  return str.replace(/\x1B\[[0-9;]*[mGKHF]/g, "");
}

export function extractRustItems(solution: string): string {
  // Remove fn main() { ... } block, keeping everything else
  const mainPattern = /\bfn\s+main\s*\(\s*\)\s*\{/;
  const match = mainPattern.exec(solution);
  if (!match) return solution.trim();
  let depth = 0;
  let i = match.index;
  while (i < solution.length) {
    if (solution[i] === "{") depth++;
    else if (solution[i] === "}") {
      depth--;
      if (depth === 0) {
        return (solution.slice(0, match.index) + solution.slice(i + 1)).trim();
      }
    }
    i++;
  }
  return solution.trim();
}

export async function runRust(code: string): Promise<RunResult> {
  if (!worker || !rustReady) {
    return { stdout: "", stderr: "", error: "Rust runner not initialized" };
  }

  return new Promise<RunResult>((resolve) => {
    const handler = (e: MessageEvent) => {
      if (e.data?.result !== undefined) {
        worker!.removeEventListener("message", handler);
        const raw = stripAnsi(String(e.data.result));
        // Miri errors start with "error" — treat as compilation/runtime error
        const isError =
          raw.trimStart().startsWith("error") || raw.includes("error[");
        resolve({
          stdout: isError ? "" : raw,
          stderr: isError ? raw : "",
          error: isError ? raw : "",
        });
      }
    };
    worker!.addEventListener("message", handler);
    worker!.postMessage({ code, printLast: false });
  });
}

export async function runTests(
  code: string,
  tests: Test[]
): Promise<TestResult[]> {
  const funcs = extractRustItems(code);
  const results: TestResult[] = [];
  for (const test of tests) {
    const testCode = test.code ? test.code.replace("{{FUNC}}", funcs) : code;
    const result = await runRust(testCode);
    results.push({
      name: test.name,
      passed: !result.error && result.stdout === test.expected,
      actual: result.error || result.stdout,
      expected: test.expected,
    });
  }
  return results;
}
