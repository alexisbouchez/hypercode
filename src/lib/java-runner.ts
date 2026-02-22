import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let javaReady = false;
let worker: Worker | null = null;
let loadPromise: Promise<void> | null = null;

export function isJavaReady(): boolean {
  return javaReady;
}

export function initJavaRunner(): Promise<void> {
  if (javaReady) return Promise.resolve();
  if (loadPromise) return loadPromise;

  loadPromise = new Promise<void>((resolve, reject) => {
    try {
      worker = new Worker(
        new URL("../workers/java-worker.ts", import.meta.url),
        { type: "module" }
      );

      worker.onmessage = (e: MessageEvent) => {
        if (e.data?.loaded) {
          javaReady = true;
          resolve();
        } else if (e.data?.error && !javaReady) {
          loadPromise = null;
          reject(new Error(e.data.error));
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

export async function runJava(code: string): Promise<RunResult> {
  if (!worker || !javaReady) {
    return { stdout: "", stderr: "", error: "Java runner not initialized" };
  }

  return new Promise<RunResult>((resolve) => {
    const handler = (e: MessageEvent) => {
      if (e.data?.stdout !== undefined || e.data?.error !== undefined) {
        worker!.removeEventListener("message", handler);
        resolve({
          stdout: e.data.stdout ?? "",
          stderr: e.data.stderr ?? "",
          error: e.data.error ?? "",
        });
      }
    };
    worker!.addEventListener("message", handler);
    worker!.postMessage({ code });
  });
}

export async function runTests(
  code: string,
  tests: Test[]
): Promise<TestResult[]> {
  const results: TestResult[] = [];
  for (const test of tests) {
    const codeToRun = test.code ? test.code.replace("{{FUNC}}", code) : code;
    const result = await runJava(codeToRun);
    results.push({
      name: test.name,
      passed: !result.error && result.stdout === test.expected,
      actual: result.error || result.stdout,
      expected: test.expected,
    });
  }
  return results;
}
