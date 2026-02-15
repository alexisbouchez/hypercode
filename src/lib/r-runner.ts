import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let rReady = false;
let rLoading = false;
let rLoadPromise: Promise<void> | null = null;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let webR: any = null;

export function isRReady(): boolean {
  return rReady;
}

export function isRLoading(): boolean {
  return rLoading;
}

export function initRRunner(): Promise<void> {
  if (rReady) return Promise.resolve();
  if (rLoadPromise) return rLoadPromise;

  rLoadPromise = (async () => {
    rLoading = true;
    try {
      const { WebR } = await import("webr");
      const w = new WebR();
      await w.init();
      webR = w;
      rReady = true;
    } catch (err) {
      rReady = false;
      rLoadPromise = null;
      throw err;
    } finally {
      rLoading = false;
    }
  })();

  return rLoadPromise;
}

export async function runR(code: string): Promise<RunResult> {
  if (!rReady || !webR) {
    return {
      stdout: "",
      stderr: "",
      error: "R runtime is not loaded. Please wait for it to initialize.",
    };
  }

  try {
    const shelter = await new webR.Shelter();
    try {
      const result = await shelter.captureR(code, {
        withAutoprint: true,
        captureStreams: true,
        captureConditions: false,
      });

      let stdout = "";
      let stderr = "";

      for (const out of result.output) {
        if (out.type === "stdout") {
          stdout += out.data + "\n";
        } else if (out.type === "stderr") {
          stderr += out.data + "\n";
        }
      }

      return { stdout, stderr, error: "" };
    } finally {
      await shelter.purge();
    }
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts user-defined functions from R source.
 * Strips top-level calls (lines that are not function assignments).
 */
export function extractRFunctions(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let inFunction = false;
  let braceCount = 0;

  for (const line of lines) {
    if (inFunction) {
      result.push(line);
      braceCount += (line.match(/\{/g) || []).length;
      braceCount -= (line.match(/\}/g) || []).length;
      if (braceCount <= 0) {
        inFunction = false;
        braceCount = 0;
      }
    } else if (line.match(/^\s*\w+\s*<-\s*function\s*\(/)) {
      result.push(line);
      braceCount += (line.match(/\{/g) || []).length;
      braceCount -= (line.match(/\}/g) || []).length;
      if (braceCount > 0) {
        inFunction = true;
      }
    } else if (line.match(/^\s*#/) || line.trim() === "") {
      // Keep comments and blank lines
      result.push(line);
    }
  }

  return result.join("\n").trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractRFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runR(codeToRun);

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
