import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let jsReady = false;

export function isJsReady(): boolean {
  return jsReady;
}

export function initJsRunner(): Promise<void> {
  jsReady = true;
  return Promise.resolve();
}

export async function runJs(code: string): Promise<RunResult> {
  try {
    let output = "";
    const originalLog = console.log;
    console.log = (...args: unknown[]) => {
      output += args.map((a) => (typeof a === "string" ? a : String(a))).join(" ") + "\n";
    };
    try {
      // eslint-disable-next-line no-new-func
      new Function(code)();
    } finally {
      console.log = originalLog;
    }
    return { stdout: output, stderr: "", error: "" };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts top-level function declarations and arrow function assignments from JS source.
 * Removes standalone top-level calls (console.log, etc.).
 */
export function extractJsFunctions(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inFunction = false;

  for (const line of lines) {
    const opens = (line.match(/\{/g) || []).length;
    const closes = (line.match(/\}/g) || []).length;

    if (!inFunction && depth === 0) {
      if (
        line.match(/^\s*function\s+\w+/) ||
        line.match(/^\s*(?:const|let|var)\s+\w+\s*=\s*(?:async\s*)?\(/)
      ) {
        inFunction = true;
        result.push(line);
        depth += opens - closes;
        if (depth <= 0) {
          inFunction = false;
          depth = 0;
        }
      }
      // Skip standalone top-level calls
    } else {
      result.push(line);
      depth += opens - closes;
      if (depth <= 0) {
        inFunction = false;
        depth = 0;
      }
    }
  }

  return result.join("\n").trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractJsFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runJs(codeToRun);
    const hasError = result.error !== "";

    results.push({
      name: test.name,
      passed: !hasError && result.stdout === test.expected,
      actual: hasError ? result.error : result.stdout,
      expected: test.expected,
    });
  }

  return results;
}
