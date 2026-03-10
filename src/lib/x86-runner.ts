import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import { assemble } from "./x86/assembler";
import { execute } from "./x86/interpreter";

// x86_64 runner is always ready (no WASM to load, pure TS)
let ready = false;

export function initX86Runner(): Promise<void> {
  ready = true;
  return Promise.resolve();
}

export function isX86Ready(): boolean {
  return ready;
}

export function runX86Code(source: string): { stdout: string; error: string } {
  try {
    const program = assemble(source);
    const result = execute(program);
    return { stdout: result.stdout, error: result.error };
  } catch (err) {
    return { stdout: "", error: err instanceof Error ? err.message : String(err) };
  }
}

export async function runX86(code: string): Promise<RunResult> {
  const result = runX86Code(code);
  return {
    stdout: result.stdout,
    stderr: "",
    error: result.error,
  };
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    const codeToRun = test.code ? test.code.replace("{{FUNC}}", code) : code;
    const result = runX86Code(codeToRun);

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
