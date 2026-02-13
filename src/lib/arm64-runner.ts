import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import { assemble } from "./arm64/assembler";
import { execute } from "./arm64/interpreter";

// ARM64 runner is always ready (no WASM to load, pure TS)
let ready = false;

export function initArm64Runner(): Promise<void> {
  ready = true;
  return Promise.resolve();
}

export function isArm64Ready(): boolean {
  return ready;
}

export function runArm64Code(source: string): { stdout: string; error: string } {
  try {
    const program = assemble(source);
    const result = execute(program);
    return { stdout: result.stdout, error: result.error };
  } catch (err) {
    return { stdout: "", error: err instanceof Error ? err.message : String(err) };
  }
}

export async function runArm64(code: string): Promise<RunResult> {
  const result = runArm64Code(code);
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
    const result = runArm64Code(codeToRun);

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
