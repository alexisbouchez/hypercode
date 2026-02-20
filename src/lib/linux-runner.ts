import { runShell } from "@/lib/linux-shell";
import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

export function initLinuxRunner(): Promise<void> {
  return Promise.resolve();
}

export function isLinuxReady(): boolean {
  return true;
}

export async function runLinux(code: string): Promise<RunResult> {
  const result = runShell(code);
  return {
    stdout: result.stdout,
    stderr: result.stderr,
    error: result.error,
  };
}

export async function runTests(
  code: string,
  tests: Test[]
): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    const codeToRun = test.code ? test.code.replace("{{FUNC}}", code) : code;
    const result = runShell(codeToRun);
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
