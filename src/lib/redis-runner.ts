import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import { RedisEmulator } from "@/lib/redis-emulator";
import { parseRedisValidation, evaluateRedisValidation } from "@/lib/redis-shared";

let redisReady = false;
const emulator = new RedisEmulator();

export function isRedisReady(): boolean {
  return redisReady;
}

export function initRedisRunner(): Promise<void> {
  redisReady = true;
  return Promise.resolve();
}

export async function runRedis(code: string): Promise<RunResult> {
  try {
    emulator.reset();
    const output = emulator.run(code);
    return { stdout: output, stderr: "", error: "" };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

export async function runTests(
  code: string,
  tests: Test[],
): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    try {
      const db = new RedisEmulator();

      // If test.code is set, it's setup code to run before the user's solution
      if (test.code) {
        const parts = test.code.split("---VALIDATE---");
        const setup = parts[0].replace("{{USER_CODE}}", code.trim());
        db.run(setup);
        const validation = parts[1]?.trim() ?? "";
        if (validation) {
          const output = db.run(validation);
          const spec = parseRedisValidation(test.expected);
          const ev = evaluateRedisValidation(spec, output);
          results.push({ name: test.name, ...ev });
        } else {
          const output = db.run(code.trim());
          const spec = parseRedisValidation(test.expected);
          const ev = evaluateRedisValidation(spec, output);
          results.push({ name: test.name, ...ev });
        }
      } else {
        const output = db.run(code.trim());
        const spec = parseRedisValidation(test.expected);
        const ev = evaluateRedisValidation(spec, output);
        results.push({ name: test.name, ...ev });
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      results.push({
        name: test.name,
        passed: false,
        actual: message,
        expected: test.expected,
      });
    }
  }

  return results;
}
