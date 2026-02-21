import { RedisEmulator } from "@/lib/redis-emulator";
import { parseRedisValidation, evaluateRedisValidation } from "@/lib/redis-shared";
import { redisLessons } from "@/lib/lessons/redis/index";
import type { LessonTestResult } from "./types";

export function runRedisTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of redisLessons) {
    for (const test of lesson.tests) {
      try {
        const db = new RedisEmulator();

        let output: string;

        if (test.code) {
          const parts = test.code.split("---VALIDATE---");
          const setup = parts[0].replace("{{USER_CODE}}", lesson.solution.trim());
          db.run(setup);
          const validation = parts[1]?.trim() ?? "";
          output = validation ? db.run(validation) : db.run(lesson.solution.trim());
        } else {
          output = db.run(lesson.solution.trim());
        }

        const spec = parseRedisValidation(test.expected);
        const ev = evaluateRedisValidation(spec, output);
        results.push({
          course: "redis",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          ...ev,
        });
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        results.push({
          course: "redis",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: false,
          actual: message,
          expected: test.expected,
        });
      }
    }
  }

  return results;
}

// Run directly if invoked as main
const isMain = import.meta.path === Bun.main;
if (isMain) {
  const results = runRedisTests();
  const passed = results.filter((r) => r.passed);
  const failed = results.filter((r) => !r.passed);

  for (const r of results) {
    const icon = r.passed ? "✓" : "✗";
    console.log(`  ${icon} [${r.course}] ${r.lessonTitle} - ${r.testName}`);
  }

  if (failed.length > 0) {
    console.log("\n--- Failures ---\n");
    for (const r of failed) {
      console.log(`  [${r.course}] ${r.lessonTitle} - ${r.testName}`);
      console.log(`    expected: ${JSON.stringify(r.expected)}`);
      console.log(`    actual:   ${JSON.stringify(r.actual)}`);
      console.log();
    }
  }

  console.log(`\n${passed.length}/${results.length} tests passed`);
  if (failed.length > 0) process.exit(1);
}
