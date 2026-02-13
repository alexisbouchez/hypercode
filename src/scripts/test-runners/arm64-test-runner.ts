import { arm64Lessons } from "@/lib/lessons/arm64/index";
import { runArm64Code } from "@/lib/arm64-runner";
import type { LessonTestResult } from "./types";

export function runArm64Tests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of arm64Lessons) {
    for (const test of lesson.tests) {
      const codeToRun = test.code
        ? test.code.replace("{{FUNC}}", lesson.solution)
        : lesson.solution;

      const result = runArm64Code(codeToRun);

      const hasError = result.error !== "";
      const actual = hasError ? result.error : result.stdout;
      const passed = !hasError && result.stdout === test.expected;

      results.push({
        course: "arm64",
        lessonId: lesson.id,
        lessonTitle: lesson.title,
        testName: test.name,
        passed,
        actual,
        expected: test.expected,
      });
    }
  }

  return results;
}
