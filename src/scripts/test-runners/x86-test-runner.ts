import { x86Lessons } from "@/lib/lessons/x86/index";
import { runX86Code } from "@/lib/x86-runner";
import type { LessonTestResult } from "./types";

export function runX86Tests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of x86Lessons) {
    for (const test of lesson.tests) {
      const codeToRun = test.code
        ? test.code.replace("{{FUNC}}", lesson.solution)
        : lesson.solution;

      const result = runX86Code(codeToRun);

      const hasError = result.error !== "";
      const actual = hasError ? result.error : result.stdout;
      const passed = !hasError && result.stdout.trim() === test.expected.trim();

      results.push({
        course: "x86",
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
