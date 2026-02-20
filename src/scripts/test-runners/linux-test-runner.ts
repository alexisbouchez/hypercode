import { runShell } from "@/lib/linux-shell";
import { linuxLessons } from "@/lib/lessons/linux/index";
import type { LessonTestResult } from "./types";

export function runLinuxTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of linuxLessons) {
    for (const test of lesson.tests) {
      const codeToRun = test.code
        ? test.code.replace("{{FUNC}}", lesson.solution)
        : lesson.solution;

      const result = runShell(codeToRun);

      const hasError = result.error !== "";
      const actual = hasError ? result.error : result.stdout;
      const passed = !hasError && result.stdout === test.expected;

      results.push({
        course: "linux",
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
