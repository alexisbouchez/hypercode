import { transpilePhp, extractPhpDeclarations } from "@/lib/php-runner";
import { phpLessons } from "@/lib/lessons/php";
import type { LessonTestResult } from "./types";

export function runPhpTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of phpLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const decls = extractPhpDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", decls);
      }

      try {
        const js = transpilePhp(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "php",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual.trim() === test.expected.trim(),
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "php",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: false,
          actual: String(err),
          expected: test.expected,
        });
      }
    }
  }

  return results;
}
