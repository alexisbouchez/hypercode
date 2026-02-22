import { transpileLean, extractLeanDefs } from "@/lib/lean-runner";
import { leanLessons } from "@/lib/lessons/lean";
import type { LessonTestResult } from "./types";

export function runLeanTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of leanLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const defs = extractLeanDefs(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", defs);
      }

      try {
        const js = transpileLean(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "lean",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual === test.expected,
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "lean",
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
