import { transpileKotlin, extractKotlinFunctions } from "@/lib/kotlin-runner";
import { kotlinLessons } from "@/lib/lessons/kotlin";
import type { LessonTestResult } from "./types";

export function runKotlinTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of kotlinLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const fns = extractKotlinFunctions(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", fns);
      }

      try {
        const js = transpileKotlin(codeToRun);
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "kotlin",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual === test.expected,
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "kotlin",
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
