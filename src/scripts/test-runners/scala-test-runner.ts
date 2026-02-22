import { transpileScala, extractScalaDeclarations } from "@/lib/scala-runner";
import { lessons as scalaLessons } from "@/lib/lessons/scala";
import type { LessonTestResult } from "./types";

export function runScalaTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of scalaLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const decls = extractScalaDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", decls);
      }

      try {
        const js = transpileScala(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "scala",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual === test.expected,
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "scala",
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
